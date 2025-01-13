package io.getquill

import io.getquill.parser.*

import scala.quoted.*
import scala.annotation.StaticAnnotation
import scala.deriving.*
import io.getquill.generic.GenericEncoder
import io.getquill.quotation.NonQuotedException

import scala.annotation.compileTimeOnly
import io.getquill.Query
import io.getquill.EntityQuery
import io.getquill.ast.{Ast, QuotationTag, ScalarTag, Transform}

import java.util.UUID

object EntityQuery {
  def apply[T] = new EntityQuery[T]() {}
}

def querySchema[T](entity: String, columns: (T => (Any, String))*): EntityQuery[T] = NonQuotedException()

sealed trait Unquoteable

trait EntityQuery[T] extends EntityQueryModel[T] with Unquoteable {
  override def withFilter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  override def filter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  override def map[R](f: T => R): EntityQuery[R] = NonQuotedException()
}

class Quoted[+T](val ast: io.getquill.ast.Ast, val lifts: List[Planter[_, _, _]], val runtimeQuotes: List[QuotationVase]) {
  // This is not a case-class because the dynamic API uses (quoted:Quoted[(foo, bar)])._1 etc... which would return quoted.ast
  // where instead we want unquote(quoted)._1 to happen instead but the implicit unquote would never happen if quoted
  // is a case class in the _1 property is available on the object.
  protected lazy val id = Quoted.QuotedId(ast, lifts, runtimeQuotes)
  override def toString = io.getquill.util.Messages.qprint(id).plainText
  override def hashCode(): Int = id.hashCode
  override def equals(other: Any): Boolean =
    other match {
      case q: Quoted[_] => q.id == this.id
      case _            => false
    }
  def copy(ast: io.getquill.ast.Ast = this.ast, lifts: List[Planter[_, _, _]] = this.lifts, runtimeQuotes: List[QuotationVase] = this.runtimeQuotes) =
    Quoted(ast, lifts, runtimeQuotes)

  def dedupeRuntimeBinds: Quoted[T] = {
    def rekeyLeafBindsUsing(ast: Ast, bindMap: Map[String, String]) =
      Transform(ast) {
        case tag: ScalarTag if (bindMap.contains(tag.uid)) => tag.copy(uid = bindMap(tag.uid))
      }

    def rekeyVasesUsing(ast: Ast, bindMap: Map[String, String]) =
      Transform(ast) {
        case tag: QuotationTag if (bindMap.contains(tag.uid)) => tag.copy(uid = bindMap(tag.uid))
      }

    def rekeyLeafBinds(quoted: Quoted[_]) = {
      val (liftIdMap, newPlanters) = quoted.lifts.map { lift =>
        val newId = UUID.randomUUID().toString
        val newLift = lift.rekey(newId)
        ((lift.uid, newId), newLift)
      }.unzip
      val newAst = rekeyLeafBindsUsing(quoted.ast, liftIdMap.toMap)
      Quoted(newAst, newPlanters, quoted.runtimeQuotes)
    }
    // need to rekey depth-first, otherwise the same uid might be rekeyed multiple times which is not the correct behavior
    // innermost binds need to be rekeyed first
    def rekeyRecurse(quoted: Quoted[_]): Quoted[_] = {
      // rekey leaf binds of the children, for inner most children runtimeQuotes shuold be empty
      val (vaseIdMap, newVases) = quoted.runtimeQuotes.map { vase =>
        val newVaseId = UUID.randomUUID().toString
        // recursively call to rekey the vase (if there are any inner dynamic quotes)
        val newQuotation = rekeyRecurse(vase.quoted)
        ((vase.uid, newVaseId), QuotationVase(newQuotation, newVaseId))
      }.unzip
      // Then go through the vases themselves (that are on this level and rekey them)
      val newAst = rekeyVasesUsing(quoted.ast, vaseIdMap.toMap)
      // finally rekey the leaf-binds of the quotation itself (this should happen on the innermost quotation first
      // since depth-first recursion is happening here)
      rekeyLeafBinds(Quoted(newAst, quoted.lifts, newVases))
    }

    rekeyRecurse(this).asInstanceOf[Quoted[T]]
  }
}
object Quoted {
  case class QuotedId(val ast: io.getquill.ast.Ast, val lifts: List[Planter[_, _, _]], val runtimeQuotes: List[QuotationVase])
  def apply[T](ast: io.getquill.ast.Ast, lifts: List[Planter[_, _, _]], runtimeQuotes: List[QuotationVase]) =
    new Quoted[T](ast, lifts, runtimeQuotes)
  def unapply[T](quoted: Quoted[T]) =
    Some((quoted.ast, quoted.lifts, quoted.runtimeQuotes))
}

// Planters contain trees that can be re-inserted into compile-time code.
// For example, a ScalarPlanter is re-inserted into the PrepareRow sequence
// Note that we cannot assume the unquote is just T since unquoted values can be
// different e.g. in EagerEntityListPlaner
sealed trait Planter[T, PrepareRow, Session] extends Unquoteable {
  def unquote: T
  def uid: String
  def rekey(newUid: String): Planter[T, PrepareRow, Session]
}

private[getquill] trait InfixValue {
  def as[T]: T
  def asCondition: Boolean
  def pure: InfixValue
  private[getquill] def generic: InfixValue
  private[getquill] def transparent: InfixValue
}

implicit class InfixInterpolator(val sc: StringContext) {
  // @compileTimeOnly(NonQuotedException.message)
  def infix(args: Any*): InfixValue = NonQuotedException()
}

implicit class SqlInfixInterpolator(val sc: StringContext) {
  // @compileTimeOnly(NonQuotedException.message)
  def sql(args: Any*): InfixValue = NonQuotedException()
}

object compat {
  implicit class QsqlInfixInterpolator(val sc: StringContext) {
    // @compileTimeOnly(NonQuotedException.message)
    def qsql(args: Any*): InfixValue = NonQuotedException()
  }
}

case class InjectableEagerPlanter[T, PrepareRow, Session](inject: _ => T, encoder: GenericEncoder[T, PrepareRow, Session], uid: String) extends Planter[T, PrepareRow, Session] {
  // This is the equivalent of InjectableEagerPlanterExpr's 'inject' method only for dynamic batch queries
  // TODO Try changing to Any => T and see if exceptions happen anywhere
  def withInject(element: Any) = EagerPlanter[T, PrepareRow, Session](inject.asInstanceOf[Any => T](element), encoder, uid)
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
  def rekey(newUid: String): InjectableEagerPlanter[T, PrepareRow, Session] = InjectableEagerPlanter(inject, encoder, newUid)
}

case class EagerListPlanter[T, PrepareRow, Session](values: List[T], encoder: GenericEncoder[T, PrepareRow, Session], uid: String) extends Planter[Query[T], PrepareRow, Session] {
  def unquote: Query[T] =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
  def rekey(newUid: String): EagerListPlanter[T, PrepareRow, Session] = EagerListPlanter(values, encoder, newUid)
}

case class EagerPlanter[T, PrepareRow, Session](value: T, encoder: GenericEncoder[T, PrepareRow, Session], uid: String) extends Planter[T, PrepareRow, Session] {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
  def rekey(newUid: String): EagerPlanter[T, PrepareRow, Session] = EagerPlanter(value, encoder, newUid)
}

case class LazyPlanter[T, PrepareRow, Session](value: T, uid: String) extends Planter[T, PrepareRow, Session] {
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
  def rekey(newUid: String): LazyPlanter[T, PrepareRow, Session] = LazyPlanter(value, newUid)
}

// Equivalent to CaseClassValueLift
case class EagerEntitiesPlanter[T, PrepareRow, Session](value: Iterable[T], uid: String, fieldGetters: List[InjectableEagerPlanter[?, PrepareRow, Session]], fieldClass: ast.CaseClass) extends Planter[Query[T], PrepareRow, Session] {
  def unquote: Query[T] =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
  def rekey(newUid: String): EagerEntitiesPlanter[T, PrepareRow, Session] = EagerEntitiesPlanter(value, newUid, fieldGetters, fieldClass)
}

// Stores runtime quotation tree. This is holder for quotations that are not inline thus can never be re-inserted into
// the ast (i.e. the ground... metaphorically speaking), therefore this holder is called Vase. The contents of the
// QuotationVase are only inserted back in during runtime.
case class QuotationVase(quoted: Quoted[Any], uid: String) extends Unquoteable

// Quotations go from a QuotationLot directly inline into the tree or put into a QuotationVase
// to be added into the runtime inlining later
// NOTE: Don't want to include quoted: Quoted[T] since the inner stored type might be different that the thing returned.
//       currently there's no use-case for that but perhaps in future.
sealed trait QuotationLot[+T](uid: String) extends Unquoteable {
  // TODO I think we should get rid of this operator. Unquote should be put on this via an implicit class which causes
  // invocation of the unquote macro?
  def unquote: T =
    throw new RuntimeException("Unquotation can only be done from a quoted block.")
}

case class Unquote[+T](quoted: Quoted[T], uid: String) extends QuotationLot[T](uid)

// TODO Does this need to be covariant? It is in current quill. Need to look up what use cases they are for covariant schemas.
case class SchemaMeta[T](val entity: Quoted[EntityQuery[T]], uid: String) extends QuotationLot[EntityQuery[T]](uid)

case class InsertMeta[T](val entity: Quoted[T], uid: String) extends QuotationLot[T](uid)

case class UpdateMeta[T](val entity: Quoted[T], uid: String) extends QuotationLot[T](uid)

// enum ActionMetaType { Insert, Update }
// or with traits?
// trait ActionMetaType; object ActionMetaType { trait Insert extends ActionMetaType; trait Update extends ActionMetaType }

// trait ActionMeta[T, MetaType]
// case class InsertMeta[T](val entity: Quoted[T], uid: String) extends ActionMeta[T, Insert] with QuotationLot[T](entity, uid)

// Then ActionMacro will take a MT (i.e. MetaType) generic argument that will control what to summon and what kind of AST
// element Ast.Insert or Ast.Update to return (also there should probably be 'Delete' meta type which does not summon a column-excluding meta)

case class QueryMeta[T, R](val entity: Quoted[Query[T] => Query[R]], uid: String, extract: R => T) extends QuotationLot[Query[T] => Query[R]](uid)

// When you query[Person].insertValue(lift(p)) that will be seen by the parser as:
//   Apply(Select(expr: EntityQuery(Person), insertValue), CaseClassLift(...))
//   because `insertValue` is a macro that resolves to the CaseClassLift(...)
// Where CaseClassLift(...) is something like:
//   CaseClassLift(Quoted(ast: CaseClass(Person)("name" -> ScalarTag(UUID_A), "age" -> ScalarTag(UUID_B)), lifts: List(EagerPlanter(UUID_A, p.name), EagerPlanter(UUID_B, p.age))))
// In situations where you have nested case classes it will look something liek this:
//   for: Person(Name(first, last), age)
//   CaseClassLift(QUoted(ast: CaseClass(Person)("name" -> CaseClass(Name)(first -> ScalaTag(UUID_A), last -> ScalarTag(UUID_B)), age -> ScalarTag(UUID_C)), List(EagerPlanter(UUID_A, p.name.first), EagerPlanter(UUID_B, p.name.last), EagerPlanter(UUID_C, p.age))))
// TODO Rename to EntityLift
// Equivalent to CaseClassValueLift
case class CaseClassLift[T](val entity: Quoted[T], uid: String) extends QuotationLot[T](uid)
