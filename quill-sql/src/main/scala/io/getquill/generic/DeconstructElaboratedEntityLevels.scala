package io.getquill.generic


import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import io.getquill.parser.Lifter
import io.getquill.quat.Quat
import io.getquill.ast.{Map => AMap, _}
import io.getquill.generic.ElaborateStructure.Term
import io.getquill.metaprog.Extractors

object DeconstructElaboratedEntityLevels {
  def apply[ProductCls: Type](elaboration: Term)(using qctx: Quotes) =
    new DeconstructElaboratedEntityLevels(using qctx).apply[ProductCls](elaboration)
}

// TODO Explain this is a specific elaborator used for Case Class Lifts
private[getquill] class DeconstructElaboratedEntityLevels(using val qctx: Quotes) extends Extractors {
  import qctx.reflect._
  import io.getquill.generic.ElaborateStructure.Term

  private[getquill] def flattenOptions(expr: Expr[_]): Expr[_] = {
    expr.asTerm.tpe.asType match {
      case '[Option[Option[t]]] => 
        //println(s"~~~~~~~~~~~~~ Option For ${Printer.TreeShortCode.show(expr.asTerm)} ~~~~~~~~~~~~~")
        flattenOptions('{ ${expr.asExprOf[Option[Option[t]]]}.flatten[t] })
      case _ =>
        //println(s"~~~~~~~~~~~~~ Non-Option For ${Printer.TreeShortCode.show(expr.asTerm)} ~~~~~~~~~~~~~")
        expr
    }    
  }

  def apply[ProductCls: Type](elaboration: Term): List[(Expr[ProductCls => _])] = {
    recurseNest[ProductCls](elaboration)
  }

  // TODO Need to include flattenOptions
  // Given Person(name: String, age: Int)
  // Type TypeRepr[Person] and then List(Expr[Person => Person.name], Expr[Person => Person.age])
  def recurseNest[Cls: Type](node: Term): List[Expr[Cls => _]] = {
    // For example (given Person(name: String, age: Int)):
    // (Term(Person.name), Person => Person.name, String)
    // Or for nested entities (given Person(name: Name, age: Int))
    // (Term(Person.name), Person => Name, Name)
    println(s">>>>> Going through term: ${node}")
    elaborateObjectOneLevel[Cls](node).flatMap { (fieldTerm, fieldGetter, returnTpe) =>
      returnTpe.asType match
        case '[tt] => // e.g. where tt is Name
          val output = 
            recurseNest[tt](fieldTerm).map { nextField =>
              '{ ${fieldGetter.asExprOf[Cls => tt]}.andThen(ttValue => $nextField(ttValue)) }
            }
          println(s"====== Nested recurse yield: ${output}")
          output
    }
  }

  // Note: Not sure if always appending name + childName is right to do. When looking
  // up fields by name with sub-sub Embedded things going to need to look into that
  private[getquill] def elaborateObjectOneLevel[Cls: Type](node: Term): List[(Term, Expr[Cls => _], TypeRepr)] = {
    val clsType = TypeRepr.of[Cls]
    node match {
      // If leaf node, don't need to do anything since high levels have already returned this field
      case term @ Term(name, _, Nil, _) =>
        List()

      // Product node not inside an option
      // T( a, [T(b), T(c)] ) => [ a.b, a.c ] 
      // (done?)         => [ P(a, b), P(a, c) ] 
      // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
      // where T is Term and P is Property (in Ast) and [] is a list
      case (Term(name, _, childProps, false)) =>
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        val output = 
          childProps.map { childTerm =>
              (
                childTerm, 
                '{ (field: Cls) => ${ ('field `.` (childTerm.name)) }  }, // for Person, Person.name
                {
                  // or classSymbol.get?
                  val memberSymbol = clsType.widen.typeSymbol.memberField(childTerm.name)  // for Person, Person.name.type
                  clsType.memberType(memberSymbol)
                }
              )
          }
        println(s"Yielding: ${output.map(_._3).map(io.getquill.util.Format.TypeRepr(_))}")
        output

      // Production node inside an Option
      // T-Opt( a, [T(b), T(c)] ) => 
      // [ a.map(v => v.b), a.map(v => v.c) ] 
      // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
      // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]

      case Term(name, _, childProps, true) if TypeRepr.of[Cls] <:< TypeRepr.of[Option[Any]] =>
        // def innerType[IT: Type]: Type[_] =
        //   Type.of[IT] match
        //     case '[Option[t]] => Type.of[t]
        //     case _ => tpe

        // val innerType = innerType[outerT]

        
        // TODO For coproducts need to check that the childName method actually exists on the type and
        // exclude it if it does not
        childProps.map { 
          childTerm => 
            // In order to be able to flatten optionals in the flattenOptionals later, we need ot make
            // sure that the method-type in the .map function below is 100% correct. That means we
            // need to lookup what the type of the field of this particular member should actually be.
            val tpe = 
              Type.of[Cls] match
                case '[Option[t]] => TypeRepr.of[t]
            //println(s"Get member '${childTerm.name}' of ${Printer.TypeReprShortCode.show(tpe)}")
            val memField = tpe.classSymbol.get.memberField(childTerm.name)
            val memeType = tpe.memberType(memField)
            //println(s"MemField of ${childTerm.name} is ${memField}: ${Printer.TypeReprShortCode.show(memeType)}")

            Type.of[Cls] match
              case '[Option[t]] =>
                memeType.asType match
                  // If the nested field is itself optional, need to account for immediate flattening
                  case '[Option[mt]] =>
                    val expr = '{ (optField: Option[t]) => optField.flatMap[mt](prop => ${('prop `.` (childTerm.name)).asExprOf[Option[mt]]}) }
                    (
                      childTerm, 
                      expr.asInstanceOf[Expr[Cls => _]],
                      memeType
                    )
                  case '[mt] =>
                    val expr = '{ (optField: Option[t]) => optField.map[mt](prop => ${('prop `.` (childTerm.name)).asExprOf[mt]}) }
                    (
                      childTerm, 
                      expr.asInstanceOf[Expr[Cls => _]],
                      memeType
                    )
        }

      case _ =>
          report.throwError(s"Illegal state during reducing expression term: '${node}' and type: '${io.getquill.util.Format.TypeRepr(clsType)}'")
    }
  }

}