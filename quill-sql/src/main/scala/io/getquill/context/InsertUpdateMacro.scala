package io.getquill.context

import scala.quoted._
import io.getquill.norm.BetaReduction
import io.getquill.util.Load
import io.getquill.parser.ParserFactory
import io.getquill.generic.ElaborateStructure
import io.getquill.ast.{Ident => AIdent, Insert => AInsert, Update => AUpdate, _}
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.metaprog.Extractors._
import io.getquill.Quoted
import io.getquill.EntityQuery
import io.getquill.QuotationVase
import io.getquill.InsertMeta
import io.getquill.UpdateMeta
import io.getquill.quat.QuatMaking
import io.getquill.quat.QuatMakingBase
import io.getquill.quat.Quat
import io.getquill.metaprog.PlanterExpr
import io.getquill.Planter
import io.getquill.Insert
import io.getquill.Update
import io.getquill.util.Format
import io.getquill.generic.ElaborationSide
import io.getquill.metaprog.SummonParser
import io.getquill.metaprog.SummonTranspileConfig
import _root_.io.getquill.ActionReturning
import io.getquill.parser.engine.History
import io.getquill.norm.TranspileConfig
import java.util.UUID
import io.getquill.metaprog.Extractors

/**
 * TODO Right now this is just insert but we can easily extend to update and delete
 *
 * The function call that regularly drives query insertion is
 * {code}
 * query[T].insert(_.field1 -> value, _.field2 -> value, etc...)
 * {code}
 * Let's call this the field-insertion api.
 *
 * This macro essentially takes an insert of the form `query[T].insert(T(...))` and converts into the former form.
 *
 * Once we've parsed an insert e.g. `query[Person]insertValue(Person("Joe", "Bloggs"))` we then need to synthesize
 * the insertions that this would represent e.g. `query[Person].insert(_.firstName -> "Joe", _.lastName -> "Bloggs")`
 *
 * Note: that in practice what users will do most often is query[person].insertValue(lift(p)) where p is a runtime value
 * this will in turn be expanded output to query[Person].insertValue(_.name -> lift(p.name), _.age -> lift(p.age))
 * The AST that gets parsed from this woll roughly be Insert(EntityQuery(Person), List(Assignment(Id(p), Prop(Id(p), name), ScalarLift(UUID_A)), Assignemnt(Id(p), Prop(Id(p), age), ScalarLift(UUID_B))))
 * The corresponding eager lifts to UUID_A and UUID_B will be returned as part of the Quoted container. That will look roughly like Quoted(ast: Insert(...above...), lifts: EagerLift(UUID_A, p.name), EagerLift(UUID_B, p.age))
 *
 * Each function of field-insertion API basically takes the form
 * {code} (v) => vAssignmentProperty -> assignmentValue (on the AST) {code}
 *
 * Let's take a look at a slighly more complex example
 * Given:
 * {code}
 *  case class Person(name: String, age: Option[Age]); Age(value: Int)
 *  quote { query[Person].insert(Person("Joe", Age(345))) }
 * {code}
 *
 * This expands out into a series of statements which will be parsed to AST assignments
 * This: `(v: Person) => v.name -> (v:Person).name`
 * Will be parsed into this:
 * {code} Assignment(Id(v), Prop(Id(v), name), Constant("Joe")) {code}
 *
 * This: `(v: Person) => v.age.map(v => v.value) -> Option(v:Age).map(v => v.value)`
 * Will be parsed into this:
 * {code}
 *   Assignment(Id(v),
 *     OptionTableMap(Prop(Id(v), age), Id(v), Prop(Id(v), value))
 *     OptionTableMap(OptionApply(CaseClass(value=345)), Id(v), Prop(Id(v), value))
 *   )
 * {code}
 *
 * The end result of this synthesis is a series of assignments for an insert for the given entity.
 *
 * Another possiblity is that the entity is lifted:
 * {code}
 *  case class Person(name: String, age: Option[Age]); Age(value: Int)
 *  quote { query[Person].insertValue(lift(Person("Joe", Age(345)))) }
 * {code}
 * TODO Finish doc
 *
 * Note that as a result of the way this is implemented, if either the InsertMeta or the SchemaMeta is not
 * inline, the entire resulting query will not be inline since they both will be summoned and used in the
 * resulting expressions. It might be useful to introduce a configuration parameter to ignore non-inline InsertMetas
 * or non-inline SchemaMetas. Or maybe this could even be an annotation.
 */
object InsertUpdateMacro {
  // Using this ident since it is replaced in cleanIdent so user will not create something conflicting with it
  private[getquill] val VIdent = AIdent("_$V", Quat.Generic)

  private[getquill] def getQuotation[T](meta: InsertMeta[T] | UpdateMeta[T]) =
    meta match {
      case v: InsertMeta[T] => v.entity
      case v: UpdateMeta[T] => v.entity
    }

  object DynamicUtil {
    def retrieveAssignmentTuple(quoted: Quoted[_]): Set[Ast] =
      quoted.ast match {
        case Tuple(values) if (values.forall(_.isInstanceOf[Property])) => values.toSet
        case other                                                      => throw new IllegalArgumentException(s"Invalid values in InsertMeta: ${other}. An InsertMeta AST must be a tuple of Property elements.")
      }
  }

  // Summon state of a schemaMeta (i.e. whether an implicit one could be summoned and whether it is static (i.e. can produce a compile-time query or dynamic))
  enum EntitySummonState[+T] {
    case Static(value: T, lifts: List[Expr[Planter[?, ?, ?]]]) extends EntitySummonState[T]
    case Dynamic(uid: String, quotation: Expr[Quoted[Any]]) extends EntitySummonState[Nothing]
    def print(using Quotes): String =
      this match {
        case Static(value, lifts) =>
          s"EntitySummonState.Static($value, ${lifts.map(Format.Expr(_))})"
        case Dynamic(uid, quotation) =>
          s"EntitySummonState.Dynamic($uid, ${Format.Expr(quotation)})"
      }
  }

  // Summon state of a updateMeta/insertMeta that indicates which columns to ignore (i.e. whether an implicit one could be summoned and whether it is static (i.e. can produce a compile-time query or dynamic))
  enum IgnoresSummonState[+T] {
    case Static(value: T) extends IgnoresSummonState[T]
    case Dynamic(quotation: Expr[Quoted[Any]]) extends IgnoresSummonState[Nothing]
  }

  /**
   * Perform the pipeline of creating an insert statement. The 'insertee' is the case class on which the SQL insert
   * statement is based. The schema is based on the EntityQuery which could potentially be an unquoted QuerySchema.
   */
  class Pipeline[T: Type, A[T] <: Insert[T] | Update[T]: Type](isStatic: Boolean)(using Quotes) extends QuatMaking with QuatMakingBase {
    import quotes.reflect._
    import io.getquill.util.Messages.qprint
    given TranspileConfig = SummonTranspileConfig()
    val parser = SummonParser().assemble

    case class InserteeSchema(schemaRaw: Expr[EntityQuery[T]]) {
      private def plainEntity: Entity = {
        val entityName = TypeRepr.of[T].classSymbol.get.name
        Entity(entityName, List(), InferQuat.of[T].probit)
      }

      def summon: EntitySummonState[Ast] = {
        val schema = schemaRaw.asTerm.underlyingArgument.asExprOf[EntityQuery[T]]
        UntypeExpr(schema) match {
          // Case 1: query[Person].insert(...)
          // the schemaRaw part is {query[Person]} which is a plain entity query (as returned from QueryMacro)
          case '{ EntityQuery[t] } =>
            EntitySummonState.Static(plainEntity, Nil)
          // Case 2: querySchema[Person](...).insert(...)
          // there are query schemas involved i.e. the {querySchema[Person]} part is a QuotationLotExpr.Unquoted that has been spliced in
          // also if there is an implicit/given schemaMeta this case will be hit (because if there is a schemaMeta,
          // the query macro would have spliced it into the code already).
          case QuotationLotExpr.Unquoted(unquotation) =>
            unquotation match {
              // The {querySchema[Person]} part is static (i.e. fully known at compile-time)
              // (also note that if it's a filter with a pre-existing lift unlift(query[Person]).filter(p => lift("Joe")).insertValue(...)
              // this case will also happen and there can be one or more lifts i.e. lift("Joe") coming from the filter clause)
              case Uprootable(_, ast, lifts) =>
                val unliftedAst = Unlifter(ast)
                // This should be some tree containing an entity (it could even be an infix containing an entity).
                // (note that we want to replant the lifts because they do not need to be extracted here, just put back into the resulting quotation of the insert/updateValue method below)
                EntitySummonState.Static(unliftedAst, lifts.map(_.plant))
              // The {querySchema[Person]} is dynamic (i.e. not fully known at compile-time)
              case pl @ Pluckable(uid, quotation, _) =>
                val variableGuess =
                  pl.expr match {
                    case a `.` b => s"(Perhaps it is `${Format.Expr(a)}`?) "
                    case _       => ""
                  }
                if (isStatic)
                  report.warning(
                    s"The non-inlined expression `${Format.Expr(pl.expr)}:${Format.TypeRepr(pl.expr.asTerm.tpe.widen)}` (on which the query depends) is forcing the query to become dynamic. Try to change its variable ${variableGuess}to inline."
                  )
                EntitySummonState.Dynamic(uid, quotation)
              case _ =>
                report.throwError(s"Quotation Lot of Insert/UpdateMeta must be either pluckable or uprootable from: '${unquotation}'")
            }

          // Case where it's not just an EntityQuery that is in the front of the update/insertValue e.g. a filter
          //   quote { query[Person].filter(...).update/insertValue(...) }
          // Also possibly the dynamic case:
          //   val v = quote { query[Person] }
          //   quote { v.filter(...).update/insertValue(...) }
          //
          // Note that the `filter` clause can have one or more lifts e.g
          //   quote { query[Person].filter(...lift(runtimeValue)...).update/insertValue(...) }
          // so these lifts need to be extracted.
          case scheme @ '{ ($q: EntityQuery[t]) } =>
            val (ast, _) = parser(q)
            val (rawLifts, runtimeLifts) = ExtractLifts(q)
            if (!runtimeLifts.isEmpty) {
              // In this particular case:
              //   val v = quote { query[Person] }
              //   quote { v.filter(u=>...).update/insertValue(...) }
              // Our scala-tree will look like this:
              //   (Unquote[EntityQuery[Person]](v, uid:111).unquote).filter(u => ...)
              // So the AST is:
              //   Filter(QuoteTag(uid:111), u, ...)
              // So we need to synthesize a scala-tree that looks like this:
              //   Quoted( Filter(QuoteTag(uid:111), u, ...), Nil, QuotationVase(uid:111, $v:query[Person]) )
              // Note that the fact that $v is query[Person] is only known at runtime.
              //
              // In the case that there are lifts in the filter, it will look like this:
              //   Query =>      |quote { v.filter(u=>...lift(...)...).update/insertValue(...) }
              //                 |val v = quote { query[Person] }
              //   Scala Tree => |(Unquote[EntityQuery[Person]](v, uid:111).unquote).filter(u => ...lifts(uid:222,...)...)
              //   Quill Ast  => |Filter(QuoteTag(uid:111), u, ...ScalarTag(uid:222)...)
              //   We Create  => |Quoted( Filter(QuoteTag(uid:111), u, ...ScalarTag(uid:222)...), EagerLift(uid:222,...), QuotationVase(uid:111, $v:query[Person]) )
              val uid = UUID.randomUUID().toString()
              if (isStatic)
                report.warning(s"A non-inlined expression (that defines a query for ${Format.TypeRepr(schemaRaw.asTerm.tpe.widen)}) is forcing the query to become dynamic. Try to change its variable to inline in order to fix the issue.")
              EntitySummonState.Dynamic(uid, '{ Quoted(${ Lifter(ast) }, ${ Expr.ofList(rawLifts) }, ${ Expr.ofList(runtimeLifts) }) })
            }
            else
              EntitySummonState.Static(ast, rawLifts)

          case _ =>
            report.throwError(s"Cannot process illegal insert meta: ${Format.Expr(schema)}")
        }
      }
    } // end InserteeSchema

    enum MacroType {
      case Insert
      case Update
    }
    object MacroType {
      def asString = ofThis().toString
      def ofThis() =
        if (TypeRepr.of[A] <:< TypeRepr.of[Insert])
          MacroType.Insert
        else if (TypeRepr.of[A] <:< TypeRepr.of[Update])
          MacroType.Update
        else
          report.throwError(s"Invalid macro action type ${io.getquill.util.Format.TypeOf[A[Any]]} must be either Insert or Update")
      def summonMetaOfThis() =
        ofThis() match {
          case MacroType.Insert => Expr.summon[InsertMeta[T]]
          case MacroType.Update => Expr.summon[UpdateMeta[T]]
        }
    }

    private object UprootableActionMeta {
      def unapply(actionMeta: Expr[InsertMeta[T]] | Expr[UpdateMeta[T]]) =
        QuotationLotExpr(actionMeta.asTerm.underlyingArgument.asExpr) match {
          case Uprootable.Ast(ast) => Some(ast)
          case _                   => None
        }
    }

    object IgnoredColumns {
      def summon: IgnoresSummonState[Set[Ast]] =
        // If someone has defined a: given meta: InsertMeta[Person] = insertMeta[Person](_.id) or UpdateMeta[Person] = updateMeta[Person](_.id)
        MacroType.summonMetaOfThis() match {
          case Some(actionMeta) =>
            actionMeta match {
              case UprootableActionMeta(ast) =>
                // if the meta is inline i.e. 'inline given meta: InsertMeta[Person] = ...' (or UpdateMeta[Person])
                Unlifter(ast) match {
                  case Tuple(values) if (values.forall(_.isInstanceOf[Property])) =>
                    IgnoresSummonState.Static(values.toSet)
                  case other =>
                    report.throwError(s"Invalid values in ${Format.TypeRepr(actionMeta.asTerm.tpe)}: ${other}. An ${Format.TypeRepr(actionMeta.asTerm.tpe)} AST must be a tuple of Property elements.")
                }
              // if the meta is not inline
              case meta: Expr[InsertMeta[T] | UpdateMeta[T]] =>
                if (isStatic) report.warning(s"The non-inlined variable `${Format.Expr(actionMeta)}:${Format.TypeRepr(actionMeta.asTerm.tpe.widen)}` will force the query to be dynamic. Try to change it to inline in order to fix the issue.")
                IgnoresSummonState.Dynamic('{ InsertUpdateMacro.getQuotation($meta) })
              case null =>
                report.throwError(
                  s"The ${MacroType.asString}Meta ${io.getquill.util.Format.Expr(actionMeta)} is null. This is invalid."
                )
            }
          // TODO Configuration to ignore dynamic insert metas?
          // println("WARNING: Only inline insert-metas are supported for insertions so far. Falling back to a insertion of all fields.")
          case None =>
            IgnoresSummonState.Static(Set.empty)
        }
    }

    /**
     * Inserted object
     * can either be static: query[Person]insertValue(Person("Joe", "Bloggs"))
     * or it can be lifted:  query[Person].insertValue(lift(Person("Joe", "Bloggs")))
     *
     * In the later case, it will become:
     * {{
     *   //Assuming x := Person("Joe", "Bloggs")
     *   CaseClassLift(
     *    Quoted(ast: CaseClass(name -> lift(idA)), ...), lifts: List(EagerLift(x.name, idA), ...))
     *   )
     * }}
     *
     * For batch queries liftQuery(people).foreach(p => query[Person].insertValue(p))
     * it will be just the ast Ident("p")
     */
    def parseInsertee(insertee: Expr[Any]): CaseClass | AIdent = {
      insertee match {
        // The case: query[Person].insertValue(lift(Person("Joe", "Bloggs")))
        case QuotationLotExpr(exprType) =>
          exprType match {
            // If clause is uprootable, pull it out. Note that any lifts inside don't need to be extracted here
            // since they will be extracted later in ExtractLifts
            case Uprootable.Ast(astExpr) =>
              val ast = Unlifter(astExpr)
              if (!ast.isInstanceOf[CaseClass])
                report.throwError(s"The lifted insertion element needs to be parsed as a Ast CaseClass but it is: ${ast}")
              ast.asInstanceOf[CaseClass]
            case _ =>
              report.throwError(s"Cannot uproot lifted element. A lifted Insert element e.g. query[T].insertValue(lift(element)) must be lifted directly inside the lift clause. The elment was:\n${insertee.show}")
          }
        // Otherwise the inserted element (i.e. the insertee) is static and should be parsed as an ordinary case class
        // i.e. the case query[Person]insertValue(Person("Joe", "Bloggs")) (or the batch case)
        case _ =>
          parseStaticInsertee(insertee)
      }
    }

    /**
     * Parse the input to of query[Person]insertValue(Person("Joe", "Bloggs")) into CaseClass(firstName="Joe",lastName="Bloggs")
     */
    def parseStaticInsertee(insertee: Expr[_]): CaseClass | AIdent = {
      val (rawAst, _) = parser(insertee)
      val ast = BetaReduction(rawAst)
      ast match {
        case cc: CaseClass => cc
        case id: AIdent    => id
        case _             => report.throwError(s"Parsed Insert Macro AST is not a Case Class: ${qprint(ast).plainText} (or a batch-query Ident)")
      }
    }

    /**
     * Actually the same as deduceAssignmentsFromCaseClass, but I decided to write
     * a separate function and comment it extensively since the logic is not simple to
     * extrapolate.
     * This function creates a series of assignments
     * of a elaborated product. However, each assignment just assigns to the identifier
     * which will be plugged in (i.e. BetaReduced) once the Ident is actually substituted.
     * E.g. if we have something like this: `val ip = quote { (p: Person) => query[Person].insertValue(p) }`
     * and then later: `run(ip(lift(Person("Joe",123))))` then the assignments list is just based
     * on the `p` identifier of the `ip` quoted function i.e:
     * `(v:Person) => v.firstName -> p.firstName` this is achived by doing
     * BetaReduce(v.firstName, v -> p). Later on when `ip(lift(Person("Joe",123)))`
     * happens the `CaseClass(firstName -> lift(...), age -> lift(...))` comes in and
     * all the right values are plugged in correctly.
     */
    def deduceAssignmentsFromIdent(insertee: AIdent) = {
      val expansionList = ElaborateStructure.ofProductType[T](VIdent.name, ElaborationSide.Encoding) // Elaboration side is Encoding since this is for an entity being inserted
      def mapping(path: Ast) = {
        val reduction = BetaReduction(path, VIdent -> insertee)
        Assignment(VIdent, path, reduction)
      }

      val assignmentsAst = expansionList.map(exp => mapping(exp))
      assignmentsAst
    }

    def deduceAssignmentsFromCaseClass(insertee: CaseClass) = {
      // Expand into a AST
      // T:Person(name:Str, age:Option[Age]) Age(value: Int) -> Ast: List(v.name, v.age.map(v => v.value))
      val expansionList = ElaborateStructure.ofProductType[T](VIdent.name, ElaborationSide.Encoding) // Elaboration side is Encoding since this is for an entity being inserted

      // Now synthesize (v) => vAssignmentProperty -> assignmentValue
      // e.g. (v:Person) => v.firstName -> "Joe"
      // TODO, Ast should always be a case class (maybe a tuple?) should verify that
      def mapping(path: Ast) = {
        val reduction = BetaReduction(path, VIdent -> insertee)
        Assignment(VIdent, path, reduction)
      }

      val assignmentsAst = expansionList.map(exp => mapping(exp))
      assignmentsAst
    }

    /** Is the assignment list know at compile time or only runtime? */
    enum AssignmentList {
      def splice: Expr[List[io.getquill.ast.Assignment]] =
        this match {
          case Static(list) =>
            Expr.ofList(list.map(asi => Lifter.NotSerializingAst.assignment(asi)))
          case Dynamic(list) =>
            list
        }
      // If it is known at compile-time we can carry the actual instance list
      case Static(list: List[io.getquill.ast.Assignment]) extends AssignmentList
      // If it is only known at runtime, we have to carry around the spliceable expression
      case Dynamic(list: Expr[List[io.getquill.ast.Assignment]])
    }

    /**
     * Get assignments from an entity and then either exclude or include them
     * either statically or dynamically.
     */
    def processAssignmentsAndExclusions(assignmentsOfEntity: List[io.getquill.ast.Assignment]): AssignmentList =
      IgnoredColumns.summon match {
        // If we have assignment-exclusions during compile time
        case IgnoresSummonState.Static(exclusions) =>
          // process which assignments to exclude and take them out
          val remainingAssignments = assignmentsOfEntity.filterNot(asi => exclusions.contains(asi.property))
          // Then just return the remaining assignments
          AssignmentList.Static(remainingAssignments)
        // If we have assignment-exclusions that can only be accessed during runtime
        case IgnoresSummonState.Dynamic(quotation) =>
          // Pull out the exclusions from the quotation
          val exclusions = '{ DynamicUtil.retrieveAssignmentTuple($quotation) }
          // Lift ALL the assignments of the entity
          val allAssignmentsLifted = Expr.ofList(assignmentsOfEntity.map(ast => Lifter.NotSerializingAst.assignment(ast)))
          // Create a statement that represents the filtered assignments during runtime
          val liftedFilteredAssignments = '{ $allAssignmentsLifted.filterNot(asi => $exclusions.contains(asi.property)) }
          // ... and return the filtered assignments
          AssignmentList.Dynamic(liftedFilteredAssignments)
      }

    /**
     * Note that the only reason Parser is needed here is to pass it into parseInsertee.
     * The batch pipeline driven by createFromPremade currently doesn't need it.
     */
    def apply(schemaRaw: Expr[EntityQuery[T]], inserteeRaw: Expr[T]) = {
      val insertee = inserteeRaw.asTerm.underlyingArgument.asExpr
      val assignmentOfEntity =
        parseInsertee(insertee) match {
          // if it is a CaseClass we either have a static thing e.g. query[Person].insert(Person("Joe", 123))
          // or we have a lifted thing e.g. query[Person].insertValue(lift(Person("Joe", 123)))
          // so we just process it based on what kind of pattern we encounter
          case astCaseClass: CaseClass => deduceAssignmentsFromCaseClass(astCaseClass)
          // if it is a Ident, then we know we have a batch query i.e. liftQuery(people).foreach(p => query[Person].insertValue(p))
          // we want to re-syntheize this as a lifted thing i.e. liftQuery(people).foreach(p => query[Person].insertValue(lift(p)))
          // and then reprocess the contents.
          // We don't want to do that here thought because we don't have the PrepareRow
          // so we can't lift content here into planters. Instead this is done in the QueryExecutionBatch pipeline
          case astIdent: AIdent => deduceAssignmentsFromIdent(astIdent)
        }

      // Insertion could have lifts and quotes inside, need to extract those.
      // E.g. it can be 'query[Person].insertValue(lift(Person("Joe",123)))'' which becomes Quoted(CaseClass(name -> lift(x), age -> lift(y), List(EagerLift("Joe", x), EagerLift(123, y)), Nil).
      // (In some cases, maybe even the runtimeQuotes position could contain things)
      // However, the insertee itself must always be available statically (i.e. it must be a Uprootable Quotation)
      val (lifts, pluckedUnquotes) = ExtractLifts(inserteeRaw)

      val quotation =
        createQuotation(
          InserteeSchema(schemaRaw.asTerm.underlyingArgument.asExprOf[EntityQuery[T]]).summon,
          assignmentOfEntity,
          lifts,
          pluckedUnquotes
        )
      UnquoteMacro(quotation)
    }

    /**
     * Create a static or dynamic quotation based on the state. Wrap the expr using some additional functions if we need to.
     * This is used for the createFromPremade if we need to wrap it into insertReturning which is used for batch-returning query execution.
     */
    def createQuotation(summonState: EntitySummonState[Ast], assignmentOfEntity: List[Assignment], lifts: List[Expr[Planter[?, ?, ?]]], pluckedUnquotes: List[Expr[QuotationVase]]) = {
      // Processed Assignments AST plus any lifts that may have come from the assignments AST themsevles.
      // That is usually the case when
      val assignmentList = processAssignmentsAndExclusions(assignmentOfEntity)

      // TODO where if there is a schemaMeta? Need to use that to create the entity
      (summonState, assignmentList) match {
        // If we can get a static entity back
        case (EntitySummonState.Static(entity, previousLifts), AssignmentList.Static(assignmentsAst)) =>
          // Lift it into an `Insert` ast, put that into a `quotation`, then return that `quotation.unquote` i.e. ready to splice into the quotation from which this `.insert` macro has been called
          val action = MacroType.ofThis() match {
            case MacroType.Insert =>
              AInsert(entity, assignmentsAst)
            case MacroType.Update =>
              AUpdate(entity, assignmentsAst)
          }

          // Now create the quote and lift the action. This is more efficient then the alternative because the whole action AST can be serialized
          val quotation = '{ Quoted[A[T]](${ Lifter(action) }, ${ Expr.ofList(previousLifts ++ lifts) }, ${ Expr.ofList(pluckedUnquotes) }) }
          // Unquote the quotation and return
          quotation

        case (EntitySummonState.Static(entity, previousLifts), assignmentsList) =>
          // Need to create a ScalarTag representing a splicing of the entity (then going to add the actual thing into a QuotationVase and add to the pluckedUnquotes)
          val actionQuote = MacroType.ofThis() match {
            case MacroType.Insert =>
              // If the assignments list is dynamic, its 'assignmentsList.splice' just puts in the Expr. If it is static, it will call the lifter so splice it.
              '{ Quoted[A[T]](AInsert(${ Lifter(entity) }, ${ assignmentsList.splice }), Nil, Nil) }
            case MacroType.Update =>
              '{ Quoted[A[T]](AUpdate(${ Lifter(entity) }, ${ assignmentsList.splice }), Nil, Nil) }
          }

          // create and lift the action
          val uid = UUID.randomUUID().toString()
          val runtimeQuote = '{ QuotationVase($actionQuote, ${ Expr(uid) }) }
          val quotation = '{ Quoted[A[T]](QuotationTag(${ Expr(uid) }), ${ Expr.ofList(previousLifts ++ lifts) }, $runtimeQuote +: ${ Expr.ofList(pluckedUnquotes) }) }
          quotation

        // If we get a dynamic entity back we need to splice things as an Expr even if the assignmentsList is know at compile time
        // e.g. entityQuotation is 'querySchema[Person](...)' which is not inline
        case (EntitySummonState.Dynamic(uid, entityQuotation), assignmentsList) =>
          // Need to create a ScalarTag representing a splicing of the entity (then going to add the actual thing into a QuotationVase and add to the pluckedUnquotes)
          val action = MacroType.ofThis() match {
            case MacroType.Insert =>
              // If the assignments list is dynamic, its 'assignmentsList.splice' just puts in the Expr. If it is static, it will call the lifter so splice it.
              '{ AInsert(QuotationTag(${ Expr(uid) }), ${ assignmentsList.splice }) }
            case MacroType.Update =>
              '{ AUpdate(QuotationTag(${ Expr(uid) }), ${ assignmentsList.splice }) }
          }

          // Create the QuotationVase in which this dynamic quotation will go
          val runtimeQuote = '{ QuotationVase($entityQuotation, ${ Expr(uid) }) }
          // Then create the quotation, adding the new runtimeQuote to the list of pluckedUnquotes
          val quotation = '{ Quoted[A[T]](${ action }, ${ Expr.ofList(lifts) }, $runtimeQuote +: ${ Expr.ofList(pluckedUnquotes) }) }
          // Unquote the quotation and return
          quotation
      }
    }

  } // end Pipeline

  def static[T: Type, A[T] <: Insert[T] | Update[T]: Type](entityRaw: Expr[EntityQuery[T]], bodyRaw: Expr[T])(using Quotes): Expr[A[T]] =
    new Pipeline[T, A](true).apply(entityRaw, bodyRaw)
  def dynamic[T: Type, A[T] <: Insert[T] | Update[T]: Type](entityRaw: Expr[EntityQuery[T]], bodyRaw: Expr[T])(using Quotes): Expr[A[T]] =
    new Pipeline[T, A](false).apply(entityRaw, bodyRaw)
}
