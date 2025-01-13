package io.getquill.parser

import io.getquill.ast.{Action as AAction, Delete as ADelete, Ident as AIdent, Insert as AInsert, Query as AQuery, Update as AUpdate, *}
import io.getquill.ast
import io.getquill.metaprog.PlanterExpr
import io.getquill.metaprog.QuotedExpr

import scala.quoted.*
import scala.annotation.StaticAnnotation
import scala.deriving.*
import io.getquill.Embedable

import scala.reflect.ClassTag
import io.getquill.norm.capture.AvoidAliasConflict
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.util.Format
import io.getquill.parser.ParserHelpers.*
import io.getquill.quat.QuatMaking
import io.getquill.quat.QuatMakingBase
import io.getquill.quat.Quat
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.Uprootable
import io.getquill.metaprog.Pluckable
import io.getquill.metaprog.Pointable
import io.getquill.metaprog.Extractors.*
import io.getquill.metaprog.SummonTranspileConfig
import io.getquill.*
import io.getquill.Ord
import io.getquill.Embedded
import io.getquill.metaprog.Is
import io.getquill.generic.ElaborationSide
import io.getquill.parser.engine.*
import io.getquill.context.{ExtractLifts, VerifyFreeVariables}
import io.getquill.norm.TranspileConfig
import io.getquill.ast.External.Source
import io.getquill.quat.VerifyNoBranches
import io.getquill.metaprog.SummonParser

trait ParserFactory {
  def assemble(using Quotes): ParserLibrary.ReadyParser
}

trait ParserLibrary extends ParserFactory {

  // TODO add a before everything identity parser,
  // a after everything except Inline recurse parser
  protected def quotationParser(using Quotes, TranspileConfig) = ParserChain.attempt(QuotationParser(_))
  protected def queryParser(using Quotes, TranspileConfig) = ParserChain.attempt(QueryParser(_))
  protected def infixParser(using Quotes, TranspileConfig) = ParserChain.attempt(InfixParser(_))
  protected def setOperationsParser(using Quotes, TranspileConfig) = ParserChain.attempt(SetOperationsParser(_))
  protected def queryScalarsParser(using Quotes, TranspileConfig) = ParserChain.attempt(QueryScalarsParser(_))
  protected def traversableOperationParser(using Quotes, TranspileConfig) = ParserChain.attempt(TraversableOperationParser(_))
  protected def patMatchParser(using Quotes, TranspileConfig) = ParserChain.attempt(CasePatMatchParser(_))
  protected def functionParser(using Quotes, TranspileConfig) = ParserChain.attempt(FunctionParser(_))
  protected def functionApplyParser(using Quotes, TranspileConfig) = ParserChain.attempt(FunctionApplyParser(_))
  protected def valParser(using Quotes, TranspileConfig) = ParserChain.attempt(ValParser(_))
  protected def arbitraryTupleParser(using Quotes, TranspileConfig) = ParserChain.attempt(ArbitraryTupleBlockParser(_))
  protected def blockParser(using Quotes, TranspileConfig) = ParserChain.attempt(BlockParser(_))
  protected def extrasParser(using Quotes, TranspileConfig) = ParserChain.attempt(ExtrasParser(_))
  protected def operationsParser(using Quotes, TranspileConfig) = ParserChain.attempt(OperationsParser(_))
  protected def orderingParser(using Quotes, TranspileConfig) = ParserChain.attempt(OrderingParser(_))
  protected def genericExpressionsParser(using Quotes, TranspileConfig) = ParserChain.attempt(GenericExpressionsParser(_))
  protected def actionParser(using Quotes, TranspileConfig) = ParserChain.attempt(ActionParser(_))
  protected def batchActionParser(using Quotes, TranspileConfig) = ParserChain.attempt(BatchActionParser(_))
  protected def optionParser(using Quotes, TranspileConfig) = ParserChain.attempt(OptionParser(_))
  protected def ifElseParser(using Quotes, TranspileConfig) = ParserChain.attempt(IfElseParser(_))
  protected def complexValueParser(using Quotes, TranspileConfig) = ParserChain.attempt(ComplexValueParser(_))
  protected def valueParser(using Quotes, TranspileConfig) = ParserChain.attempt(ValueParser(_))

  // def userDefined(using quotesInput: Quotes) = Series(new Glosser[Ast] {
  //   val quotes = quotesInput
  //   def apply(root: Parser[Ast]) = PartialFunction.empty[Expr[_], Ast]
  // })

  // Everything needs to be parsed from a quoted state, and sent to the subsequent parser
  def assemble(using Quotes): ParserLibrary.ReadyParser = {
    given TranspileConfig = SummonTranspileConfig()
    val assembly =
      quotationParser
        .orElse(valueParser)
        .orElse(queryParser)
        .orElse(queryScalarsParser)
        .orElse(infixParser)
        .orElse(setOperationsParser)
        .orElse(traversableOperationParser)
        .orElse(optionParser)
        .orElse(orderingParser)
        .orElse(actionParser)
        .orElse(batchActionParser)
        .orElse(functionParser) // decided to have it be it's own parser unlike Quill3
        .orElse(patMatchParser)
        .orElse(valParser)
        .orElse(arbitraryTupleParser)
        .orElse(blockParser)
        .orElse(operationsParser)
        .orElse(extrasParser)
        .orElse(ifElseParser)
        .orElse(complexValueParser) // must go before functionApplyParser since valueParser parsers '.apply on case class' and the functionApply would take that
        .orElse(functionApplyParser) // must go before genericExpressionsParser otherwise that will consume the 'apply' clauses
        .orElse(genericExpressionsParser)
        .complete
    ParserLibrary.ReadyParser(assembly)
  }

} // end ParserLibrary

object ParserLibrary extends ParserLibrary {
  class ReadyParser private[parser] (parser: Parser) {
    def apply(expr: Expr[_])(using Quotes, TranspileConfig) = {
      val lifts = LiftsAccum.Empty
      val ast = parser(expr)(using History.Root, lifts)
      ast -> lifts
    }
  }
}

class FunctionApplyParser(rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) {
  import quotes.reflect._
  import io.getquill.norm.capture.AvoidAliasConflict

  // case q"new { def apply[..t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def attempt = {
    case Unseal(Apply(Select(term, "apply"), args)) =>
      FunctionApply(rootParse(term.asExpr), args.map(arg => rootParse(arg.asExpr)))
  }
}

class FunctionParser(rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with Helpers {
  import quotes.reflect._

  import io.getquill.norm.capture.AvoidAliasConflict

  // case q"new { def apply[..t1](...$params) = $body }" =>
  //  c.fail("Anonymous classes aren't supported for function declaration anymore. Use a method with a type parameter instead. " +
  //    "For instance, replace `val q = quote { new { def apply[T](q: Query[T]) = ... } }` by `def q[T] = quote { (q: Query[T] => ... }`")

  def attempt = {
    case Unseal(RawLambdaN(params, body)) =>
      val subtree = Function(params.map((name, tpe) => cleanIdent(name, tpe)), rootParse(body.asExpr))
      // If there are actions inside the subtree, we need to do some additional sanitizations
      // of the variables so that their content will not collide with code that we have generated.

      // TODO Add back once moved to the quill subtree because AvoidAliasConflict is private[getquill]
      // if (CollectAst.byType[Action](subtree).nonEmpty)
      //  AvoidAliasConflict.sanitizeVariables(subtree, dangerousVariables)
      // else
      subtree
  }
}

class ValParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with PatternMatchingValues {
  import quotes.reflect._
  def attempt = {
    case Unseal(ValDefTerm(ast)) => ast
  }
}
/**
 * Matches `runtime.Tuples.cons(head,tail)`.
 */
object TupleCons {
  def unapply(using Quotes)(t: quotes.reflect.Term): Option[(quotes.reflect.Term, quotes.reflect.Term)] = {
    import quotes.reflect.*
    t match {
      case Apply(Select(Select(Ident("runtime"), "Tuples"), "cons"), List(head, tail)) =>
        Some((head, tail))
      case _ =>
        None
    }
  }
}

/**
 * Matches inner.asInstanceOf[T]: T
 */
object AsInstanceOf {
  def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] = {
    import quotes.reflect._
    term match {
      case TypeApply(Select(inner, "asInstanceOf"), _) => Some(inner)
      case _ => None
    }
  }
}

/**
 * Matches an inlined call to `Tuple.*:`:
 * {{{
 * {
 *   val Tuple_this: scala.Tuple$package.EmptyTuple.type = scala.Tuple$package.EmptyTuple
 *
 *   (scala.runtime.Tuples.cons(i, Tuple_this).asInstanceOf[scala.*:[scala.Int, scala.Tuple$package.EmptyTuple.type]]: scala.*:[scala.Int, scala.Tuple$package.EmptyTuple])
 * }
 * }}}
 */
object ArbitraryTupleConstructionInlined {
  def unapply(using Quotes)(t: quotes.reflect.Term): Option[(quotes.reflect.Term, quotes.reflect.Term)] = {
    import quotes.reflect.{Ident => TIdent, *}
    t match {
      case
        Inlined(
          _,
          List(ValDef("Tuple_this", _, Some(prevTuple))),
          Typed(AsInstanceOf(TupleCons(head, TIdent("Tuple_this"))), _)
        ) =>
        Some((head, prevTuple))
      case _ => None
    }
  }
}

/**
 * Parses a few cases of arbitrary tuples.
 *
 * Scala 3 produces a few different trees for arbitrary tuples. Method `*:` is marked as inline.
 * Under the hood it actually invokes `Tuples.cons` function:
 * {{{
 *     inline def *: [H, This >: this.type <: Tuple] (x: H): H *: This =
 *       runtime.Tuples.cons(x, this).asInstanceOf[H *: This]
 * }}}
 * So, at least we have to match Tuples.cons.
 * However, it's not the only variation. Scala also produces a block with intermediate val `Tuple_this` definitions:
 * {{{
 *   {
 *     val Tuple_this: scala.Tuple$package.EmptyTuple.type = scala.Tuple$package.EmptyTuple
 *     val `Tuple_this₂`: scala.*:[java.lang.String, scala.Tuple$package.EmptyTuple] = (scala.runtime.Tuples.cons("", Tuple_this).asInstanceOf[scala.*:[java.lang.String, scala.Tuple$package.EmptyTuple.type]]: scala.*:[java.lang.String, scala.Tuple$package.EmptyTuple])
 *
 *     (scala.runtime.Tuples.cons(1, `Tuple_this₂`).asInstanceOf[scala.*:[scala.Int, scala.*:[java.lang.String, scala.Tuple$package.EmptyTuple]]]: scala.*:[scala.Int, scala.*:[java.lang.String, scala.Tuple$package.EmptyTuple]])
 *   }
 * }}}
 */
class ArbitraryTupleBlockParser(val rootParse: Parser)(using Quotes, TranspileConfig)
  extends Parser(rootParse)
    with PatternMatchingValues {

  import quotes.reflect.{Block => TBlock, Ident => TIdent, _}

  def attempt = {
    case '{EmptyTuple} =>
      ast.Tuple(List())
    case '{$a *: EmptyTuple} =>
      val aAst = rootParse(a)
      ast.Tuple(List(aAst))
    case inlined@Unseal(ArbitraryTupleConstructionInlined(singleValue, prevTuple)) =>
      val headAst = rootParse(singleValue.asExpr)
      val prevTupleAst = rootParse(prevTuple.asExpr)
      prevTupleAst match {
        case ast.Tuple(lst) => ast.Tuple(headAst :: lst)
        case _ =>
          throw IllegalArgumentException(s"Unexpected tuple ast ${prevTupleAst}")
      }
    case block@Unseal(TBlock(parts, ArbitraryTupleConstructionInlined(head,Typed(AsInstanceOf(TupleCons(head2,TIdent("Tuple_this"))), _)) )) if (parts.length > 0) =>
      val headAst = rootParse(head.asExpr)
      val head2Ast = rootParse(head2.asExpr)
      val partsAsts = headAst :: head2Ast :: parts.reverse.flatMap{
        case ValDef("Tuple_this", tpe, Some(TIdent("EmptyTuple"))) => List()
        case ValDef("Tuple_this", tpe, Some(Typed(AsInstanceOf(TupleCons(next,TIdent("Tuple_this"))), _))) => List(rootParse(next.asExpr))
        case ValDef("Tuple_this", tpe, Some(unknown)) =>
          throw IllegalArgumentException(s"Unexpected Tuple_this = ${unknown.show}")
      }
      Tuple(partsAsts)
  }
}

class BlockParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with PatternMatchingValues {
  import quotes.reflect.{Block => TBlock, _}

  def attempt = {
    case block @ Unseal(TBlock(parts, lastPart)) if (parts.length > 0) =>
      val partsAsts =
        parts.map {
          case term: Term      => rootParse(term.asExpr)
          case ValDefTerm(ast) => ast
          case other           =>
            // TODO Better site-description in error (does other.show work?)
            report.throwError(s"Illegal statement ${other.show} in block ${block.show}")
        }
      val lastPartAst = rootParse(lastPart.asExpr)
      Block((partsAsts :+ lastPartAst))
  }
}

class CasePatMatchParser(val rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with PatternMatchingValues {
  import quotes.reflect.{Constant => TConstant, _}

  def attempt = {
    case Unseal(PatMatchTerm(patMatch)) =>
      patMatch match {
        case PatMatch.SimpleClause(ast)                          => ast
        case PatMatch.MultiClause(clauses: List[PatMatchClause]) => nestedIfs(clauses)
        case PatMatch.AutoAddedTrivialClause =>
          Constant(true, Quat.BooleanValue)
      }
  }

  def nestedIfs(clauses: List[PatMatchClause]): Ast =
    clauses match {
      case PatMatchClause(body, guard) :: tail => ast.If(guard, body, nestedIfs(tail))
      case Nil                                 => ast.NullValue
    }
}

/** Same as traversableOperationParser, pre-filters that the result-type is a boolean */
class TraversableOperationParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[Boolean]
    with PatternMatchingValues {
  import quotes.reflect._
  def attempt = {
    case '{ ($col: collection.Map[k, v]).contains($body) } =>
      MapContains(rootParse(col), rootParse(body))
    case '{ ($col: collection.Set[v]).contains($body) } =>
      SetContains(rootParse(col), rootParse(body))
    case '{ ($col: collection.Seq[v]).contains($body) } =>
      ListContains(rootParse(col), rootParse(body))
  }
}

class OrderingParser(val rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with PatternMatchingValues {
  import quotes.reflect._

  def attempt: (History, LiftsAccum) ?=> PartialFunction[Expr[_], Ordering] = {
    case '{ implicitOrd } => AscNullsFirst

    // Doing this on a lower level since there are multiple cases of Order.apply with multiple arguemnts
    case Unseal(Apply(TypeApply(Select(Ident("Ord"), "apply"), _), args)) =>
      // parse all sub-orderings if this is a composite
      val subOrderings = args.map(_.asExpr).map(ordExpression => attempt.lift(ordExpression).getOrElse(failParse(ordExpression, classOf[Ordering])))
      TupleOrdering(subOrderings)

    case '{ Ord.asc[t] }            => Asc
    case '{ Ord.desc[t] }           => Desc
    case '{ Ord.ascNullsFirst[t] }  => AscNullsFirst
    case '{ Ord.descNullsFirst[t] } => DescNullsFirst
    case '{ Ord.ascNullsLast[t] }   => AscNullsLast
    case '{ Ord.descNullsLast[t] }  => DescNullsLast
  }
}

// TODO Pluggable-in unlifter via implicit? Quotation generic should have it in the root?
class QuotationParser(rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) {
  import quotes.reflect.{Ident => TIdent, Apply => TApply, _}

  def attempt = {

    case QuotationLotExpr.Unquoted(quotationLot) =>
      quotationLot match {
        case Uprootable(uid, astTree, lifts) =>
          summon[LiftsAccum].addLifts(lifts.map(_.plant))
          Unlifter(astTree)
        case pluckable @ Pluckable(uid, astTree, _)  =>
          summon[LiftsAccum].addPluckableUnquote(pluckable.pluck)
          QuotationTag(uid)
        case Pointable(quote)            => report.throwError(s"Quotation is invalid for compile-time or processing: ${quote.show}", quote)
      }

    case PlanterExpr.UprootableUnquote(expr) =>
      summon[LiftsAccum].addLift(expr.plant)
      ScalarTag(expr.uid, Source.Parser)

    // A inline quotation can be parsed if it is directly inline. If it is not inline, a error
    // must happen (specifically have a check for it or just fail to parse?)
    // since we would not know the UID since it is not inside of a bin. This situation
    // should only be encountered to a top-level quote passed to the 'run' function and similar situations.
    // NOTE: Technically we only need to uproot the AST here, not the lifts, but using UprootableWithLifts
    // since that's the only construct in QuotedExpr that checks if there is an Inline block at the front.
    // If needed for performance reasons, a Inlined checking extractor can be made in QuotedExpr that ignores lifts
    case QuotedExpr.UprootableWithLifts(quotedExpr, lifts) =>
      summon[LiftsAccum].addLifts(lifts.map(_.plant))
      Unlifter(quotedExpr.ast)
  }
}

// As a performance optimization, ONLY Matches things returning Action[_] UP FRONT.
// All other kinds of things rejected
class ActionParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[Action[_]]
    with Assignments
    with PropertyParser {
  import quotes.reflect.{Constant => TConstant, _}

  def combineAndCheckAndParse[T: Type, A <: Ast](first: Expr[T], others: Seq[Expr[T]])(checkClause: Expr[_] => Unit)(parseClause: Expr[_] => A): Seq[A] = {
    val assignments = (first.asTerm +: others.map(_.asTerm).filterNot(isNil(_)))
    assignments.foreach(term => checkClause(term.asExpr))
    assignments.map(term => parseClause(term.asExpr))
  }

  def attempt = {
    case '{ type t; ($query: EntityQueryModel[`t`]).insert(($first: `t` => (Any, Any)), (${ Varargs(others) }: Seq[`t` => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(first, others)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.OrFail(_))
      AInsert(rootParse(query), assignments.toList)
    case '{ type t; ($query: EntityQueryModel[`t`]).update(($first: `t` => (Any, Any)), (${ Varargs(others) }: Seq[`t` => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(first, others)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.OrFail(_))
      AUpdate(rootParse(query), assignments.toList)
    case '{ type t; ($query: EntityQueryModel[`t`]).delete } =>
      ADelete(rootParse(query))

    // Returning generated
    // summon an idiom if one can be summoned (make sure when doing import ctx._ it is provided somewhere)
    // (for dialect-specific behavior, try to summon a dialect implicitly, it may not exist since a dialect may
    // not be supported. If one exists then check the type of returning thing. If not then dont.
    // Later: Introduce a module that verifies the AST later before compilation and emits a warning if the returning type is incorrect
    //        do the same thing for dynamic contexts witha log message

    // Form:    ( (Query[Perosn]).[action](....) ).returning[T]
    // Example: ( query[Person].insertValue(lift(joe))).returning[Something]
    case '{ ($action: Insert[t]).returning[r] } =>
      report.throwError(s"A 'returning' clause must have arguments.")
    // NOTE: Need to make copies for Insert/Update/Delete because presently `Action` does not have a .returning method
    case '{ ($action: Update[t]).returning[r] } =>
      report.throwError(s"A 'returning' clause must have arguments.")
    case '{ ($action: Delete[t]).returning[r] } =>
      report.throwError(s"A 'returning' clause must have arguments.")

    case '{ ($action: Insert[t]).returning[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      // // TODO Verify that the idiom supports this type of returning clause
      // idiomReturnCapability match {
      //   case ReturningMultipleFieldSupported | ReturningClauseSupported | OutputClauseSupported =>
      //   case ReturningSingleFieldSupported =>
      //     c.fail(s"The 'returning' clause is not supported by the ${currentIdiom.getOrElse("specified")} idiom. Use 'returningGenerated' instead.")
      //   case ReturningNotSupported =>
      //     c.fail(s"The 'returning' or 'returningGenerated' clauses are not supported by the ${currentIdiom.getOrElse("specified")} idiom.")
      // }
      // // Verify that the AST in the returning-body is valid
      // idiomReturnCapability.verifyAst(bodyAst)
      Returning(rootParse(action), ident, bodyAst)
    case '{ ($action: Insert[t]).returningMany[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      Returning(rootParse(action), ident, bodyAst)

    case '{ ($action: Insert[t]).onConflictIgnore } =>
      OnConflict(rootParse(action), OnConflict.NoTarget, OnConflict.Ignore)

    case '{ type t; ($action: Insert[`t`]).onConflictIgnore(($target: `t` => Any), (${ Varargs(targets) }: Seq[`t` => Any]): _*) } =>
      val targetProperties = combineAndCheckAndParse(target, targets)(_ => ())(LambdaToProperty.OrFail(_))
      OnConflict(
        rootParse(action),
        OnConflict.Properties(targetProperties.toList),
        OnConflict.Ignore
      )

    case '{ type t; ($action: Insert[`t`]).onConflictUpdate(($assign: (`t`, `t`) => (Any, Any)), (${ Varargs(assigns) }: Seq[(`t`, `t`) => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(assign, assigns)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.Double.OrFail(_))
      OnConflict(
        rootParse(action),
        OnConflict.NoTarget,
        OnConflict.Update(assignments.toList)
      )

    case '{ type t; ($action: Insert[`t`]).onConflictUpdate(($target: `t` => Any), (${ Varargs(targets) }: Seq[`t` => Any]): _*)(($assign: (`t`, `t`) => (Any, Any)), (${ Varargs(assigns) }: Seq[(`t`, `t`) => (Any, Any)]): _*) } =>
      val assignments = combineAndCheckAndParse(assign, assigns)(AssignmentTerm.CheckTypes(_))(AssignmentTerm.Double.OrFail(_))
      val targetProperties = combineAndCheckAndParse(target, targets)(_ => ())(LambdaToProperty.OrFail(_))
      OnConflict(
        rootParse(action),
        OnConflict.Properties(targetProperties.toList),
        OnConflict.Update(assignments.toList)
      )

    // Need to make copies because presently `Action` does not have a .returning method
    case '{ ($action: Update[t]).returning[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      Returning(rootParse(action), ident, bodyAst)
    case '{ ($action: Update[t]).returningMany[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      Returning(rootParse(action), ident, bodyAst)

    case '{ ($action: Delete[t]).returning[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      Returning(rootParse(action), ident, bodyAst)
    case '{ ($action: Delete[t]).returningMany[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      Returning(rootParse(action), ident, bodyAst)

    case '{ ($action: Insert[t]).returningGenerated[r](${ Lambda1(id, tpe, body) }) } =>
      val ident = cleanIdent(id, tpe)
      val bodyAst = reprocessReturnClause(ident, rootParse(body), action, Type.of[t])
      // // TODO Verify that the idiom supports this type of returning clause
      // idiomReturnCapability match {
      //   case ReturningNotSupported =>
      //     c.fail(s"The 'returning' or 'returningGenerated' clauses are not supported by the ${currentIdiom.getOrElse("specified")} idiom.")
      //   case _ =>
      // }
      // // Verify that the AST in the returning-body is valid
      // idiomReturnCapability.verifyAst(bodyAst)
      ReturningGenerated(rootParse(action), ident, bodyAst)
  }

  private def isNil(term: Term): Boolean =
    Untype(term) match {
      case Repeated(Nil, any) => true
      case _                  => false
    }

  import io.getquill.generic.ElaborateStructure

  /**
   * Note. Ported from reprocessReturnClause in Scala2-Quill parsing. Logic is much simpler in Scala 3
   * and potentially can be simplified even further.
   * In situations where the a `.returning` clause returns the initial record i.e. `.returning(r => r)`,
   * we need to expand out the record into it's fields i.e. `.returning(r => (r.foo, r.bar))`
   * otherwise the tokenizer would be force to pass `RETURNING *` to the SQL which is a problem
   * because the fields inside of the record could arrive out of order in the result set
   * (e.g. arrive as `r.bar, r.foo`). Use use the value/flatten methods in order to expand
   * the case-class out into fields.
   */
  private def reprocessReturnClause(ident: AIdent, originalBody: Ast, action: Expr[_], actionType: Type[_]) =
    (ident == originalBody, action.asTerm.tpe) match {
      case (true, IsActionType()) =>
        val newBody =
          actionType match {
            case '[at] => ElaborateStructure.ofAribtraryType[at](ident.name, ElaborationSide.Decoding) // elaboration side is Decoding since this is for entity being returned from the Quill query
          }
        newBody
      case (true, _) =>
        report.throwError("Could not process whole-record 'returning' clause. Consider trying to return individual columns.")
      case _ =>
        originalBody
    }

  private object IsActionType {
    def unapply(term: TypeRepr): Boolean =
      term <:< TypeRepr.of[Insert[_]] || term <:< TypeRepr.of[Update[_]] || term <:< TypeRepr.of[Delete[_]]
  }

} // end ActionParser

// As a performance optimization, ONLY Matches things returning BatchAction[_] UP FRONT.
// All other kinds of things rejected
class BatchActionParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[BatchAction[_]]
    with Assignments {
  import quotes.reflect.{Constant => TConstant, _}

  def attempt = {
    case '{ type a <: Action[_] with QAC[_, _]; ($q: Query[t]).foreach[`a`, b](${ Lambda1(ident, tpe, body) })($unq) } =>
      val id = cleanIdent(ident, tpe)
      Foreach(rootParse(q), id, rootParse(body))
  }
}

class IfElseParser(rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) {
  import quotes.reflect.{Constant => TConstant, _}

  def attempt = {
    case '{
          if ($cond) { $thenClause }
          else { $elseClause }
        } =>
      ast.If(rootParse(cond), rootParse(thenClause), rootParse(elseClause))
  }
}

// We can't use PrefilterType[Option[_]] here since the types of quotations that need to match
// are not necessarily an Option[_] e.g. Option[t].isEmpty needs to match on a clause whose type is Boolean
// That's why we need to use the 'Is' object and optimize it that way here
class OptionParser(rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with Helpers {
  import quotes.reflect.{Constant => TConstant, _}

  import MatchingOptimizers._
  import extras._

  extension (quat: Quat) {
    def isProduct = quat.isInstanceOf[Quat.Product]
  }

  /**
   * Note: The -->, -@> etc.. clauses are just to optimize the match by doing an early-exit if possible.
   * they don't actaully do any application-relevant logic
   */
  def attempt = {
    case "isEmpty" --> '{ ($o: Option[t]).isEmpty } =>
      OptionIsEmpty(rootParse(o))

    case "nonEmpty" --> '{ ($o: Option[t]).nonEmpty } =>
      OptionNonEmpty(rootParse(o))

    case "isDefined" --> '{ ($o: Option[t]).isDefined } =>
      OptionIsDefined(rootParse(o))

    case "flatten" -@> '{ ($o: Option[t]).flatten($impl) } =>
      OptionFlatten(rootParse(o))

    case "map" -@> '{ ($o: Option[t]).map(${ Lambda1(id, idType, body) }) } =>
      val queryAst = rootParse(o)
      if (queryAst.quat.isProduct) OptionTableMap(rootParse(o), cleanIdent(id, idType), rootParse(body))
      else OptionMap(queryAst, cleanIdent(id, idType), rootParse(body))

    case "flatMap" -@> '{ ($o: Option[t]).flatMap(${ Lambda1(id, idType, body) }) } =>
      val queryAst = rootParse(o)
      if (queryAst.quat.isProduct) OptionTableFlatMap(rootParse(o), cleanIdent(id, idType), rootParse(body))
      else OptionFlatMap(queryAst, cleanIdent(id, idType), rootParse(body))

    case "exists" -@> '{ ($o: Option[t]).exists(${ Lambda1(id, idType, body) }) } =>
      val queryAst = rootParse(o)
      if (queryAst.quat.isProduct) OptionTableExists(rootParse(o), cleanIdent(id, idType), rootParse(body))
      else OptionExists(queryAst, cleanIdent(id, idType), rootParse(body))

    case "forall" -@> '{ ($o: Option[t]).forall(${ Lambda1(id, idType, body) }) } =>
      // TODO If is embedded warn about no checking? Investigate that case
      val queryAst = rootParse(o)
      if (queryAst.quat.isProduct) OptionTableForall(rootParse(o), cleanIdent(id, idType), rootParse(body))
      else OptionForall(queryAst, cleanIdent(id, idType), rootParse(body))

    case "getOrElse" -@> '{ type t; ($o: Option[`t`]).getOrElse($body: `t`) } =>
      OptionGetOrElse(rootParse(o), rootParse(body))

    case "contains" -@> '{ type t; ($o: Option[`t`]).contains($body: `t`) } =>
      OptionContains(rootParse(o), rootParse(body))

    case '{ ($o: Option[t]).orNull($refl) } =>
      OptionOrNull(rootParse(o))

    case '{ ($o: Option[t]).getOrNull } =>
      OptionGetOrNull(rootParse(o))

    case '{ ($o: Option[t]).filterIfDefined(${ Lambda1(id, idType, body) }) } =>
      val queryAst = rootParse(o)
      if (queryAst.quat.isProduct)
        report.throwError("filterIfDefined only allowed on individual columns, not on case classes or tuples.")
      else
        FilterIfDefined(queryAst, cleanIdent(id, idType), rootParse(body))
  }
}

// As a performance optimization, ONLY Matches things returning Query[_] UP FRONT.
// All other kinds of things rejected
class QueryParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[Query[_]]
    with PropertyAliases
    with Helpers {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}
  import MatchingOptimizers._

  private def warnVerifyNoBranches(v: VerifyNoBranches.Output, expr: Expr[_]): Unit =
    if (v.messages.nonEmpty)
      report.warning("Questionable row-class found.\n" + v.messages.map(_.msg).mkString("\n"), expr)

  def attempt = {
    case expr @ '{ type t; EntityQuery.apply[`t`] } =>
      val tpe = TypeRepr.of[t]
      val name: String = tpe.classSymbol.get.name
      val quat = InferQuat.ofType(tpe).probit
      warnVerifyNoBranches(VerifyNoBranches.in(quat), expr)
      Entity(name, List(), quat)

    case expr @ '{ querySchema[t](${ ConstExpr(name: String) }, ${ GenericSeq(properties) }: _*) } =>
      val quat = InferQuat.of[t].probit
      warnVerifyNoBranches(VerifyNoBranches.in(quat), expr)
      Entity.Opinionated(name, properties.toList.map(PropertyAliasExpr.OrFail[t](_)), quat, Renameable.Fixed)

    case "map" -@> '{ ($q: Query[qt]).map[mt](${ Lambda1(ident, tpe, body) }) } =>
      Map(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "flatMap" -@> '{ ($q: Query[qt]).flatMap[mt](${ Lambda1(ident, tpe, body) }) } =>
      FlatMap(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "filter" -@> '{ ($q: Query[qt]).filter(${ Lambda1(ident, tpe, body) }) } =>
      Filter(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "withFilter" -@> '{ ($q: Query[qt]).withFilter(${ Lambda1(ident, tpe, body) }) } =>
      Filter(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "concatMap" -@@> '{ type t1; type t2; ($q: Query[qt]).concatMap[`t1`, `t2`](${ Lambda1(ident, tpe, body) })($unknown_stuff) } => // ask Alex why is concatMap like this? what's unkonwn_stuff?
      ConcatMap(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "union" -@> '{ ($a: Query[t]).union($b) }       => Union(rootParse(a), rootParse(b))
    case "unionAll" -@> '{ ($a: Query[t]).unionAll($b) } => UnionAll(rootParse(a), rootParse(b))
    case "++" -@> '{ ($a: Query[t]).++($b) }             => UnionAll(rootParse(a), rootParse(b))

    case ("join" -@> '{ type t1; type t2; ($q1: Query[`t1`]).join[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(InnerJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))
    case ("leftJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).leftJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(LeftJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))
    case ("rightJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).rightJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(RightJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))
    case ("fullJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).fullJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(FullJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))

    case "join" -@> '{ type t1; ($q1: Query[`t1`]).join[`t1`](${ Lambda1(ident1, tpe, on) }) } =>
      FlatJoin(InnerJoin, rootParse(q1), cleanIdent(ident1, tpe), rootParse(on))
    case "leftJoin" -@> '{ type t1; ($q1: Query[`t1`]).leftJoin[`t1`](${ Lambda1(ident1, tpe, on) }) } =>
      FlatJoin(LeftJoin, rootParse(q1), cleanIdent(ident1, tpe), rootParse(on))

    case "take" -@> '{ type t; ($q: Query[`t`]).take($n: Int) } => Take(rootParse(q), rootParse(n))
    case "drop" -@> '{ type t; ($q: Query[`t`]).drop($n: Int) } => Drop(rootParse(q), rootParse(n))

    // 2-level so we don't care to select-apply it now
    case "sortBy" -@@> '{ type r; ($q: Query[t]).sortBy[`r`](${ Lambda1(ident1, tpe, body) })($ord: Ord[`r`]) } =>
      SortBy(rootParse(q), cleanIdent(ident1, tpe), rootParse(body), rootParse(ord))

    case "groupBy" -@> '{ type r; ($q: Query[t]).groupBy[`r`](${ Lambda1(ident1, tpe, body) }) } =>
      GroupBy(rootParse(q), cleanIdent(ident1, tpe), rootParse(body))

    case "groupByMap" -@@> '{ ($q: Query[t]).groupByMap[g, r](${ Lambda1(byIdent, byTpe, byBody) })(${ Lambda1(mapIdent, mapTpe, mapBody) }) } =>
      GroupByMap(rootParse(q), cleanIdent(byIdent, byTpe), rootParse(byBody), cleanIdent(mapIdent, mapTpe), rootParse(mapBody))

    case "distinctOn" -@> '{ ($q: Query[t]).distinctOn[r](${ Lambda1(ident, tpe, body) }) } =>
      rootParse(q) match {
        case fj: FlatJoin => failFlatJoin("distinctOn")
        case other        => DistinctOn(rootParse(q), cleanIdent(ident, tpe), rootParse(body))
      }

    case "distinct" --> '{ ($q: Query[t]).distinct } =>
      rootParse(q) match {
        case fj: FlatJoin => failFlatJoin("distinct")
        case other        => Distinct(other)
      }

    case "nested" --> '{ ($q: Query[t]).nested } =>
      rootParse(q) match {
        case fj: FlatJoin => failFlatJoin("nested")
        case other        => io.getquill.ast.Nested(other)
      }
  }

  def failFlatJoin(clauseName: String) =
    report.throwError(
      s"""
        |The .${clauseName} cannot be placed after a join clause in a for-comprehension. Put it before.
        |For example. Change:
        |  for { a <- query[A]; b <- query[B].join(...).nested } to:
        |  for { a <- query[A]; b <- query[B].nested.join(...) }
        |""".stripMargin
    )

  import io.getquill.JoinQuery

  private case class OnClause(ident1: String, tpe1: quotes.reflect.TypeRepr, ident2: String, tpe2: quotes.reflect.TypeRepr, on: quoted.Expr[_])
  private object withOnClause {
    def unapply(jq: Expr[_]) =
      jq match {
        case '{ ($q: JoinQuery[a, b, r]).on(${ Lambda2(ident1, tpe1, ident2, tpe2, on) }) } =>
          Some((UntypeExpr(q), OnClause(ident1, tpe1, ident2, tpe2, on)))
        case _ =>
          None
      }
  }

} // end QueryParser

/** Query contains, nonEmpty, etc... Pre-filters for a boolean output type */
class SetOperationsParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[Boolean]
    with PropertyAliases {

  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}

  def attempt = {
    case '{ type t; type u >: `t`; ($q: Query[`t`]).nonEmpty } =>
      UnaryOperation(SetOperator.`nonEmpty`, rootParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).isEmpty } =>
      UnaryOperation(SetOperator.`isEmpty`, rootParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).contains[`u`]($b) } =>
      BinaryOperation(rootParse(q), SetOperator.`contains`, rootParse(b))
  }

} // end SetOperationsParser

/**
 * Since QueryParser only matches things that output Query[_], make a separate parser that
 * parses things like query.sum, query.size etc... when needed.
 */
class QueryScalarsParser(val rootParse: Parser)(using Quotes) extends Parser(rootParse) with PropertyAliases {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}

  def attempt = {
    case '{ type t; type u >: `t`; ($q: Query[`t`]).value[`u`] }   => rootParse(q)
    case '{ type t; type u >: `t`; ($q: Query[`t`]).min[`u`] }     => Aggregation(AggregationOperator.`min`, rootParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).max[`u`] }     => Aggregation(AggregationOperator.`max`, rootParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).avg[`u`]($n) } => Aggregation(AggregationOperator.`avg`, rootParse(q))
    case '{ type t; type u >: `t`; ($q: Query[`t`]).sum[`u`]($n) } => Aggregation(AggregationOperator.`sum`, rootParse(q))
    case '{ type t; ($q: Query[`t`]).size }                        => Aggregation(AggregationOperator.`size`, rootParse(q))

    case '{ type t; type u >: `t`; min[`u`]($q) }                  => Aggregation(AggregationOperator.`min`, rootParse(q))
    case '{ type t; type u >: `t`; max[`u`]($q) }                  => Aggregation(AggregationOperator.`max`, rootParse(q))
    case '{ type t; type u >: `t`; count[`u`]($q) }                => Aggregation(AggregationOperator.`size`, rootParse(q))
    case '{ type t; type u >: `t`; avg[`u`]($q: Option[`u`])($n) } => Aggregation(AggregationOperator.`avg`, rootParse(q))
    case '{ type t; type u >: `t`; sum[`u`]($q: Option[`u`])($n) } => Aggregation(AggregationOperator.`sum`, rootParse(q))
    case '{ type t; type u >: `t`; avg[`u`]($q: `u`)($n) }         => Aggregation(AggregationOperator.`avg`, rootParse(q))
    case '{ type t; type u >: `t`; sum[`u`]($q: `u`)($n) }         => Aggregation(AggregationOperator.`sum`, rootParse(q))
  }

}

class InfixParser(val rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with Assignments {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, Apply => TApply, _}

  def attempt = {
    case '{ ($i: InfixValue).pure.asCondition }       => genericInfix(i)(true, false, Quat.BooleanExpression)
    case '{ ($i: InfixValue).asCondition }            => genericInfix(i)(false, false, Quat.BooleanExpression)
    case '{ ($i: InfixValue).generic.pure.as[t] }     => genericInfix(i)(true, false, Quat.Generic)
    case '{ ($i: InfixValue).transparent.pure.as[t] } => genericInfix(i)(true, true, Quat.Generic)
    case '{ ($i: InfixValue).pure.as[t] }             => genericInfix(i)(true, false, InferQuat.of[t])
    case '{ ($i: InfixValue).as[t] }                  => genericInfix(i)(false, false, InferQuat.of[t])
    case '{ ($i: InfixValue) }                        => genericInfix(i)(false, false, Quat.Value)
  }

  def genericInfix(i: Expr[_])(isPure: Boolean, isTransparent: Boolean, quat: Quat)(using History, LiftsAccum) = {
    val (parts, paramsExprs) = InfixComponentsOrFail.unapply(i).getOrElse { failParse(i, classOf[Infix]) }
    if (parts.exists(_.endsWith("#"))) {
      PrepareDynamicInfix(parts.toList, paramsExprs.toList)(isPure, isTransparent, quat)
    } else {
      val infixAst = Infix(parts.toList, paramsExprs.map(rootParse(_)).toList, isPure, isTransparent, quat)
      Quat.improveInfixQuat(infixAst)
    }
  }

  private object PrepareDynamicInfix {
    def apply(parts: List[String], params: List[Expr[Any]])(isPure: Boolean, isTransparent: Boolean, quat: Quat)(using History, LiftsAccum): Dynamic = {
      // Basically the way it works is like this
      //
      // sql"foo#${bar}baz" becomes:
      //   InfixInterpolator(List("foo#", "baz"), List(bar:Expr)) should become:
      //   List( Part('{"foo" /*# is dropped*/ + String.valueOf($bar:Expr)}), Part('{"baz"}) )
      //
      // sql"foo${bar}baz" becomes:
      //   InfixInterpolator(List("foo", "baz"), List(bar:Expr)) should become:
      //   List( Part('{"foo"}), Param(bar:Expr), Part('{"baz"})  )
      import InfixElement._
      val elements =
        parts.zipWithIndex.flatMap {
          case (part, index) if (index < params.length) =>
            if (part.endsWith("#")) {
              // InfixInterpolator(List("foo#", ...), List(bar:Expr)) should become:
              //   Part('{"foo" + String.valueOf($bar:Expr)}') :: Nil
              Part('{ ${ Expr(part.dropRight(1)) } + String.valueOf(${ params(index) }) }) :: Nil
            } else {
              // InfixInterpolator(List("foo", ...), List(bar:Expr)) should become:
              //   Part('{"foo"}) :: Param(bar:Expr) :: Nil
              Part(Expr(part)) :: Param(params(index)) :: Nil
            }
          // We are on the last element (i.e. no more params after this)
          // there cannot be a # here because it could not come before the dollar sign i.e. sql"#${param} #no more params here"
          case (part, index) =>
            Part(Expr(part)) :: Nil
        }

      val fused =
        (elements.foldLeft(List.empty[InfixElement]) {
          // Shorthand:
          // fus: fusion, add: addition
          // acc: accum, ele: element
          // LE: List.empty (a.k.a Nil)
          // Part("foo") is really Part('{"foo"}) or Part(Expr("foo"))
          // (m1) is the 1st match clause below, (m2) is the 2nd one

          // Here are two examples of thos this works
          //
          // List( Part('{"foo"}), Part('{"bar"}), Part('{"baz"}) ) =>
          //   (0 ) LE                                                - remaining:List( Part('{"bar"}), Part('{"baz"}) ) =>
          //   (m2) elm:Part("foo") :: acc:LE                         - remaining:List( Part('{"bar"}), Part('{"baz"}) ) =>
          //   (m1) fus:Part(fusion:"foo" + add:"bar") :: LE          - remaining:List( Part('{"baz"}) ) =>
          //   (m1) fus:Part(fusion:"foo" + "bar" + add:"baz")) :: LE - remaining:List() =>
          //
          // List( Part("foo"), Param(bar), Part("baz") ) =>
          //   (0 ) LE                                                                         - remaining:List( Part('{"foo"}), Part('{"bar"}), Part('{"baz"}), Part('{"blin"}) ) =>
          //   (m2) elm:Part("foo") :: acc:LE                                                  - remaining:List( Param(bar), Part('{"baz"}), Part('{"blin"}) ) =>
          //   (m2) elm:Param(bar)  :: acc:(Part('{"foo"})) :: LE)                             - remaining:List( Part('{"baz"}), Part('{"blin"}) ) =>
          //   (m2) elm:Part("baz") :: acc:(Param(bar) :: Part("foo")) :: LE)                  - remaining:List( Part('{"blin"}) ) =>
          //   (m1) fus:Part("baz") :: acc:(Part("blin") }) :: Param(bar) :: Part("foo") :: LE - remaining: List()
          //
          // Note that once the process is done the elements are reversed

          // (m1)
          case (Part(fusion) :: tail, Part(addition)) =>
            Part('{ $fusion + $addition }) :: tail
          // (m2)
          case (accum, element) =>
            element :: accum
        }).reverse

      val newParts =
        fused.collect {
          case Part(v) => v
        }

      val parser = SummonParser().assemble

      val (newParams, parserLiftsAccum) =
        fused.collect {
          case Param(v) =>
            val (rawAst, parserLifts) = parser(v)
            Lifter(rawAst) -> parserLifts
        }.unzip

      val (lifts, pluckedUnquotes) = {
        parserLiftsAccum.map { liftAccum =>
          liftAccum.lifts -> liftAccum.pluckableUnquotes
        }
        .unzip match {
          case (lifts, pluckable) => lifts.flatten -> pluckable.flatten
        }
      }

      // If there is a lift that one of the static parts has, the lift should be extracted anyway
      // from the outer quote. Have a look at the "with lift" test in InfixText.scala for more detail
      Dynamic(
        '{
          Quoted(
            Infix(${ Expr.ofList(newParts) }, ${ Expr.ofList(newParams) }, ${ Expr(isPure) }, ${ Expr(isTransparent) }, ${ Lifter.quat(quat) }),
            ${ Expr.ofList(lifts) },
            ${ Expr.ofList(pluckedUnquotes) }
          )
        },
        quat
      )
    } // end apply

    enum InfixElement {
      case Part(value: Expr[String])
      case Param(value: Expr[Any])
    }
  } // end PrepareDynamicInfix

} // end InfixParser

class ExtrasParser(val rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with ComparisonTechniques {
  import quotes.reflect._

  private object ExtrasModule {
    def unapply(term: Term) =
      term.tpe <:< TypeRepr.of[extras.type]
  }

  private object ExtrasMethod {
    def unapply(expr: Expr[_]): Option[(Term, String, Term)] =
      expr.asTerm match {
        case Apply(
              Apply(
                UntypeApply(Ident(op)),
                List(left)
              ),
              List(right)
            ) =>
          Some((left, op, right))
        case _ =>
          None
      }
  }

  def attempt = {
    case ExtrasMethod(a, "===", b) =>
      equalityWithInnerTypechecksAnsi(a, b)(Equal)
    case ExtrasMethod(a, "=!=", b) =>
      equalityWithInnerTypechecksAnsi(a, b)(NotEqual)
  }
}

class OperationsParser(val rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with ComparisonTechniques with QuatMaking {
  import quotes.reflect._
  import io.getquill.ast.Infix
  // Note that if we import Dsl._ here then the "like" construct
  // will be parsed from the Dsl.extensions as opposed to the exported ones?
  // need to look into potential differences in these imports.

  // Handles named operations, ie Argument Operation Argument
  object NamedOp1 {
    def unapply(expr: Expr[_]): Option[(Expr[_], String, Expr[_])] =
      UntypeExpr(expr) match {
        case Unseal(Apply(Select(Untype(left), op: String), Untype(right) :: Nil)) =>
          Some(left.asExpr, op, right.asExpr)
        case _ =>
          None
      }
  }

  private def isValue(tpe: TypeRepr) =
    isNumeric(tpe) || existsEncoderFor(tpe)

  object operator {
    def unapply(name: String) =
      name match {
        case ">"  => Some(NumericOperator.`>`)
        case ">=" => Some(NumericOperator.`>=`)
        case "<"  => Some(NumericOperator.`<`)
        case "<=" => Some(NumericOperator.`<=`)
        case _    => None
      }
  }

  def attempt = {
    case '{ ($str: String).like($other) } =>
      Infix(List("", " like ", ""), List(rootParse(str), rootParse(other)), true, false, Quat.Value)

    case NamedOp1('{ ($leftRaw: Ordered[t]) }, operator(op), right) =>
      // If this is an operator tacked-on via an implicit class (e.g. the pattern used in DateOps) it is
      // legimiate, pull out the actual implicit class argument. This is a valid case of ProtoQuill use of implicit classes.
      // (unlike extension methods)
      val left =
        leftRaw match {
          case ImplicitClassExtensionPattern(_, left) => left.asExpr
          case other                                  => other
        }

      // Whatever the case, parse the expressions that came out
      BinaryOperation(rootParse(left), op, rootParse(right))

    case expr @ NamedOp1(left, "==", right) =>
      equalityWithInnerTypechecksIdiomatic(left.asTerm, right.asTerm)(Equal)
    case expr @ NamedOp1(left, "equals", right) =>
      equalityWithInnerTypechecksIdiomatic(left.asTerm, right.asTerm)(Equal)
    case expr @ NamedOp1(left, "!=", right) =>
      equalityWithInnerTypechecksIdiomatic(left.asTerm, right.asTerm)(NotEqual)

    case NamedOp1(left, "||", right) =>
      BinaryOperation(rootParse(left), BooleanOperator.||, rootParse(right))
    case NamedOp1(left, "&&", right) =>
      BinaryOperation(rootParse(left), BooleanOperator.&&, rootParse(right))

    case '{ !($b: Boolean) } => UnaryOperation(BooleanOperator.!, rootParse(b))

    // Unary minus symbol i.e. val num: Int = 4; quote { -lift(num) }.
    // This is done term-level or we would have to do it for every numeric type
    case Unseal(Select(num, "unary_-")) if isNumeric(num.tpe) => UnaryOperation(NumericOperator.-, rootParse(num.asExpr))

    // In the future a casting system should be implemented to handle these cases.
    // Until then, let the SQL dialect take care of the auto-conversion.
    // This is done term-level or we would have to do it for every numeric type
    // toString is automatically converted into the Apply form i.e. foo.toString automatically becomes foo.toString()
    // so we need to parse it as an Apply. The others don't take arg parens so they are not in apply-form.

    case Unseal(Apply(Select(encodeable, "toString"), List())) if isValue(encodeable.tpe) =>
      val inner = rootParse(encodeable.asExpr)
      Infix(List("cast(", " as VARCHAR)"), List(inner), false, false, inner.quat)
    case Unseal(Select(num, "toInt")) if isValue(num.tpe)    => rootParse(num.asExpr)
    case Unseal(Select(num, "toLong")) if isValue(num.tpe)   => rootParse(num.asExpr)
    case Unseal(Select(num, "toShort")) if isValue(num.tpe)  => rootParse(num.asExpr)
    case Unseal(Select(num, "toFloat")) if isValue(num.tpe)  => rootParse(num.asExpr)
    case Unseal(Select(num, "toDouble")) if isValue(num.tpe) => rootParse(num.asExpr)
    case Unseal(Select(num, "toByte")) if isValue(num.tpe)   => rootParse(num.asExpr)
    case Unseal(Select(num, "toChar")) if isValue(num.tpe)   => rootParse(num.asExpr)

    // TODO not sure how I want to do this on an SQL level. Maybe implement using SQL function containers since
    // they should be more dialect-portable then just infix
    case '{ ($str: String).length } => Infix(List("Len(", ")"), List(rootParse(str)), true, false, Quat.Value)

    // String Operations Cases
    case NamedOp1(left, "+", right) if is[String](left) || is[String](right) =>
      BinaryOperation(rootParse(left), StringOperator.+, rootParse(right))

    case '{ ($i: String).toString }                 => rootParse(i)
    case '{ ($str: String).toUpperCase }            => UnaryOperation(StringOperator.toUpperCase, rootParse(str))
    case '{ ($str: String).toLowerCase }            => UnaryOperation(StringOperator.toLowerCase, rootParse(str))
    case '{ ($str: String).toLong }                 => UnaryOperation(StringOperator.toLong, rootParse(str))
    case '{ ($str: String).toInt }                  => UnaryOperation(StringOperator.toInt, rootParse(str))
    case '{ ($left: String).startsWith($right) }    => BinaryOperation(rootParse(left), StringOperator.startsWith, rootParse(right))
    case '{ ($left: String).split($right: String) } => BinaryOperation(rootParse(left), StringOperator.split, rootParse(right))

    // 1 + 1
    // Apply(Select(Lit(1), +), Lit(1))
    // Expr[_] => BinaryOperation
    case NumericOperation(binaryOperation) =>
      binaryOperation
  }

  // def isPrimitiveOrFail(using Quotes)(opName: String)(tpe: quotes.reflect.TypeRepr) =
  //   import quotes.reflect._
  //   if (isPrimitive(tpe)) true
  //   else
  //     report.throwError(s"Can only perform the operation `${opName}` on primitive types but found the type: ${Format.TypeRepr(tpe.widen)} (primitive types are: Int,Long,Short,Float,Double,Boolean,Char)")

  object NumericOperation {
    def unapply(expr: Expr[_])(using History, LiftsAccum): Option[BinaryOperation] =
      UntypeExpr(expr) match {
        case NamedOp1(left, NumericOpLabel(binaryOp), right) if (isNumeric(left.asTerm.tpe) && isNumeric(right.asTerm.tpe)) =>
          Some(BinaryOperation(rootParse(left), binaryOp, rootParse(right)))
        case _ => None
      }
  }

  object NumericOpLabel {
    def unapply(str: String): Option[BinaryOperator] =
      str match {
        case "+"  => Some(NumericOperator.`+`)
        case "-"  => Some(NumericOperator.-)
        case "*"  => Some(NumericOperator.*)
        case "/"  => Some(NumericOperator./)
        case "%"  => Some(NumericOperator.%)
        case ">"  => Some(NumericOperator.`>`)
        case "<"  => Some(NumericOperator.`<`)
        case ">=" => Some(NumericOperator.`>=`)
        case "<=" => Some(NumericOperator.`<=`)
        case _    => None
      }
  }
}

/**
 * Should check that something is a null-constant basically before anything else because
 * null-constant can match anything e.g. a (something: SomeValue) clause. Found this out
 * when tried to do just '{ (infix: InfixValue) } and 'null' matched it
 */
class ValueParser(rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with QuatMaking {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}

  def attempt = {
    // Parse Null values
    case Unseal(Literal(NullConstant())) => NullValue
    // null.asInstanceOf[someType]
    case Unseal(TypeApply(Select(Literal(NullConstant()), "asInstanceOf"), List(Inferred()))) => NullValue
    // For cases where a series of flatMaps returns nothing etc...
    case Unseal(Literal(UnitConstant())) => Constant((), Quat.Value)
    // Parse Constants
    case expr @ Unseal(ConstantTerm(v)) => Constant(v, InferQuat.ofExpr(expr))
    // Parse Option constructors
    case '{ Some.apply[t]($v) }   => OptionSome(rootParse(v))
    case '{ Option.apply[t]($v) } => OptionApply(rootParse(v))
    case '{ None }                => OptionNone(Quat.Null)
    case '{ Option.empty[t] }     => OptionNone(InferQuat.of[t])
  }
}

class ComplexValueParser(rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with QuatMaking
    with Helpers {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}

  def attempt = {
    case ArrowFunction(prop, value) =>
      Tuple(List(rootParse(prop), rootParse(value)))

    // Parse tuples
    case Unseal(Apply(TypeApply(Select(TupleIdent(), "apply"), types), values)) =>
      Tuple(values.map(v => rootParse(v.asExpr)))

    case CaseClassCreation(ccName, fields, args) =>
      if (fields.length != args.length)
        throw new IllegalArgumentException(s"In Case Class ${ccName}, does not have the same number of fields (${fields.length}) as it does arguments ${args.length} (fields: ${fields}, args: ${args.map(_.show)})")
      val argsAst = args.map(rootParse(_))
      CaseClass(ccName, fields.zip(argsAst))

    case orig @ Unseal(i @ TIdent(x)) =>
      cleanIdent(i.symbol.name, InferQuat.ofType(i.tpe))
  }
}

class GenericExpressionsParser(val rootParse: Parser)(using Quotes, TranspileConfig) extends Parser(rootParse) with PropertyParser {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, Apply => TApply, _}
  import reflect.Selectable.reflectiveSelectable

  def attempt = {
    case expr @ ImplicitClassExtensionPattern(cls, constructorArg) =>
      report.throwError(ImplicitClassExtensionPattern.errorMessage(expr, cls, constructorArg), expr)

    case UncastSelectable('{ reflectiveSelectable($v).selectDynamic($propNameExpr) }) =>
      val propName =
        propNameExpr match {
          case Expr(v) => v
          case _       => report.throwError(s"Cannot parse the property ${Format.Expr(propNameExpr)}. It was not a static string property.")
        }
      Property(rootParse(v), propName)

    case AnyProperty(property) => property

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      rootParse(innerTree.asExpr)

    case Unseal(Inlined(_, _, v)) =>
      rootParse(v.asExpr)
  }
}
