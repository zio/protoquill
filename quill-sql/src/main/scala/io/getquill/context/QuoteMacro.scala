package io.getquill.context

import scala.quoted._
import io.getquill.parser.ParserFactory
import io.getquill.util.Load
import io.getquill.norm.BetaReduction
import io.getquill.parser.engine.Parser
import io.getquill.parser.Lifter
import io.getquill.metaprog.PlanterExpr
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.Pluckable
import io.getquill.metaprog.Pointable
import io.getquill.Quoted
import io.getquill.metaprog.SummonParser
import io.getquill.metaprog.SummonSerializationBehaviors
import io.getquill.parser.engine.History
import io.getquill.context.sql.norm.SimplifyFilterTrue
import io.getquill.parser.Unlifter
import io.getquill.util.Format
import io.getquill.norm.TranspileConfig
import io.getquill.metaprog.SummonTranspileConfig
import io.getquill.ast.Ast
import io.getquill.ast.StatefulTransformer
import io.getquill.QuotationVase

object ExtractLifts {
  // Find all lifts, dedupe by UID since lifts can be inlined multiple times hence
  // appearing in the AST in multiple places.
  def extractLifts(body: Expr[Any])(using Quotes) = {
    // TODO If we want to support functionality to resolve all LazyPlanters into
    // eager Planters before the 'run' function, need to look thorugh eager/lazy
    // (planters) with the same UID and choose the ones that are eager for the same UID
    // i.e. since they would be the resolved ones
    // val m =

    // order of the lifts shuold not matter
    // PlanterExpr.findUnquotes(body).zipWithIndex           // preserve the original order
    //   .groupBy((r, idx) => r.uid)                         // group by uids
    //   .map((uid, planters) => planters.sortBy(_._2).head) // for each uid, pick first index
    //   .toList.sortBy(_._2).map(_._1)                      // once that's done, sort by original index
    //   .map(_.plant)                                       // then replant

    import quotes.reflect._

    // TODO First one for each UID should win because it's the outermost one, should make sure tha't the case
    // PlanterExpr.findUnquotes(body).foldLeft(LinkedHashMap[String, PlanterExpr[_, _, _]]())((elem, map) => (map.addIfNotContains(elem.uid, elem)))
    PlanterExpr.findUnquotes(body).distinctBy(_.uid).map(_.plant)
  }

  def extractRuntimeUnquotes(body: Expr[Any])(using Quotes) = {
    import quotes.reflect.report
    val unquotes = QuotationLotExpr.findUnquotes(body)
    unquotes
      .collect {
        case expr: Pluckable => expr
        case Pointable(expr) =>
          report.throwError(s"Invalid runtime Quotation: ${expr.show}. Cannot extract a unique identifier.", expr)
      }
      .distinctBy(_.uid)
      .map(_.pluck)
  }

  def apply(body: Expr[Any])(using Quotes) =
    (extractLifts(body), extractRuntimeUnquotes(body))
}

object QuoteMacro {

  object DynamicsExtractor {
    import io.getquill.ast.Dynamic
    import io.getquill.ast.QuotationTag
    import java.util.UUID
    import io.getquill.metaprog.Extractors._

    case class Extractee(uid: String, dynamic: Dynamic)
    case class Transform(state: List[Extractee]) extends StatefulTransformer[List[Extractee]] {
      override def apply(e: Ast): (Ast, StatefulTransformer[List[Extractee]]) =
        e match {
          case dyn: Dynamic =>
            val uid = UUID.randomUUID().toString()
            (QuotationTag(uid), Transform(Extractee(uid, dyn) +: state))
          case _ => super.apply(e)
        }

    }

    def apply(ast: Ast)(using Quotes): (Ast, List[Expr[QuotationVase]]) = {
      import quotes.reflect._

      def printAstWithCustom(ast: Ast)(uid: String, replacementText: String) = {
        import io.getquill.MirrorIdiom._
        import io.getquill.idiom.StatementInterpolator._
        import io.getquill.ast.External
        implicit def externalTokenizer: Tokenizer[External] =
          Tokenizer[External] {
            case QuotationTag(tagUid) if (tagUid == uid) =>
              replacementText.token
            case _ => stmt"?"
          }
        ast.token.toString
      }

      val (newAst, transformer) = Transform(List())(ast)
      val extracted = transformer.state.reverse
      val quotations =
        extracted.map {
          case Extractee(uid, Dynamic(value, quat)) =>
            val quotation =
              value match {
                case expr: Expr[_] if (is[Quoted[_]](expr)) =>
                  expr.asExprOf[Quoted[_]]
                case expr: Expr[_] =>
                  report.throwError(s"Dynamic value has invalid expression: ${Format.Expr(expr)} in the AST:\n${printAstWithCustom(newAst)(uid, "<INVALID-HERE>")}")
                case other =>
                  report.throwError(s"Dynamic value is not an expression: ${other} in the AST:\n${printAstWithCustom(newAst)(uid, "<INVALID-HERE>")}")
              }

            '{ QuotationVase($quotation, ${ Expr(uid) }) }
        }

      (newAst, quotations)
    }
  }

  def apply[T](bodyRaw: Expr[T])(using Quotes, Type[T], Type[Parser]): Expr[Quoted[T]] = {
    import quotes.reflect._

    // NOTE Can disable underlyingArgument here if needed and make body = bodyRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val body = bodyRaw.asTerm.underlyingArgument.asExpr

    val parser = SummonParser().assemble
    val (serializeQuats, serializeAst) = SummonSerializationBehaviors()
    given TranspileConfig = SummonTranspileConfig()

    val (rawAst, parserLifts) = parser(body)
    val (noDynamicsAst, dynamicQuotes) = DynamicsExtractor(rawAst)
    val ast = SimplifyFilterTrue(BetaReduction(noDynamicsAst))

    val reifiedAst = Lifter.WithBehavior(serializeQuats, serializeAst)(ast)
    val u = Unlifter(reifiedAst)

    val (lifts, pluckedUnquotes) = (parserLifts.lifts, parserLifts.pluckableUnquotes)

    '{ Quoted[T](${ reifiedAst }, ${ Expr.ofList(lifts) }, ${ Expr.ofList(pluckedUnquotes ++ dynamicQuotes) }) }
  }
}
