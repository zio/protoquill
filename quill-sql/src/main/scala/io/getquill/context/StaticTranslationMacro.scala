package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import io.getquill.generic.EncodingDsl
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{ Transform, QuotationTag }
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.ast.{ Query => AQuery, _ }
import scala.util.{Success, Failure}
import io.getquill.idiom.Statement
import io.getquill.QAC
import io.getquill.NamingStrategy
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.generic.ElaborateTrivial
import io.getquill.util.Format
import io.getquill.util.Interpolator
import io.getquill.util.Messages.TraceType
import io.getquill.util.ProtoMessages
import io.getquill.context.StaticTranslationMacro
import io.getquill.quat.Quat

object StaticTranslationMacro:
  import io.getquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import io.getquill.Planter
  import io.getquill.idiom.LoadNaming
  import io.getquill.util.Load
  import io.getquill.generic.GenericEncoder
  import io.getquill.ast.External
  import io.getquill.ReturnAction
  import io.getquill.NamingStrategy

  // Process the AST during compile-time. Return `None` if that can't be done.
  private[getquill] def processAst[T: Type](astExpr: Expr[Ast], topLevelQuat: Quat, wrap: ElaborationBehavior, idiom: Idiom, naming: NamingStrategy)(using Quotes):Option[(Unparticular.Query, List[External], Option[ReturnAction], Ast)] =
    import io.getquill.ast.{CollectAst, QuotationTag}

    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty

    val unliftedAst = VerifyFreeVariables(Unlifter(astExpr))

    if (noRuntimeQuotations(unliftedAst)) {
      val expandedAst = ElaborateTrivial(wrap)(unliftedAst)
      val (ast, stmt, _) = idiom.translate(expandedAst, topLevelQuat, ExecutionType.Static)(using naming)

      val liftColumns =
        (ast: Ast, stmt: Statement) => Unparticular.translateNaive(stmt, idiom.liftingPlaceholder)

      val returningAction = expandedAst match
        // If we have a returning action, we need to compute some additional information about how to return things.
        // Different database dialects handle these things differently. Some allow specifying a list of column-names to
        // return from the query. Others compute this information from the query data directly. This information is stored
        // in the dialect and therefore is computed here.
        case r: ReturningAction =>
          Some(io.getquill.norm.ExpandReturning.applyMap(r)(liftColumns)(idiom, naming))
        case _ =>
          None

      val (unparticularQuery, externals) = Unparticular.Query.fromStatement(stmt, idiom.liftingPlaceholder)
      Some((unparticularQuery, externals, returningAction, unliftedAst))
    } else {
      None
    }
  end processAst

  /**
   * There are some cases where we actually do not want to use all of the lifts in a Quoted.
   * For example:
   * {{ query[Person].insert(_.id -> lift(1), _.name -> lift("Joe")).returningGenerated(_.id)) }}
   * becomes something like:
   * {{ Quoted(query[Person].insert(_.id -> lift(A), _.name -> lift(B)).returningGenerated(_.id)), lifts: List(ScalarTag(A, 1), ScalarTag(B, "Joe"))) }}
   * but since we are excluding the person.id column (this is done in the transformation phase NormalizeReturning which is in SqlNormalization in the quill-sql-portable module)
   * actually we only want only the ScalarTag(B) so we need to get the list of lift tags (in tokens) once the Dialect has serialized the query
   * which correctly order the list of lifts. A similar issue happens with insertMeta and updateMeta.
   * Process compile-time lifts, return `None` if that can't be done.
   * liftExprs = Lifts that were put into planters during the quotation. They are
   * 're-planted' back into the PreparedStatement vars here.
   * matchingExternals = the matching placeholders (i.e 'lift tags') in the AST
   * that contains the UUIDs of lifted elements. We check against list to make
   * sure that that only needed lifts are used and in the right order.
   */
  private[getquill] def processLifts(lifts: List[PlanterExpr[_, _, _]], matchingExternals: List[External])(using Quotes): Option[List[PlanterExpr[_, _, _]]] =
    import quotes.reflect.report

    val encodeablesMap =
      lifts.map(e => (e.uid, e)).toMap

    val uidsOfScalarTags =
      matchingExternals.collect {
        case tag: ScalarTag => tag.uid
      }

    val sortedEncodeables =
      uidsOfScalarTags.map { uid =>
        encodeablesMap.get(uid) match
          case Some(encodeable) => encodeable
          case None =>
            report.throwError(s"Invalid Transformations Encountered. Cannot find lift with ID: ${uid}.")
      }

    // TODO This should be logged if some fine-grained debug logging is enabled. Maybe as part of some phase that can be enabled via -Dquill.trace.types config
    // val remaining = encodeables.removedAll(uidsOfScalarTags)
    // if (!remaining.isEmpty)
    //   println(s"Ignoring the following lifts: [${remaining.map((_, v) => Format.Expr(v.plant)).mkString(", ")}]")
    Some(sortedEncodeables)
  end processLifts

  def idiomAndNamingStatic[D <: Idiom, N <: NamingStrategy](using Quotes, Type[D], Type[N]): Try[(Idiom, NamingStrategy)] =
    for {
      idiom <- Load.Module[D]
      namingStrategy <- LoadNaming.static[N]
    } yield (idiom, namingStrategy)

  def apply[I: Type, T: Type, D <: Idiom, N <: NamingStrategy](
    quotedRaw: Expr[Quoted[QAC[I, T]]],
    wrap: ElaborationBehavior,
    topLevelQuat: Quat
  )(using qctx:Quotes, dialectTpe:Type[D], namingType:Type[N]): Option[StaticState] =
    import quotes.reflect.{Try => TTry, _}
    // NOTE Can disable if needed and make quoted = quotedRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val quoted = quotedRaw.asTerm.underlyingArgument.asExpr

    extension [T](opt: Option[T]) {
      def errPrint(str: => String) =
        opt match {
          case s: Some[T] => s
          case None =>
            if (HasDynamicSplicingHint.fail)
              report.throwError(str)
            else
              if (io.getquill.util.Messages.tracesEnabled(TraceType.Standard))
                println(s"[StaticTranslationError] ${str}")
              None
        }
    }

    val tryStatic =
      for {
        (idiom, naming) <-
          idiomAndNamingStatic[D, N].toOption.errPrint(
            s"Could not parse Idiom/Naming from ${Format.TypeOf[D]}/${Format.TypeOf[N]}"
          )

        // TODO (MAJOR) Really should plug quotedExpr into here because inlines are spliced back in but they are not properly
        // recognized by QuotedExpr.uprootableOpt for some reason

        (quotedExpr, lifts) <-
          QuotedExpr.uprootableWithLiftsOpt(quoted).errPrint(
            s"Could not uproot (i.e. compile-time extract) the quote: `${Format.Expr(quoted)}`. Make sure it is an `inline def`. If it already is, this may be a quill error."
          )

        (query, externals, returnAction, ast) <-
          processAst[T](quotedExpr.ast, topLevelQuat, wrap, idiom, naming).errPrint(s"Could not process the AST:\n${Format.Expr(quotedExpr.ast)}")

        encodedLifts <-
          processLifts(lifts, externals).errPrint(s"Could not process the lifts:\n${lifts.map(_.toString).mkString(",\n")}")

      } yield {
        if (io.getquill.util.Messages.debugEnabled)
          queryPrint(PrintType.Query(query.basicQuery), Some(idiom))

        StaticState(query, encodedLifts, returnAction, idiom)(ast)
      }

    if (tryStatic.isEmpty)
      queryPrint(PrintType.Message(s"Dynamic Query Detected"), None)

    tryStatic
  end apply

  private[getquill] enum PrintType:
    case Query(str: String)
    case Message(str: String)

  private[getquill] def queryPrint(printType: PrintType, idiomOpt: Option[Idiom])(using Quotes) =
    import quotes.reflect._
    import io.getquill.util.IndentUtil._

    val msg =
      printType match
        case PrintType.Query(queryString) =>
          val formattedQueryString =
            if (io.getquill.util.Messages.prettyPrint)
              idiomOpt.map(idiom => idiom.format(queryString)).getOrElse(queryString)
            else
              queryString

          if (ProtoMessages.useStdOut)
            s"Quill Query:\n${formattedQueryString.multiline(1, "")}"
          else
            s"Quill Query: ${formattedQueryString}"

        case PrintType.Message(str) =>
          str
      end match

    if (ProtoMessages.useStdOut)
      val posValue = Position.ofMacroExpansion
      val pos = s"\nat: ${posValue.sourceFile}:${posValue.startLine + 1}:${posValue.startColumn + 1}"
      println(msg + pos)
    else
      report.info(msg)
  end queryPrint

end StaticTranslationMacro