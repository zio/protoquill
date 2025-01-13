package io.getquill.parser.engine

import scala.quoted._
import scala.reflect.ClassTag
import io.getquill.ast.Ast
import io.getquill.util.Format
import io.getquill.norm.TranspileConfig

sealed trait ParserChain(using Quotes, TranspileConfig) {
  self =>
  def name: String
  protected def build(rootParse: Parser): Parser
  def orElse(that: ParserChain): ParserChain = ParserChain.OrElse(self, that)
  lazy val complete: Parser =
    new Parser(Parser.Nil) {
      built =>
      def attempt = build(built).attemptProper
    }
}

object ParserChain {
  import scala.quoted._
  def attempt[P <: Parser: ClassTag](rootInjector: Parser => P)(using Quotes, TranspileConfig): ParserChain =
    Attempt[P](rootInjector)

  private final case class Attempt[P <: Parser: ClassTag](rootInjector: Parser => P)(using Quotes, TranspileConfig) extends ParserChain {
    lazy val name = summon[ClassTag[P]].runtimeClass.getSimpleName
    protected def build(rootParse: Parser) = rootInjector(rootParse)
  }

  private final case class OrElse(left: ParserChain, right: ParserChain)(using Quotes, TranspileConfig) extends ParserChain {
    def name = s"${left.name}_or_${right.name}"
    protected def build(rootParse: Parser): Parser =
      new Parser(rootParse) {
        def attempt = {
          val leftOrRightMatch: PartialFunction[Expr[_], Option[Ast]] =
            PartialFunction.fromFunction[Expr[_], Option[Ast]](expr => {
              val leftParser = left.build(rootParse)
              val rightParser = right.build(rootParse)
              val history = summon[History]
              val lifts = summon[LiftsAccum]
              val leftHistory = History.Matched(left, history)(Format.Expr(expr))
              // if the left side parser did not match, that means that it was ignored so add that info to the history
              val rightHistory = History.Matched(right, History.Ignored(left, history)(Format.Expr(expr)))(Format.Expr(expr))
              val leftLift: Expr[_] => Option[Ast] = leftParser.attemptProper(using leftHistory, lifts).lift
              val rightLift: Expr[_] => Option[Ast] = rightParser.attemptProper(using rightHistory, lifts).lift
              leftLift(expr).orElse(rightLift(expr))
            })
          leftOrRightMatch.unlift
        } // end attempt
      }
  }
}
