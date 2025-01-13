package io.getquill.parser.engine

import scala.quoted._
import io.getquill.ast.Ast
import io.getquill.parser.ParserHelpers

trait Parser(rootParse: Parser | Parser.Nil)(using Quotes) {
  import quotes.reflect._
  def apply(input: Expr[_])(using History, LiftsAccum): Ast = attempt.lift(input).getOrElse(error(input))
  protected def error(input: Expr[_]): Nothing = failParse(input, classOf[Ast])
  protected def attempt: (History, LiftsAccum) ?=> PartialFunction[Expr[_], Ast]
  // Attempt the parser externally. Usually this is the just the `attempt` method
  // but in some cases we might want early-exist functionality.
  private[engine] def attemptProper: (History, LiftsAccum) ?=> PartialFunction[Expr[_], Ast] = attempt
}

object Parser {
  sealed trait Nil
  object Nil extends Nil

  def empty(rootParse: Parser | Parser.Nil)(using Quotes) =
    new Parser(rootParse) {
      protected def attempt: (History, LiftsAccum) ?=> PartialFunction[Expr[_], Ast] = PartialFunction.empty
    }

  /** Optimizes 'Clause' by checking if it is some given type first. Otherwise can early-exit */
  trait PrefilterType[Criteria: Type](using Quotes) extends Parser {
    import quotes.reflect._
    def prefilter(expr: Expr[_]): Boolean =
      expr.asTerm.tpe <:< TypeRepr.of[Criteria]
  }

  /** Optimizes 'Clause' by allowing a more efficient 'prematch' criteria to be used */
  trait Prefilter(using Quotes) extends Parser {
    def prefilter(expr: Expr[_]): Boolean
    private[engine] override def attemptProper: (History, LiftsAccum) ?=> PartialFunction[Expr[_], Ast] =
      new PartialFunction[Expr[_], Ast] {
        def apply(expr: Expr[_]): Ast = attempt.apply(expr)
        def isDefinedAt(expr: Expr[_]): Boolean = prefilter(expr) && attempt.isDefinedAt(expr)
      }
  }

} // end Parser
