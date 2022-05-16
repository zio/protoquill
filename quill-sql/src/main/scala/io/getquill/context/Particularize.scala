package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.InjectableEagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.LazyPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.{Map => AMap, _}
import io.getquill.util.Interleave
import io.getquill.idiom.StatementInterpolator._
import scala.annotation.tailrec
import io.getquill.idiom._
import scala.quoted._
import io.getquill.util.Format
import io.getquill.metaprog.InjectableEagerPlanterExpr

/**
 * For a query that has a filter(p => liftQuery(List("Joe","Jack")).contains(p.name)) we need to turn
 * the "WHERE p.name in (?)" into WHERE p.name in (?, ?) i.e. to "Particularize" the query
 * to the number of elements in the query lift. In Scala2-Quill we could just access the values
 * of the liftQuery list directly since the lift was an 'Any' value directly in the AST.
 * In Scala 3 however, we need to treat the lifted list as an Expr and create an Expr[String]
 * that represents the Query that is to be during runtime based on the content of the list
 * which has to be manipulated inside of a '{ ... } block.
 */
object Particularize:
  // ====================================== TODO additional-lifts case here too ======================================
  // ====================================== TODO additional-lifts case here too ======================================
  // ====================================== TODO additional-lifts case here too ======================================
  // ====================================== TODO additional-lifts case here too ======================================
  // ====================================== TODO additional-lifts case here too ======================================
  // the following should test for that: update - extra lift + scalars + liftQuery/setContains
  object Static:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp](query: Unparticular.Query, lifts: List[Expr[Planter[_, _, _]]], runtimeLiftingPlaceholder: Expr[Int => String], emptySetContainsToken: Token => Token)(using Quotes): Expr[String] =
      raw(query.realQuery, lifts, runtimeLiftingPlaceholder, emptySetContainsToken)

    private[getquill] def raw[PrepareRowTemp, Session](statement: Statement, lifts: List[Expr[Planter[_, _, _]]], runtimeLiftingPlaceholder: Expr[Int => String], emptySetContainsToken: Token => Token)(using Quotes): Expr[String] = {
      import quotes.reflect._

      enum LiftChoice:
        case ListLift(value: EagerListPlanterExpr[Any, PrepareRowTemp, Session])
        case SingleLift(value: PlanterExpr[Any, PrepareRowTemp, Session])

      val listLifts: Map[String, EagerListPlanterExpr[Any, PrepareRowTemp, Session]] =
        lifts.collect {
          case PlanterExpr.Uprootable(planterExpr: EagerListPlanterExpr[_, _, _]) =>
            planterExpr.asInstanceOf[EagerListPlanterExpr[Any, PrepareRowTemp, Session]]
        }.map(lift => (lift.uid, lift)).toMap

      val singleLifts: Map[String, EagerPlanterExpr[Any, PrepareRowTemp, Session]] =
        lifts.collect {
          case PlanterExpr.Uprootable(planterExpr: EagerPlanterExpr[_, _, _]) =>
            planterExpr.asInstanceOf[EagerPlanterExpr[Any, PrepareRowTemp, Session]]
        }.map(lift => (lift.uid, lift)).toMap

      val injectableLifts: Map[String, InjectableEagerPlanterExpr[Any, PrepareRowTemp, Session]] =
        lifts.collect {
          case PlanterExpr.Uprootable(planterExpr: InjectableEagerPlanterExpr[_, _, _]) =>
            planterExpr.asInstanceOf[InjectableEagerPlanterExpr[Any, PrepareRowTemp, Session]]
        }.map(lift => (lift.uid, lift)).toMap

      def getLifts(uid: String): LiftChoice =
        listLifts.get(uid).map(LiftChoice.ListLift(_))
          .orElse(singleLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .orElse(injectableLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .getOrElse {
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts.map(io.getquill.util.Format.Expr(_))})")
          }

      /**
       * Actual go from a liftQuery(List("Joe", "Jack")) to "?, ?" using the lifting placeholder.
       * Also return how much the index should be incremented
       */
      def placeholders(uid: String, initialIndex: Expr[Int]): (Expr[Int], Expr[String], LiftChoice) =
        val liftType = getLifts(uid)
        liftType match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder = '{ ${ lifts.expr }.zipWithIndex.map((_, index) => $runtimeLiftingPlaceholder($initialIndex + index)).mkString(", ") }
            val liftsLength = '{ ${ lifts.expr }.length }
            (liftsLength, liftsPlaceholder, liftType)
          case LiftChoice.SingleLift(lift) =>
            (Expr(1), '{ $runtimeLiftingPlaceholder($initialIndex) }, liftType)

      object Matrowl:
        sealed trait Ground:
          override def toString = "Gnd"
        case object Ground extends Ground
        def Bottom = Matrowl(List(), Matrowl.Ground)

      /**
       * A Matrowl lit. Matroshka + Bowl is essentially a stack where each frame consists of a list of items.
       * You can add to the list on the top of the stack, pop the current Matrowl, or stack another one on top of it.
       * This datastructure became necessary in the token2Expr function when I realized in that in the case of:
       * Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag))), the list this tag points to can be empty which means
       * that the emptySetContainsToken needs to be expanded instead of the Expr[String] that is returned by the placeholders
       * function. The problem however is that we only know at runtime whether the list is zero or non-zero elements long.
       * This leads us to the requirement to either make token2Expr on tail recursive and introduce a something like this:
       * {{
       *   case Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag))) =>
       *   '{if (list.length != 0)
       *      token2Expr(...)
       *     else
       *      token2Expr(emptySetContainsToken(a), ...)
       *    }
       * }}
       * This of course is no longer tail-recursive and therefore would require a stack-frame for every token
       * that needs to be Expr[String]'ed. One possible alternative would be to trampoline the entire execution
       * however, that would likely introduce a significant performance penalty. Instead, a simplification can be made
       * in which variations of the conditional (i.e. the regular expansion and the emptySetContainsToken one)
       * are expanded and they are kept separate in the 'done-pile' of token2Expr in some kind of data strucuture
       * from which they can be picked up later.
       * The following sequence of steps therefore emerges when running into a
       * `case Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag)))` where `tag` is a list lift:
       * <li> Take the current done-area of token2Expr and stack a new matrowl above it
       * <li> Process all the tokens that would be needed to apply a emptySetContainsToken tokenization
       * <li> Add yet another stack frame on top of the matrowl
       * <li> Process all the tokens that would be needed to apply a regular tokenization of the list
       * i.e. `stmt"$a $op (") :: Work.AlreadyDone(liftsExpr) :: Work.Token(stmt")")` etc... and place them
       * onto a the matrowl we just created.
       * <li> Pop the two created stack frames into groups (one, two) and splice them into the '{if (list.length != 0) {one} else {two}}`
       * note that they will come out in the opposite order from which they were put in.
       */
      case class Matrowl private (doneWorks: List[Expr[String]], below: Matrowl | Matrowl.Ground):
        def dropIn(doneWork: Expr[String]): Matrowl =
          // println(s"Dropping: ${Format.Expr(doneWork)} into ${this.toString}")
          this.copy(doneWorks = doneWork +: this.doneWorks)
        def stack: Matrowl =
          // println(s"Stack New Matrowl ():=> ${this.toString}")
          Matrowl(List(), this)
        def pop: (List[Expr[String]], Matrowl) =
          // println(s"Pop Top Matrowl: ${this.toString}")
          below match
            case m: Matrowl        => (doneWorks, m)
            case e: Matrowl.Ground => report.throwError("Tokenization error, attempted to pop a bottom-level element")
        def pop2: (List[Expr[String]], List[Expr[String]], Matrowl) =
          // println(s"Pop Two Matrowls...")
          val (one, firstBelow) = pop
          val (two, secondBelow) = firstBelow.pop
          (one, two, secondBelow)
        def isBottom: Boolean =
          below match
            case m: Matrowl        => false
            case e: Matrowl.Ground => true
        def scoop: List[Expr[String]] =
          // println(s"Scoop From Matrowl: ${this.toString}")
          doneWorks
        override def toString = s"(${doneWorks.map(Format.Expr(_)).mkString(", ")}) -> ${below.toString}"
      end Matrowl

      enum Work:
        case AlreadyDone(expr: Expr[String])
        case Token(token: io.getquill.idiom.Token)
        // Stack the Matrowl
        case Stack
        // Pop the Matrowl
        case Pop2(finished: (Expr[String], Expr[String]) => Expr[String])
      object Work:
        def StackL = List(Work.Stack)

      extension (stringExprs: Seq[Expr[String]])
        def mkStringExpr = stringExprs.foldLeft(Expr(""))((concatonation, nextExpr) => '{ $concatonation + $nextExpr })

      def token2Expr(token: Token): Expr[String] = {
        @tailrec
        def apply(
            workList: List[Work],
            matrowl: Matrowl,
            placeholderCount: Expr[Int] // I.e. the index of the '?' that is inserted in the query (that represents a lift) or the $N if an actual number is used (e.g. in the H2 context)
        ): Expr[String] = workList match {
          case Nil =>
            if (!matrowl.isBottom)
              report.throwError("Did not get to the bottom of the stack while tokenizing")
            matrowl.scoop.reverse.mkStringExpr
          case head :: tail =>
            head match {
              case Work.Stack          => apply(tail, matrowl.stack, placeholderCount)
              case Work.Pop2(finished) =>
                // we expect left := workIfListNotEmpty and right := workIfListEmpty
                // this is the logical completion of the SetContainsToken(a, op, ScalarTagToken(tag)) case
                // (note that these should come off in reversed order from the one they were put in)
                val (left, right, restOfMatrowl) = matrowl.pop2
                val finishedExpr = finished(left.reverse.mkStringExpr, right.reverse.mkStringExpr)
                apply(tail, restOfMatrowl.dropIn(finishedExpr), placeholderCount)

              case Work.AlreadyDone(expr)      => apply(tail, matrowl.dropIn(expr), placeholderCount)
              case Work.Token(StringToken(s2)) => apply(tail, matrowl.dropIn(Expr(s2)), placeholderCount)
              case Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag))) =>
                val (liftsLength, liftsExpr, liftChoice) = placeholders(tag.uid, placeholderCount)
                liftChoice match
                  // If it is a list that could be empty, we have to create a branch structure that will expand
                  // both variants of that using the Matrowl nested structure
                  case LiftChoice.ListLift(_) =>
                    val workIfListNotEmpty = Work.Token(stmt"$a $op (") :: Work.AlreadyDone(liftsExpr) :: Work.Token(stmt")") :: Nil
                    val workIfListEmpty = List(Work.Token(emptySetContainsToken(a)))
                    val complete =
                      (workIfListNotEmpty: Expr[String], workIfListEmpty: Expr[String]) =>
                        '{
                          if ($liftsLength != 0) $workIfListNotEmpty else $workIfListEmpty
                        }
                    val work = Work.StackL ::: workIfListEmpty ::: Work.StackL ::: workIfListNotEmpty ::: List(Work.Pop2(complete))
                    // println(s"** Push Two Variants ** - \nWork is: ${work}\nTail is: ${tail}")
                    // We can spliced liftsLength combo even if we're not splicing in the array itself (i.e. in cases)
                    // where we're splicing the empty token. That's fine since when we're splicing the empty token, the
                    // array length is zero.
                    apply(work ::: tail, matrowl, '{ $placeholderCount + $liftsLength })

                  // Otherwise it's just a regular scalar-token expansion
                  case _ =>
                    // println(s"** Push One Variant ** - \nWork is: ${stmt"$a $op ($b)"}\nTail is: ${tail}")
                    apply(Work.Token(stmt"$a $op ($b)") +: tail, matrowl, placeholderCount)

              // The next two variants cannot be a list operation now since that was handled in the
              // Work.Token(SetContainsToken(a, op, b @ ScalarTagToken(tag))) case above
              // They can be set-operations on a lift but not one that can be empty
              case Work.Token(SetContainsToken(a, op, b)) =>
                apply(Work.Token(stmt"$a $op ($b)") +: tail, matrowl, placeholderCount)
              case Work.Token(ScalarTagToken(tag)) =>
                val (liftsLength, liftsExpr, _) = placeholders(tag.uid, placeholderCount)
                apply(tail, matrowl.dropIn(liftsExpr), '{ $placeholderCount + $liftsLength })

              case Work.Token(Statement(tokens)) =>
                apply(tokens.map(Work.Token(_)) ::: tail, matrowl, placeholderCount)
              case Work.Token(_: ScalarLiftToken) =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case Work.Token(_: QuotationTagToken) =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
            }
        }
        apply(List(Work.Token(token)), Matrowl.Bottom, Expr(0))
      }
      token2Expr(statement)
    }
  end Static

  object Dynamic:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp](
        query: Unparticular.Query,
        lifts: List[Planter[_, _, _]],
        liftingPlaceholder: Int => String,
        emptySetContainsToken: Token => Token
    ): String =
      raw(query.realQuery, lifts, liftingPlaceholder, emptySetContainsToken)

    private[getquill] def raw[PrepareRowTemp, Session](statements: Statement, lifts: List[Planter[_, _, _]], liftingPlaceholder: Int => String, emptySetContainsToken: Token => Token): String = {
      enum LiftChoice:
        case ListLift(value: EagerListPlanter[Any, PrepareRowTemp, Session])
        case SingleLift(value: Planter[Any, PrepareRowTemp, Session])

      val listLifts = lifts.collect { case e: EagerListPlanter[_, _, _] => e.asInstanceOf[EagerListPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap
      val singleLifts = lifts.collect { case e: EagerPlanter[_, _, _] => e.asInstanceOf[EagerPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap
      val injectableLifts = lifts.collect { case e: InjectableEagerPlanter[_, _, _] => e.asInstanceOf[InjectableEagerPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap

      def getLifts(uid: String): LiftChoice =
        listLifts.get(uid).map(LiftChoice.ListLift(_))
          .orElse(singleLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .orElse(injectableLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .getOrElse {
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts})")
          }

      // TODO Also need to account for empty tokens but since we actually have a reference to the list can do that directly
      def placeholders(uid: String, initialIndex: Int): (Int, String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder =
              lifts.values.zipWithIndex.map((_, index) => liftingPlaceholder(index + initialIndex)).mkString(", ")
            val liftsLength = lifts.values.length
            (liftsLength, liftsPlaceholder)
          case LiftChoice.SingleLift(lift) =>
            (1, liftingPlaceholder(initialIndex))

      def isEmptyListLift(uid: String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) => lifts.values.isEmpty
          case _                          => false

      def token2String(token: Token): String = {
        @tailrec
        def apply(
            workList: List[Token],
            sqlResult: Seq[String],
            placeholderIndex: Int // I.e. the index of the '?' that is inserted in the query (that represents a lift)
        ): String = workList match {
          case Nil => sqlResult.reverse.foldLeft("")((concatonation, nextExpr) => concatonation + nextExpr)
          case head :: tail =>
            head match {
              case StringToken(s2) => apply(tail, s2 +: sqlResult, placeholderIndex)
              case SetContainsToken(a, op, b) =>
                b match
                  case ScalarTagToken(tag) if isEmptyListLift(tag.uid) =>
                    apply(emptySetContainsToken(a) +: tail, sqlResult, placeholderIndex)
                  case _ =>
                    apply(stmt"$a $op ($b)" +: tail, sqlResult, placeholderIndex)
              case ScalarTagToken(tag) =>
                val (liftsLength, lifts) = placeholders(tag.uid, placeholderIndex)
                apply(tail, lifts +: sqlResult, placeholderIndex + liftsLength)
              case Statement(tokens) => apply(tokens.foldRight(tail)(_ +: _), sqlResult, placeholderIndex)
              case _: ScalarLiftToken =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case _: QuotationTagToken =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
            }
        }
        apply(List(token), Seq(), 0)
      }

      token2String(statements)
    }
  end Dynamic

end Particularize
