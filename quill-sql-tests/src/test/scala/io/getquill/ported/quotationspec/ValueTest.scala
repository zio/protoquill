package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill._
import io.getquill.PicklingHelper._

class ValueTest extends Spec with TestEntities {
  "value" - { //helloo
    "null" in {
      inline def s = quote ("s")
      // Need to make "s" an external element because scala3 compiler optimizes `s != null` to `true` on a low level (i.e. even before the AST).
      inline def q = quote(s != null)
      println(quote(unquote(q)).ast)
      quote(unquote(q)).ast.asInstanceOf[BinaryOperation].b mustEqual NullValue
    }
    "constant" in {
      inline def q = quote(11L)
      val v = Constant.auto(11L)
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
    "tuple" - {
      "literal" in {
        inline def q = quote((1, "a"))
        val v = Tuple(List(Constant.auto(1), Constant.auto("a")))
        quote(unquote(q)).ast mustEqual v
        repickle(v) mustEqual v
      }
      // "arrow assoc" - {
      //   // TODO Is this even allowed in dotty?
      //   // "unicode arrow" in {
      //   //   inline def q = quote(1 → "a")
      //   //   quote(unquote(q)).ast mustEqual Tuple(List(Constant.auto(1), Constant.auto("a")))
      //   // }
      "normal arrow" in {
        inline def q = quote(1 -> "a" -> "b")
        val v = Tuple(List(Tuple(List(Constant.auto(1), Constant.auto("a"))), Constant.auto("b")))
        quote(unquote(q)).ast mustEqual v
        repickle(v) mustEqual v
      }
      "explicit `Predef.ArrowAssoc`" in {
        inline def q = quote(Predef.ArrowAssoc("a").->[String]("b"))
        val v = Tuple(List(Constant.auto("a"), Constant.auto("b")))
        quote(unquote(q)).ast mustEqual v
        repickle(v) mustEqual v
      }
      // }
    }
  }
}
