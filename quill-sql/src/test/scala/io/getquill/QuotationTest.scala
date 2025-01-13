package io.getquill

import scala.language.implicitConversions
import io.getquill.QuotationLot

import io.getquill.ast.{Query => AQuery, _}
import io.getquill.ast.{Property => Prop}
import io.getquill.Quoted
import io.getquill.Planter
import io.getquill.EagerPlanter
import io.getquill.LazyPlanter
import io.getquill.QuotationVase
import io.getquill.QuotationLot
import io.getquill.generic.GenericEncoder
import io.getquill.context.mirror.Row
import io.getquill.context.ExecutionType
import io.getquill.quat.quatOf
import io.getquill.quat.Quat
import io.getquill.quote
import io.getquill.query
import io.getquill.context.mirror.MirrorSession

import org.scalatest._

class QuotationTest extends Spec with Inside {
  case class Address(street: String, zip: Int) extends Embedded
  case class Person(name: String, age: Int, address: Address)
  import ShortAst._

  object scalarTag {
    def apply(uid: String) = ScalarTag(uid, External.Source.Parser)
    def unapply(ast: Ast) =
      ast match {
        case ScalarTag(uid, _) => Some(uid)
        case _                 => None
      }
  }
  val IdentP = Ident("p", quatOf[Person])
  val PersonQuat = quatOf[Person].probit

  "compiletime quotation has correct ast for" - {
    "trivial whole-record select" in {
      inline def q = quote { query[Person] }
      q.ast mustEqual Entity("Person", List(), quatOf[Person].probit)
    }
    "single field mapping" in {
      inline def q = quote { query[Person].map(p => p.name) }
      q.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name"))
    }
    "anonymous single field mapping" in {
      inline def q = quote { query[Person].map(_.name) }
      q.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), Ident("x1", quatOf[Person]), Property(Ident("x1", quatOf[Person]), "name"))
    }
    "splice into another quotation without quote" in {
      inline def q = query[Person]
      inline def qq = quote { q.map(p => p.name) }
      qq.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name"))
    }
    "unquoted splice into another quotation" in {
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      qq.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name"))
    }
    "double unquoted splice into another quotation" in {
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      inline def qqq = quote { qq.map(s => s) }
      qq.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name"))
      qqq.ast mustEqual Map(Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name")), Ident("s"), Ident("s"))
    }
    "double splice into another quotation, middle not quoted" in {
      inline def q = quote { query[Person] }
      inline def qq = q.map(p => p.name)
      inline def qqq = quote { qq.map(s => s) }
      qq.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name"))
      qqq.ast mustEqual Map(Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name")), Ident("s", quatOf[Person]), Ident("s", quatOf[Person]))
    }
    "double unquoted splict with a lift" in {
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      qq.ast mustEqual Map(Entity("Person", List(), quatOf[Person].probit), IdentP, Property(IdentP, "name"))

      val ctx = new MirrorContext(MirrorSqlDialect, Literal) // We only need a context to do lifts
      import ctx._
      inline def qqq = quote { qq.map(s => s + lift("hello")) }
    }
    "query with a lazy lift" in {
      inline def q = quote { lazyLift("hello") }
      q must matchPattern {
        case Quoted(scalarTag(tagUid), List(LazyPlanter("hello", vaseUid)), List()) if (tagUid == vaseUid) =>
      }
    }
    "run lazy lift" in {
      case class Person(name: String)
      inline def q = quote { query[Person].filter(p => p.name == lazyLift("Joe")) }
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      ctx.run(q).triple mustEqual (
        "SELECT p.name FROM Person p WHERE p.name = ?", List("Joe"), ExecutionType.Static
      )
    }
    "two level query with a lift and plus operator" in {
      case class Address(street: String, zip: Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      qq.ast mustEqual Map(Entity("Person", List(), PersonQuat), IdentP, Property(IdentP, "name"))

      // We only need a context to do lifts
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._
      inline def qqq = quote { qq.map(s => s + lift("hello")) }
      qqq must matchPattern {
        case Quoted(
              Map(Map(Ent("Person"), IdentP, Property(Id("p"), "name")), Id("s"), Id("s") `(+)` scalarTag(tagUid)),
              List(EagerPlanter("hello", encoder, planterUid)), // Compare encoders by ref since all mirror encoders are same case class
              Nil
            ) if (tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }
    }
    "two level query with a two lifts and plus operator" in {
      case class Address(street: String, zip: Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      inline def q = quote { query[Person] }

      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      inline def qq = quote { q.map(p => p.name + lift("how")) }
      qq must matchPattern {
        case Quoted(
              Map(Entity("Person", List(), `PersonQuat`), IdentP, Property(IdentP, "name") `(+)` scalarTag(tuid)),
              List(EagerPlanter("how", enc, puid)),
              Nil
            ) if (tuid == puid) =>
      }

      inline def qqq = quote { qq.map(s => s + lift("are you")) }
      qqq must matchPattern {
        case Quoted(
              Map(Map(Ent("Person"), IdentP, Property(IdentP, "name") `(+)` scalarTag(tuid1)), Id("s"), Id("s") `(+)` scalarTag(tuid2)),
              List(EagerPlanter("how", enc1, puid1), EagerPlanter("are you", enc2, puid2)),
              Nil
            ) if (tuid1 == puid1 && tuid2 == puid2 && enc1.eq(summon[Encoder[String]])) =>
      }
    }
    "three level query with a lazy-lift/eager-lift/lazy-lift, and plus operator" in {
      case class Address(street: String, zip: Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      inline def q = quote { query[Person] }
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      inline def qq = quote { q.map(p => p.name + lazyLift("hello")) }
      qq must matchPattern {
        case Quoted(
              Map(Entity("Person", List(), `PersonQuat`), IdentP, Property(IdentP, "name") `(+)` scalarTag(uid)),
              List(LazyPlanter("hello", planterUid)),
              Nil
            ) if (uid == planterUid) =>
      }

      inline def qqq = quote { qq.map(s => s + lift("how") + lazyLift("are you")) } // hellooooooo
      qqq must matchPattern {
        case Quoted(
              Map(Map(Ent("Person"), IdentP, Property(IdentP, "name") `(+)` scalarTag(tuid1)), Id("s"), Id("s") `(+)` scalarTag(tuid2) `(+)` scalarTag(tuid3)),
              List(LazyPlanter("hello", puid1), EagerPlanter("how", encoder, puid2), LazyPlanter("are you", puid3)),
              Nil
            ) if (tuid1 == puid1 && tuid2 == puid2 && tuid3 == puid3 && encoder.eq(summon[Encoder[String]])) =>
      }
    }
  }

  "runtime quotation has correct ast for" - {
    "simple one-level query with map" in {
      val q = quote { query[Person].map(p => p.name) }
      q.ast mustEqual Map(Entity("Person", List(), `PersonQuat`), IdentP, Property(IdentP, "name"))
    }
    "two-level query with map" in {
      val q = quote { query[Person] }
      val qq = quote { q.map(p => p.name) }
      qq must matchPattern {
        case Quoted(
              Map(QuotationTag(tagId), IdentP, Property(IdentP, "name")),
              List(),
              List(QuotationVase(Quoted(Entity("Person", List(), `PersonQuat`), List(), List()), vaseId))
            ) if (vaseId == tagId) =>
      }
    }

    "lift" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      val q = quote { lift("hello") }
      q must matchPattern {
        case Quoted(scalarTag(tagUid), List(EagerPlanter("hello", encoder, vaseUid)), List()) if (tagUid == vaseUid) =>
      }
      List(Row("hello")) mustEqual q.encodeEagerLifts(Row(), MirrorSession.default)
    }

    "spliced lift" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      val q = quote { lift("hello") }
      val qq = quote { q }
      qq must matchPattern {
        case Quoted(
              QuotationTag(quotationTagId),
              Nil,
              List(QuotationVase(Quoted(scalarTag(scalarTagId), List(EagerPlanter("hello", encoder, planterId)), Nil), quotationVaseId))
            ) if (quotationTagId == quotationVaseId && scalarTagId == planterId && encoder.eq(summon[Encoder[String]])) =>
      }
      List(Row("hello")) mustEqual q.encodeEagerLifts(Row(), MirrorSession.default)
    }
    "query with a lift and plus operator" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      inline def q = quote { query[Person].map(p => p.name + lift("hello")) }
      q must matchPattern {
        case Quoted(
              Map(Ent("Person"), IdentP, Property(IdentP, "name") `(+)` scalarTag(tagUid)),
              List(EagerPlanter("hello", encoder, planterUid)),
              Nil
            ) if (tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }
    }
    "two-level query with a lift and plus operator" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      val q = quote { query[Person] }
      val qq = quote { q.map(p => p.name + lift("hello")) }
      qq must matchPattern {
        case Quoted(
              Map(QuotationTag(tid), IdentP, Property(IdentP, "name") `(+)` scalarTag(tagUid)),
              List(EagerPlanter("hello", encoder, planterUid)),
              List(QuotationVase(Quoted(Ent("Person"), Nil, Nil), vid))
            ) if (tid == vid && tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }
      ctx.pull(qq) mustEqual (List("hello"), ExecutionType.Dynamic)
    }
    "three level val query with a two lifts and plus operator" in {
      case class Address(street: String, zip: Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      val q = quote { query[Person] }

      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      val qq = quote { q.map(p => p.name + lift("how")) }
      qq must matchPattern {
        case Quoted(
              Map(QuotationTag(qid), IdentP, Property(IdentP, "name") `(+)` scalarTag(tuid)),
              List(EagerPlanter("how", enc, puid)),
              List(QuotationVase(Quoted(Ent("Person"), Nil, Nil), vid))
            ) if (tuid == puid && qid == vid) =>
      }

      val qqq = quote { qq.map(s => s + lift("are you")) }
      val quat = quatOf[Person]
      qqq must matchPattern {
        case Quoted(
              Map(QuotationTag(qid2), Id("s"), Id("s") `(+)` scalarTag(tid2)),
              List(EagerPlanter("are you", enc2, pid2)),
              List(QuotationVase(
                Quoted(
                  Map(QuotationTag(qid), Id("p"), Property(Id("p"), "name") `(+)` scalarTag(tid)),
                  List(EagerPlanter("how", enc, pid)),
                  List(QuotationVase(Quoted(Ent("Person"), Nil, Nil), vid))
                ),
                vid2
              ))
            ) if (tid == pid && qid == vid && tid2 == pid2 && qid2 == vid2) =>
      }
      // if this below tests fails, line error is 259 on scalatest, report as a bug? reproduce?
      // [info]   (List(how),Dynamic) did not equal (List(how, are you),Dynamic) (QuotationTest.scala:259)
      ctx.pull(qqq) mustEqual (List("how", "are you"), ExecutionType.Dynamic)
    }
  }

  "mixed compile-time and runtime queries" - {
    // TODO Make a test of this case but with an eager lazy lift that shuold fail?
    "runtime -> compile-time" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      val q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name + lift("hello")) }
      qq must matchPattern {
        case Quoted(
              Map(QuotationTag(tid), `IdentP`, Property(`IdentP`, "name") `(+)` scalarTag(tagUid)),
              List(EagerPlanter("hello", encoder, planterUid)),
              List(QuotationVase(Quoted(Ent("Person"), Nil, Nil), vid))
            ) if (tid == vid && tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }
      val r = ctx.run(qq)
      ctx.pull(qq) mustEqual (List("hello"), ExecutionType.Dynamic)
    }

    "compile-time -> runtime" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      inline def q = quote { query[Person] }
      val qq = quote { q.map(p => p.name + lift("hello")) }
      qq must matchPattern {
        case Quoted(
              Map(Ent("Person"), IdentP, Property(IdentP, "name") `(+)` scalarTag(tagUid)),
              List(EagerPlanter("hello", encoder, planterUid)),
              Nil
            ) if (tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }

      val r = ctx.run(qq)
      ctx.pull(qq) mustEqual (List("hello"), ExecutionType.Dynamic)
    }

    "compile-time -> runtime -> compile-time" in {
      case class Address(street: String, zip: Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      inline def q = quote { query[Person] }

      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      val qq = quote { q.map(p => p.name + lift("how")) }
      qq must matchPattern {
        case Quoted(
              Map(Ent("Person"), IdentP, Property(IdentP, "name") `(+)` scalarTag(tuid)),
              List(EagerPlanter("how", enc, puid)),
              Nil
            ) if (tuid == puid) =>
      }

      inline def qqq = quote { qq.map(s => s + lift("are you")) }
      // Should not match this pattern, should be spliced directly from the inline def
      qqq must matchPattern {
        case Quoted(
              Map(QuotationTag(qid2), Id("s"), Id("s") `(+)` scalarTag(tid2)),
              List(EagerPlanter("are you", enc2, pid2)),
              List(QuotationVase(
                Quoted(
                  Map(Ent("Person"), IdentP, Property(IdentP, "name") `(+)` scalarTag(tid)),
                  List(EagerPlanter("how", enc1, pid)),
                  Nil
                ),
                vid2
              ))
            ) if (pid == tid && tid2 == pid2 && qid2 == vid2) =>
      }
      ctx.pull(qqq) mustEqual (List("how", "are you"), ExecutionType.Dynamic)
    }

    "runtime -> compile-time -> runtime" in {
      case class Address(street: String, zip: Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      val q = quote { query[Person] }

      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      inline def qq = quote { q.map(p => p.name + lift("how")) }
      qq must matchPattern {
        case Quoted(
              Map(QuotationTag(qid), IdentP, Property(IdentP, "name") `(+)` scalarTag(tuid)),
              List(EagerPlanter("how", enc, puid)),
              List(QuotationVase(Quoted(Ent("Person"), Nil, Nil), vid))
            ) if (tuid == puid && qid == vid) =>
      }

      val qqq = quote { qq.map(s => s + lift("are you")) }
      // Should not match this pattern, should be spliced directly from the inline def
      qqq must matchPattern {
        case Quoted(
              Map(QuotationTag(qid2), Id("s"), Id("s") `(+)` scalarTag(tid2)),
              List(EagerPlanter("how", enc, pid), EagerPlanter("are you", enc2, pid2)),
              List(QuotationVase(
                Quoted(
                  Map(QuotationTag(qid), IdentP, Property(IdentP, "name") `(+)` scalarTag(tid)),
                  List(EagerPlanter("how", enc1, pid1)),
                  List(QuotationVase(Quoted(Ent("Person"), Nil, Nil), vid))
                ),
                vid2
              ))
            ) if (tid == pid && qid == vid && pid1 == pid && tid2 == pid2 && qid2 == vid2) =>
      }
      ctx.pull(qqq) mustEqual (List("how", "are you"), ExecutionType.Dynamic)
    }
  }

  "special cases" - {
    "lazy lift shuold crash dynamic query" in {
      case class Person(name: String, age: Int)
      val q = quote { query[Person].map(p => p.name + lazyLift("hello")) }

      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      assertThrows[IllegalArgumentException] { ctx.run(q) }
    }

    "pull quote from unavailable context - only inlines - with map" in {
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      class Outer {
        inline def qqq = new Inner().qq.map(s => s + lift("are you"))
        class Inner {
          inline def qq = new Core().q.map(p => p.name + lift("how"))
          class Core {
            inline def q = query[Person]
          }
        }
      }
      inline def q = quote { new Outer().qqq }
      ctx.run(q).triple mustEqual (
        "SELECT (p.name || ?) || ? FROM Person p", List("how", "are you"), ExecutionType.Static
      )
    }

    "pull quote from unavailable context - only inlines - with map and lazy lift" in {
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      class Outer {
        inline def qqq = new Inner().qq.map(s => s + lift("are you"))
        class Inner {
          inline def qq = new Core().q.map(p => p.name + lazyLift("how"))
          class Core {
            inline def q = query[Person]
          }
        }
      }
      inline def q = quote { new Outer().qqq }
      ctx.run(q).triple mustEqual (
        "SELECT (p.name || ?) || ? FROM Person p", List("how", "are you"), ExecutionType.Static
      )
    }

    "pull quote from unavailable context - only inlines" in {
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._

      class Outer {
        inline def qqq = quote { new Inner().qq }
        class Inner {
          inline def qq = quote { new Core().q }
          class Core {
            inline def q = quote { query[Person] }
          }
        }
      }
      inline def qry = quote { new Outer().qqq }
      ctx.run(qry).triple mustEqual (
        "SELECT x.name, x.age, x.street, x.zip FROM Person x", List(), ExecutionType.Static
      )
    }
  }
}

