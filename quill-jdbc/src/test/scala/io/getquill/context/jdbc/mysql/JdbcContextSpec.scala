package io.getquill.context.jdbc.mysql

import io.getquill.Spec
import io.getquill._

class JdbcContextSpec extends Spec with TestEntities {

  val context = testContext
  import testContext._

  "probes sqls" in {
    inline def p = testContext.probe("DELETE FROM TestEntity")
  }

  "run non-batched action" in {
    testContext.run(qr1.delete)
    inline def insert = quote {
      qr1.insert(_.i -> lift(1))
    }
    testContext.run(insert) mustEqual 1
  }

  "provides transaction support" - {
    "success" in {
      testContext.run(qr1.delete)
      testContext.transaction {
        testContext.run(qr1.insert(_.i -> 33))
      }
      testContext.run(qr1).map(_.i) mustEqual List(33)
    }
    "failure" in {
      testContext.run(qr1.delete)
      intercept[IllegalStateException] {
        testContext.transaction {
          testContext.run(qr1.insert(_.i -> 33))
          throw new IllegalStateException
        }
      }
      testContext.run(qr1).isEmpty mustEqual true
    }
    "nested" in {
      testContext.run(qr1.delete)
      testContext.transaction {
        testContext.transaction {
          testContext.run(qr1.insert(_.i -> 33))
        }
      }
      testContext.run(qr1).map(_.i) mustEqual List(33)
    }
    // Prepare is not implemented yet
    // "prepare" in {
    //   testContext.prepareParams(
    //     "select * from Person where name=? and age > ?", ps => (List("Sarah", 127), ps)
    //   ) mustEqual List("127", "'Sarah'")
    // }
  }

  "Insert with returning with single column table" in {
    // NOTE: if you make this an inline def the 2nd call and 1st call will run simultaneously
    // since the value will be inlined into the mustBe ___ call. That means both calls will
    // be run simultaneosly. This will cause mysql to block.
    val inserted = testContext.run {
      qr4.insertValue(lift(TestEntity4(0))).returningGenerated(_.i)
    }
    testContext.run(qr4.filter(_.i == lift(inserted))).head.i mustBe inserted
  }
}
