package io.getquill.context.jdbc.mysql

import io.getquill.context.sql.DepartmentsSpec
import io.getquill.*
import io.getquill.context.jdbc.{JdbcSpecEncoders, MysqlJdbcSpecEncoders}

class DepartmentsJdbcSpecEncoders extends DepartmentsSpec with MysqlJdbcSpecEncoders {

  val context = testContext
  import testContext._

  override def beforeAll() = {
    testContext.transaction {
      testContext.run(query[Department].delete)
      testContext.run(query[Employee].delete)
      testContext.run(query[Task].delete)

      testContext.run(liftQuery(departmentEntries).foreach(p => departmentInsert(p)))
      testContext.run(liftQuery(employeeEntries).foreach(p => employeeInsert(p)))
      testContext.run(liftQuery(taskEntries).foreach(p => taskInsert(p)))
    }
    ()
  }

  "Example 8 - nested naive" in {
    testContext.run(`Example 8 expertise naive`(lift(`Example 8 param`))) mustEqual `Example 8 expected result`
  }

  "Example 9 - nested db" in {
    testContext.run(`Example 9 expertise`(lift(`Example 9 param`))) mustEqual `Example 9 expected result`
  }
}