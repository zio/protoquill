package io.getquill.context.jdbc

import io.getquill.{NamingStrategy, ReturnAction}
import io.getquill.ReturnAction.{ReturnColumns, ReturnNothing, ReturnRecord}
import io.getquill.context.{Context, ExecutionInfo}
import io.getquill.context.sql.SqlContext
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.util.ContextLogger

import java.sql.{Connection, JDBCType, PreparedStatement, ResultSet, Statement}
import java.util.TimeZone

trait JdbcContextTypes {

  type Index = Int
  type PrepareRow = PreparedStatement
  type ResultRow = ResultSet
  type Session = Connection
  type Runner = Unit

  protected val dateTimeZone = TimeZone.getDefault

  /**
   * Parses instances of java.sql.Types to string form so it can be used in creation of sql arrays.
   * Some databases does not support each of generic types, hence it's welcome to override this method
   * and provide alternatives to non-existent types.
   *
   * @param intType one of java.sql.Types
   * @return JDBC type in string form
   */
  protected def parseJdbcType(intType: Int): String = JDBCType.valueOf(intType).getName
}