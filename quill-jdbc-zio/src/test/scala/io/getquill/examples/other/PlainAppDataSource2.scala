package io.getquill.examples.other

import com.zaxxer.hikari.{ HikariConfig, HikariDataSource }
import io.getquill.util.LoadConfig
import io.getquill.{ JdbcContextConfig, Literal, PostgresZioJdbcContext }
import zio.Console.printLine
import zio.{ Runtime, Unsafe, Task, ZLayer }
import javax.sql.DataSource
import io.getquill._
import zio.ZIO

object PlainAppDataSource2 {

  object MyPostgresContext extends PostgresZioJdbcContext(Literal)
  import MyPostgresContext._

  case class Person(name: String, age: Int)

  def hikariConfig = new HikariConfig(JdbcContextConfig(LoadConfig("testPostgresDB")).configProperties)
  def hikariDataSource = new HikariDataSource(hikariConfig)

  val zioDS: ZLayer[Any, Throwable, DataSource] =
    ZLayer(ZIO.attempt(hikariDataSource))

  def main(args: Array[String]): Unit = {
    val people = quote {
      query[Person].filter(p => p.name == "Alex")
    }
    val qzio =
      MyPostgresContext.run(people)
        .tap(result => printLine(result.toString))
        .provide(zioDS)

    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(qzio)
    }
    ()
  }
}
