package io.getquill

import caliban.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban._
import caliban.quick._ 
import zio.http._
import zio.http.Server
import zio.{ExitCode, ZIO}
import io.getquill._
import io.getquill.context.qzio.ImplicitSyntax._
import io.getquill.context.ZioJdbc._
import io.getquill.util.LoadConfig
import zio.Console.printLine
import zio.{ ZIOApp, ExitCode, URIO, Task }
import java.io.Closeable
import javax.sql.DataSource

import scala.language.postfixOps
import caliban.execution.Field
import caliban.schema.ArgBuilder
import io.getquill.CalibanIntegration._
import io.getquill.util.ContextLogger
import io.getquill
import io.getquill.FlatSchema._
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import zio.json.JsonEncoder
import zio.json.JsonDecoder
import caliban._

object Dao {
  case class PersonAddressPlanQuery(plan: String, pa: List[PersonAddress])
  private val logger = ContextLogger(classOf[Dao.type])

  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val ds = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  given Implicit[DataSource] = Implicit(ds)

  inline def q(inline columns: List[String], inline filters: Map[String, String]) =
    quote {
      query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
        .map((p, a) => PersonAddress(p.id, p.first, p.last, p.age, a.map(_.street)))
        .filterColumns(columns)
        .filterByKeys(filters)
        .take(10)
    }
  inline def plan(inline columns: List[String], inline filters: Map[String, String]) =
    quote { sql"EXPLAIN ${q(columns, filters)}".pure.as[Query[String]] }

  def personAddress(columns: List[String], filters: Map[String, String]) = {
    println(s"Getting columns: $columns")
    run(q(columns, filters)).implicitDS.mapError(e => {
      logger.underlying.error("personAddress query failed", e)
      e
    })
  }

  def personAddressPlan(columns: List[String], filters: Map[String, String]) =
    run(plan(columns, filters), OuterSelectWrap.Never).map(_.mkString("\n")).implicitDS.mapError(e => {
      logger.underlying.error("personAddressPlan query failed", e)
      e
    })

  def resetDatabase() =
    (for {
      _ <- run(sql"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
      _ <- run(liftQuery(ExampleData.people).foreach(row => query[PersonT].insertValue(row)))
      _ <- run(liftQuery(ExampleData.addresses).foreach(row => query[AddressT].insertValue(row)))
    } yield ()).implicitDS
} // end Dao

object CalibanExample extends zio.ZIOAppDefault {

  case class Queries(
      personAddress: Field => (ProductArgs[PersonAddress] => Task[List[PersonAddress]]),
      personAddressPlan: Field => (ProductArgs[PersonAddress] => Task[Dao.PersonAddressPlanQuery])
  )

  val endpoints =
     graphQL(
      RootResolver(
        Queries(
          personAddress =>
            (productArgs =>
              Dao.personAddress(quillColumns(personAddress), productArgs.keyValues)
            ),
          personAddressPlan =>
            (productArgs => {
              val cols = quillColumns(personAddressPlan)
              (Dao.personAddressPlan(cols, productArgs.keyValues) zip Dao.personAddress(cols, productArgs.keyValues)).map(
                (pa, plan) => Dao.PersonAddressPlanQuery(pa, plan)
              )
            })
        )
      )
    )

  val myApp = for {
    _ <- Dao.resetDatabase()
    - <- endpoints.runServer(
            port = 8088,
            apiPath = "/api/graphql",
            graphiqlPath = Some("/graphiql")
    )
  } yield ()

  override def run =
    myApp.exitCode

} // end CalibanExample