package io.getquill

import caliban.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.RootResolver
import zio.{ExitCode, ZIO}
import io.getquill.*
import io.getquill.context.qzio.ImplicitSyntax.*
import io.getquill.context.ZioJdbc.*
import io.getquill.util.LoadConfig
import zio.Console.printLine
import zio.{ExitCode, Task, URIO, ZIOApp}

import java.io.Closeable
import javax.sql.DataSource
import scala.language.postfixOps
import caliban.execution.Field
import caliban.schema.ArgBuilder
import io.getquill.CalibanIntegration.*
import io.getquill.util.ContextLogger
import io.getquill.NestedSchema.*
import caliban.schema.Schema.auto.*
import caliban.schema.ArgBuilder.auto.*
import zio.json.JsonEncoder
import zio.json.JsonDecoder
import caliban._
import caliban.quick._ 


object DaoNested {
  case class PersonAddressPlanQuery(plan: String, pa: List[PersonAddressNested])
  private val logger = ContextLogger(classOf[DaoNested.type])

  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val ds = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  given Implicit[DataSource] = Implicit(ds)

  inline def q(inline columns: List[String], inline filters: Map[String, String]) =
    quote {
      query[PersonT].leftJoin(query[AddressT]).on((p, a) => p.id == a.ownerId)
        .map((p, a) => PersonAddressNested(p.id, p.name, p.age, a.map(_.street)))
        .filterByKeys(filters)
        .filterColumns(columns)
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
} // end DaoNested

object CalibanExampleNested extends zio.ZIOAppDefault {
  private val logger = ContextLogger(classOf[CalibanExampleNested.type])

  case class Queries(
      personAddress: Field => (ProductArgs[PersonAddressNested] => Task[List[PersonAddressNested]]),
      personAddressPlan: Field => (ProductArgs[PersonAddressNested] => Task[DaoNested.PersonAddressPlanQuery])
  )

  val endpoints =
     graphQL(
      RootResolver(
        Queries(
          personAddress =>
            (productArgs =>
              DaoNested.personAddress(quillColumns(personAddress), productArgs.keyValues)
            ),
          personAddressPlan =>
            (productArgs => {
              val cols = quillColumns(personAddressPlan)
              logger.underlying.info(s"Selected Columns: ${cols}")
              (DaoNested.personAddressPlan(cols, productArgs.keyValues) zip DaoNested.personAddress(cols, productArgs.keyValues)).map(
                (pa, plan) => DaoNested.PersonAddressPlanQuery(pa, plan)
              )
            })
        )
      )
    )

  val myApp = for {
    _ <- DaoNested.resetDatabase()
    _ <- endpoints.runServer(
          port = 8088,
          apiPath = "/api/graphql",
          graphiqlPath = Some("/graphiql")
      )
  } yield ()

  override def run =
    myApp.exitCode

} // end CalibanExampleNested