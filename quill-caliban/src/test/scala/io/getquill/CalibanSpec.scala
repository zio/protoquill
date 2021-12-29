package io.getquill

import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import zio.{ZIO, Task}
import caliban.GraphQL
import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.util.ContextLogger

trait CalibanSpec extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val zioDS = DataSourceLayer.fromPrefix("testPostgresDB")

  private val logger = ContextLogger(this.getClass)

  // FlatSchema and NestedSchema share the same DB data so only need to create it using one of them
  override def beforeAll() = {
    import FlatSchema._
    (for {
        _ <- Ctx.run(infix"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
        _ <- Ctx.run(liftQuery(ExampleData.people).foreach(row => query[PersonT].insert(row)))
        _ <- Ctx.run(liftQuery(ExampleData.addresses).foreach(row => query[AddressT].insert(row)))
      } yield ()
    ).provideLayer(zioDS).unsafeRunSync()
  }

  // override def afterAll() = {
  //   import FlatSchema._
  //   Ctx.run(infix"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]]).provideLayer(zioDS).unsafeRunSync()
  // }

  def api: GraphQL[Any]

  extension [R, E, A](qzio: ZIO[Any, Throwable, A])
    def unsafeRunSync(): A = zio.Runtime.default.unsafeRun(qzio)

  def unsafeRunQuery(queryString: String) =
    val output =
      (for {
        interpreter <- api.interpreter
        result      <- interpreter.execute(queryString)
      } yield (result)
      ).tapError{ e =>
        fail("GraphQL Validation Error", e)
        ZIO.unit
      }.unsafeRunSync()

    if (output.errors.length != 0)
      fail(s"GraphQL Validation Failures: ${output.errors}")
    else
      output.data.toString
  end unsafeRunQuery
}