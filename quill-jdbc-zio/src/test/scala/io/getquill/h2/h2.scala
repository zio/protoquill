package io.getquill

import io.getquill.context.ZioJdbc.DataSourceLayer
import io.getquill.context.qzio.ImplicitSyntax.Implicit
import zio.Runtime.Scoped
import javax.sql.DataSource

package object h2 {
  implicit val pool: Implicit[Scoped[DataSource]] = zio.Unsafe.unsafe { Implicit(zio.Runtime.unsafe.fromLayer(DataSourceLayer.fromPrefix("testH2DB"))) }
  object testContext extends H2ZioJdbcContext(Literal) with TestEntities
}
