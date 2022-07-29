package io.getquill

import io.getquill.context.qzio.ImplicitSyntax.Implicit
import io.getquill.ZioSpec.runLayerUnsafe
import io.getquill.jdbczio.Quill

package object sqlserver {
  val pool = runLayerUnsafe(Quill.DataSource.fromPrefix("testSqlServerDB"))
  object testContext extends Quill.SqlServer(Literal, pool) with TestEntities
}
