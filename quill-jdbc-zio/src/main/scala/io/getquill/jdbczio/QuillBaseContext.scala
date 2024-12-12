package io.getquill.jdbczio

import io.getquill._
import io.getquill.context.{ ContextVerbStream, ExecutionInfo, ProtoContextSecundus }
import io.getquill.context.jdbc.JdbcContextTypes
import io.getquill.context.qzio.{ ZioContext, ZioJdbcContext, ZioTranslateContext }
import io.getquill.context.sql.idiom.SqlIdiom
import zio.{ ZEnvironment, ZIO }
import zio.stream.ZStream

import java.sql.{ Connection, PreparedStatement, ResultSet, SQLException }
import javax.sql.DataSource
import scala.util.Try
import scala.annotation.targetName

trait QuillBaseContext[+Dialect <: SqlIdiom, +Naming <: NamingStrategy] extends ZioContext[Dialect, Naming]
  with JdbcContextTypes[Dialect, Naming]
  with ProtoContextSecundus[Dialect, Naming]
  with ContextVerbStream[Dialect, Naming]
  with ZioTranslateContext[Dialect, Naming] {

  def ds: DataSource

  override type StreamResult[T] = ZStream[Environment, Error, T]
  override type Result[T] = ZIO[Environment, Error, T]
  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Long
  override type RunActionReturningResult[T] = T
  override type RunBatchActionResult = List[Long]
  override type RunBatchActionReturningResult[T] = List[T]

  // Needed for TranslateContext in ProtoQuill
  override type Runner = Unit
  override type TranslateRunner = Unit
  override protected def context: Runner = ()
  def translateContext: TranslateRunner = ()

  override type Error = SQLException
  override type Environment = Any
  override type PrepareRow = PreparedStatement
  override type ResultRow = ResultSet

  override type TranslateResult[T] = ZIO[Environment, Error, T]
  override type Session = Connection

  final lazy val underlying: ZioJdbcContext[Dialect, Naming] = dsDelegate
  private[getquill] val dsDelegate: ZioJdbcContext[Dialect, Naming]

  override def close() = ()

  // No Probing in Dotty yet
  //override def probe(sql: String): Try[_] = dsDelegate.probe(sql)

  @targetName("runQueryDefault")
  inline def run[T](inline quoted: Quoted[Query[T]]): ZIO[Any, SQLException, List[T]] = InternalApi.runQueryDefault(quoted)
  @targetName("runQuery")
  inline def run[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): ZIO[Any, SQLException, List[T]] = InternalApi.runQuery(quoted, wrap)
  @targetName("runQuerySingle")
  inline def run[T](inline quoted: Quoted[T]): ZIO[Any, SQLException, T] = InternalApi.runQuerySingle(quoted)
  @targetName("runAction")
  inline def run[E](inline quoted: Quoted[Action[E]]): ZIO[Any, SQLException, Long] = InternalApi.runAction(quoted)
  @targetName("runActionReturning")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, T]]): ZIO[Any, SQLException, T] = InternalApi.runActionReturning[E, T](quoted)
  @targetName("runActionReturningMany")
  inline def run[E, T](inline quoted: Quoted[ActionReturning[E, List[T]]]): ZIO[Any, SQLException, List[T]] = InternalApi.runActionReturningMany[E, T](quoted)
  @targetName("runBatchAction")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): ZIO[Any, SQLException, List[Long]] = InternalApi.runBatchAction(quoted, rowsPerBatch)
  @targetName("runBatchActionDefault")
  inline def run[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]]): ZIO[Any, SQLException, List[Long]] = InternalApi.runBatchAction(quoted, 1)
  @targetName("runBatchActionReturning")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): ZIO[Any, SQLException, List[T]] = InternalApi.runBatchActionReturning(quoted, rowsPerBatch)
  @targetName("runBatchActionReturningDefault")
  inline def run[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]]): ZIO[Any, SQLException, List[T]] = InternalApi.runBatchActionReturning(quoted, 1)

  def executeAction(sql: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, Long] =
    onDS(dsDelegate.executeAction(sql, prepare)(info, dc))

  def executeQuery[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, List[T]] =
    onDS(dsDelegate.executeQuery[T](sql, prepare, extractor)(info, dc))

  override def executeQuerySingle[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, T] =
    onDS(dsDelegate.executeQuerySingle[T](sql, prepare, extractor)(info, dc))

  override def translateQueryEndpoint[T](statement: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor, prettyPrint: Boolean = false)(executionInfo: ExecutionInfo, dc: Runner): TranslateResult[String] =
    onDS(dsDelegate.translateQueryEndpoint[T](statement, prepare, extractor, prettyPrint)(executionInfo, dc))

  override def translateBatchQueryEndpoint(groups: List[BatchGroup], prettyPrint: Boolean = false)(executionInfo: ExecutionInfo, dc: Runner): TranslateResult[List[String]] =
    onDS(dsDelegate.translateBatchQueryEndpoint(groups.asInstanceOf[List[QuillBaseContext.this.dsDelegate.BatchGroup]], prettyPrint)(executionInfo, dc))

  def streamQuery[T](fetchSize: Option[Int], sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: Runner): ZStream[Any, SQLException, T] =
    onDSStream(dsDelegate.streamQuery[T](fetchSize, sql, prepare, extractor)(info, dc))

  def executeActionReturning[O](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[O], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, O] =
    onDS(dsDelegate.executeActionReturning[O](sql, prepare, extractor, returningBehavior)(info, dc))

  def executeActionReturningMany[O](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[O], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, List[O]] =
    onDS(dsDelegate.executeActionReturningMany[O](sql, prepare, extractor, returningBehavior)(info, dc))

  def executeBatchAction(groups: List[BatchGroup])(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, List[Long]] =
    onDS(dsDelegate.executeBatchAction(groups.asInstanceOf[List[QuillBaseContext.this.dsDelegate.BatchGroup]])(info, dc))

  def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(info: ExecutionInfo, dc: Runner): ZIO[Any, SQLException, List[T]] =
    onDS(dsDelegate.executeBatchActionReturning[T](groups.asInstanceOf[List[QuillBaseContext.this.dsDelegate.BatchGroupReturning]], extractor)(info, dc))

  // Used in translation functions
  private[getquill] def prepareParams(statement: String, prepare: Prepare): ZIO[Any, SQLException, Seq[String]] =
    onDS(dsDelegate.prepareParams(statement, prepare))

  /**
   * Execute instructions in a transaction. For example, to add a Person row to the database and return
   * the contents of the Person table immediately after that:
   * {{{
   *   val a = run(query[Person].insert(Person(...)): ZIO[DataSource, SQLException, Long]
   *   val b = run(query[Person]): ZIO[DataSource, SQLException, Person]
   *   transaction(a *> b): ZIO[DataSource, SQLException, Person]
   * }}}
   */
  def transaction[R, E ,A](op: ZIO[R, E, A]): ZIO[R, E | SQLException, A] =
    dsDelegate.transaction(op).provideSomeEnvironment[R]((env: ZEnvironment[R]) => env.add[DataSource](ds: DataSource))

  private def onDS[T, E](qio: ZIO[DataSource, E, T]): ZIO[Any, E, T] =
    qio.provideEnvironment(ZEnvironment(ds))

  private def onDSStream[T, E](qstream: ZStream[DataSource, E, T]): ZStream[Any, E, T] =
    qstream.provideEnvironment(ZEnvironment(ds))
}
