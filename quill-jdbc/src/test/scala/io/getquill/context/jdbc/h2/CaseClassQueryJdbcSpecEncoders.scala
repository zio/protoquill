package io.getquill.context.jdbc.h2

import io.getquill.context.sql.CaseClassQuerySpec
import org.scalatest.matchers.should.Matchers.*
import io.getquill.*

import io.getquill.generic.{DecodingType, GenericDecoder}

class CaseClassQueryJdbcSpecEncoders extends CaseClassQuerySpec with H2JdbcContext.Codec {

  val context = testContext
  import testContext._

  override given contactDecoder: GenericDecoder[ResultRow, Session, Contact, DecodingType.Composite] = deriveComposite
  override given addressDecoder: GenericDecoder[ResultRow, Session, Address, DecodingType.Composite] = deriveComposite
  override given nicknameDecoder: GenericDecoder[ResultRow, Session, Nickname, DecodingType.Composite] = deriveComposite
  override given nicknameSameFieldDecoder: GenericDecoder[ResultRow, Session, NicknameSameField, DecodingType.Composite] = deriveComposite

  override def beforeAll() = {
    testContext.transaction {
      testContext.run(query[Contact].delete)
      testContext.run(query[Address].delete)
      testContext.run(liftQuery(peopleEntries).foreach(p => peopleInsert(p)))
      testContext.run(liftQuery(addressEntries).foreach(p => addressInsert(p)))
    }
    ()
  }

  "Example 1 - Single Case Class Mapping" in {
    testContext.run(`Ex 1 CaseClass Record Output`) should contain theSameElementsAs `Ex 1 CaseClass Record Output expected result`
  }
  "Example 1A - Single Case Class Mapping" in {
    testContext.run(`Ex 1A CaseClass Record Output`) should contain theSameElementsAs `Ex 1 CaseClass Record Output expected result`
  }
  "Example 1B - Single Case Class Mapping" in {
    testContext.run(`Ex 1B CaseClass Record Output`) should contain theSameElementsAs `Ex 1 CaseClass Record Output expected result`
  }

  "Example 2 - Single Record Mapped Join" in {
    testContext.run(`Ex 2 Single-Record Join`) should contain theSameElementsAs `Ex 2 Single-Record Join expected result`
  }

  "Example 3 - Inline Record as Filter" in {
    testContext.run(`Ex 3 Inline Record Usage`) should contain theSameElementsAs `Ex 3 Inline Record Usage exepected result`
  }

  "Example 4 - Ex 4 Mapped Union of Nicknames" in {
    testContext.run(`Ex 4 Mapped Union of Nicknames`) should contain theSameElementsAs `Ex 4 Mapped Union of Nicknames expected result`
  }

  "Example 4 - Ex 4 Mapped Union All of Nicknames" in {
    testContext.run(`Ex 4 Mapped Union All of Nicknames`) should contain theSameElementsAs `Ex 4 Mapped Union All of Nicknames expected result`
  }

  "Example 4 - Ex 4 Mapped Union All of Nicknames Filtered" in {
    testContext.run(`Ex 4 Mapped Union All of Nicknames Filtered`) should contain theSameElementsAs `Ex 4 Mapped Union All of Nicknames Filtered expected result`
  }

  "Example 4 - Ex 4 Mapped Union All of Nicknames Same Field" in {
    testContext.run(`Ex 4 Mapped Union All of Nicknames Same Field`) should contain theSameElementsAs `Ex 4 Mapped Union All of Nicknames Same Field expected result`
  }

  "Example 4 - Ex 4 Mapped Union All of Nicknames Same Field Filtered" in {
    testContext.run(`Ex 4 Mapped Union All of Nicknames Same Field Filtered`) should contain theSameElementsAs `Ex 4 Mapped Union All of Nicknames Same Field Filtered expected result`
  }
}
