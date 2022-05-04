package io.getquill

import scala.language.implicitConversions
//
//import io.getquill.QueryDsl._
//import io.getquill.SchemaMeta
//import io.getquill.QueryMeta
//import io.getquill.InsertMeta
import io.getquill.util.debug.PrintMac

object MiniQuillTest {

  val ctx = new MirrorContext(PostgresDialect, Literal) // helloooooooo
  import ctx._

  def main(args: Array[String]): Unit = {

    // case class Person(id: Int, name: String, age: Int)
    // case class Address(street: String, zip: Int, personId: Int)

    // case class PersonName(name: String)

    // implicit inline def qm: QueryMeta[PersonName, String] = {
    //     queryMeta[PersonName, String](
    //       quote {
    //         (q: Query[PersonName]) => q.map(p => p.name)
    //       }
    //     )((name: String) => PersonName(name))
    //   }

    // val q = quote {
    //   query[PersonName]
    // }

    case class Person(name: String, age: Int)

    inline def q = quote {
      query[Person].map(_.age).sum
    }
    println(ctx.run(q))

    // inline def q = quote {
    //   query[Person].filter(p => p.name == lift("joe")) //helooo
    // }
    // inline def result = run(q)
    // println( result.string(true) )
    // println( result.prepareRow.data.toList )

    // inline def q1 = quote {
    //   query[Person].join(query[Address]).on((p, a) => p.id == a.personId)
    // }
    // inline def result1 = run(q1)
    // println( result1.string(true) )
    // println( result1.prepareRow.data.toList )

    // Need to test them here as well as above the class def
    // case class Name(first: String, last: String) extends Embedded
    // case class Person(id: Int, name: Name)
    // case class Contact(f: String, l: String) //hello

    // inline def qq = query[Person].map(p => Contact(p.name.first, p.name.last))
    // io.getquill.parser.PrintMac(qq)

    // inline def q = quote {
    //   qq
    // }
    // println( run(q) )

    // {
    //   case class Age(value: Int) extends Embedded
    //   case class Person(name: String, age: Age)
    //   inline def q = quote {
    //     query[Person].insert(Person("Joe", Age(123)))
    //   }
    //   println(q.ast)
    //   run(q)
    // }

    // Moved this out to the testing area due to https://github.com/lampepfl/dotty/issues/10880
    // Could try moving it back to main codebase later if needed.
    // {
    //   case class Age(value: Int) extends Embedded
    //   case class Person(name: String, age: Option[Age])

    //   inline def q = quote {
    //     //query[Person].insert(_.name -> "joe")
    //     query[Person].insert(Person("Joe", Option(Age(123)))) //hello
    //   }
    //   println(q.ast)
    //   run(q)
    // }

    // Slightly different then above case, should test this too
    // {
    //   case class Age(value: Option[Int]) extends Embedded
    //   case class Person(name: String, age: Option[Age])

    //   // When using implicit val
    //   // implicit val personSchema: EntityQuery[Person] = querySchema[Person]("tblPerson", _.name -> "colName")
    //   inline given personSchema: SchemaMeta[Person] =
    //     schemaMeta[Person]("tblPerson", _.name -> "colName", _.age.map(_.value) -> "colValue")
    //   PrintMac(personSchema)

    //   inline def q = quote {
    //     //query[Person].insert(_.name -> "joe")
    //     query[Person].insert(Person("Joe", Option(Age(Option(123))))) //helloooooooooooooooooooooooooooooooooo
    //   }
    //   println(q.ast)
    //   run(q)
    // }

    // Test regular insert with schema
    // Test insert with entity
    // Test insert with entity with optionals
    // Test Insert with schema and entity and optionls

    // Test this
    // {
    //   case class Age(value: Int) extends Embedded
    //   case class Person(name: String, age: Option[Age])

    //   // When using implicit val
    //   // implicit val personSchema: EntityQuery[Person] = querySchema[Person]("tblPerson", _.name -> "colName")
    //   inline given personSchema: SchemaMeta[Person] =
    //     schemaMeta[Person]("tblPerson", _.name -> "colName", _.age.map(_.value) -> "colValue")
    //   PrintMac(personSchema)

    //   inline def q = quote {
    //     //query[Person].insert(_.name -> "joe")
    //     query[Person].insert(Person("Joe", Option(Age(123)))) //helloooooooooooooooooooooooooo
    //   }
    //   println(q.ast)
    //   run(q)
    // }

    // {
    //   case class Person(name: String, age: Int)
    //   inline def q = quote {
    //     query[Person].insert(_.name -> "Joe", _.age -> 123) //hello
    //   }
    //   println(run(q))
    // }

    // ============ With Insert Meta ============
    // {
    //   case class Person(id: Int, name: String)
    //   inline given personSchema: InsertMeta[Person] = insertMeta[Person](_.id)
    //   //PrintMac(personSchema)
    //   // TODO What if this is a val?
    //   inline def q = quote {
    //     query[Person].insert(Person(1, "Joe")) //hello
    //   }
    //   //PrintMac(q)
    //   println( run(q) ) // hello
    // }

    // TODO Exclude a column (via InsertMeta) from Optional object (i.e and multiple excludes)
    // TODO Exclude a column (via InsertMeta)from Insert meta with Insert Schema (i.e and multiple excludes)
    // TODO Exclude a column (via InsertMeta)from Optional object Insert meta with Insert Schema (i.e and multiple excludes)

    // println(q.ast)
    // println( run(q) )

    // hello
    // ============================ Testing Insert Returning =============================
    // {
    //   case class Person(id: Int, name: String, age: Int)
    //   inline def q = quote { query[Person].insertValue(lift(Person(0, "Joe", 123))).returningGenerated(r => (r.name, r.id)) }
    //   println( run(q) )
    // }

    // hellooooooooooooo

    infixAndLiftQuery()
  }

  def infixAndLiftQuery() = {

    // hellooooooooooooooo

  }

  // def liftSetExamples() = {
  //   case class Person(name: String, age: Int)
  //   val names = List("Joe", "Jack")
  //   inline def q = quote {
  //     query[Person].filter(p => liftQuery(names).contains(p.name))
  //   }

  //   println(run(q))
  // }

  // def liftSetExamplesDynamic() = {
  //   case class Person(name: String, age: Int)
  //   val names = List("Joe", "Jack")
  //   val q = quote {
  //     query[Person].filter(p => liftQuery(names).contains(p.name))
  //   }

  //   println(run(q))
  // }

  // def insertExamples() = {
  //   case class Person(name: String, age: Int)
  //   // inline def a = quote { query[Person].insert(_.name -> "Joe", _.age -> 123) } // Insert "assignment form"
  //   // inline def q = quote { query[Person].insert(Person("Joe", 123)) }            // Insert entity form
  //   // inline given personMeta: InsertMeta[Person] = insertMeta[Person](_.age)
  //   // PrintMac(personMeta)
  //   // println(ctx.run(q).string)
  //   // println(ctx.run(q).string == ("INSERT INTO Person (name) VALUES ('Joe')"))
  //   // println(ctx.run(a).string == ("INSERT INTO Person (name,age) VALUES ('Joe', 123)"))

  //   inline def q = quote { (v: Person) =>
  //     query[Person].insertValue(v)
  //   }

  //   // TODO Make sure that 'v' is not used as an identifier in the insert macro, rather a new id is found
  //   val v = Person("Joe", 123)

  //   //PrintMac(lift(p))
  //   println(io.getquill.util.Messages.qprint(quote { lift(v) } )) //helloooooooooooooooooooo

  //   inline def applied = quote { q(lift(v)) }
  //   println(io.getquill.util.Messages.qprint(applied))
  //   //PrintMac(applied)
  //   println(run(applied).string)
  // }
}
