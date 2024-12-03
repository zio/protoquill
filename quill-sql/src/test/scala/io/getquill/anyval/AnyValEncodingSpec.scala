package io.getquill.anyval

import io.getquill.*
import io.getquill.MirrorContext.Codec.*

case class Blah(value: String, value2: Int)
case class Rec(value: Blah, otherValue: String)
case class Name(value: String) extends AnyVal

class AnyValEncodingSpec extends MirrorSpec {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  case class Person(name: Name, age: Int)
  given CompositeDecoder[Person] = deriveComposite

  "simple anyval should encode and decode" in {
    // val id = Rec(Blah("Joe", 123), "Bloggs")
    val name = Name("Joe")
    val mirror = ctx.run(query[Person].filter(p => p.name == lift(name)))
    println(mirror)
  }
}
