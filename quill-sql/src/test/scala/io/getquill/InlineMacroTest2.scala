// Creating an inline fold. Once val parser is implemented this shuold be possible
// package io.getquill

//
// import scala.language.implicitConversions
//

// object InlineMacroTest2 {

//   inline def fold[R](inline curr: R, inline list: List[R], inline op: (R, R) => R): R = {
//     inline if (ListFlicer.isNil(list))
//       curr
//     else
//       fold(op(curr, ListFlicer.index(list, 0)), ListFlicer.tail(list), op)
//   }

//   //inline def addl(inline str1:String, inline str2: String) = str1 + str2

//   def main(args: Array[String]): Unit = {

//
//     case class Address(street: String, zip: Int) extends Embedded
//     given Embedable[Address]
//     case class Person(id: Int, firstName: String, age: Int, addr: Address, middleName: String, lastName: String)

//     inline def q = quote {
//       query[Person].map(p => fold("", List(p.firstName, p.middleName, p.lastName), (str1:String, str2:String) => str1 + str2))
//     }

//     val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//     import ctx._

//     val output = run(q)
//     println(output)
//   }
// }
