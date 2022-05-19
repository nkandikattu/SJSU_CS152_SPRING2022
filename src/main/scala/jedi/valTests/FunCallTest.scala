package jedi.valTests

import jedi.expression._
import jedi.context._
import jedi.value._


object FunCallTest extends App {
  val globalEnvironment = new Environment
  val operands = List(Exact(6), Exact(7))
  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment)) // 13
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment)) // true
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment)) // 42
}
