package jedi.valTests

import jedi.value.{Exact, Inexact, Boole, Chars}
import jedi.context.alu
import jedi.expression.Identifier

@main def testNumbers() =

  val life = Exact(42)
  val two = Exact(2)
  val pi = Inexact(3.14)
  val e = Inexact(2.718)
  val twotoo = Inexact(2.0)

  println(two * life) // 84
  println(-life) // -42
  println(-pi) // -3.14
  println(e * pi) // 8.53452
  println(pi * two) // 6.28
  println(two * pi) // 6.28
  println(two - pi) // -1.14
  println(two / pi) // 0.6369426751592356
  println(life / two) // 21
  println(two < life) // true
  println(e < two) // false
  println(e > two) // true
  println(two == twotoo) // true

@main def testBooles() =
  val x = Boole.TRUE
  val y = Boole.TRUE
  val z = Boole.FALSE
  println(x == y) // true
  println(x == z) // false
  println(!Boole.TRUE || Boole.FALSE || Boole.TRUE) // true
  println(!(Boole.FALSE || Boole.TRUE) || Boole.TRUE) // true
  println(Boole.TRUE && !(Boole.FALSE && Boole.TRUE)) // true

@main def testChars() =
  val mars = Chars("Mars")
  val jupiter = Chars("Jupiter")
  val hello = Chars("Hello, ")
  val result = Chars("result = ")
  println(hello + mars) // Hello, Mars
  println(hello + jupiter) // Hello, Jupiter
  println(result + Exact(42)) // result = 42
  println(result + Inexact(42.0)) // result = 42.0
  val two = Exact(2)
  println(jupiter.subChars(two, Exact(5))) // pit
  println(jupiter.subChars(Exact(0), Exact(3))) // Jup
  println(jupiter < mars) // true
  println(Chars("baby") < Chars("amazing")) // false
  println(Chars("Mars") == mars) // true
  println(Chars("Mars") == jupiter) // false

@main def aluTests() =
  println(alu.execute(Identifier("add"), List(Exact(3), Exact(9), Inexact(2.0)))) // 14.0
  println(alu.execute(Identifier("mul"), List(Exact(3), Exact(9), Inexact(2.0)))) // 54.0
  println(alu.execute(Identifier("sub"), List(Exact(3), Exact(9), Inexact(2.0)))) // -8.0
  println(alu.execute(Identifier("div"), List(Inexact(32.0), Exact(2), Exact(2)))) // 8.0
  println(alu.execute(Identifier("add"), List(Chars("result = "), Exact(42)))) // result = 42


  println(alu.execute(Identifier("less"), List(Chars("cat"), Chars("bat")))) // false
  println(alu.execute(Identifier("more"), List(Chars("cat"), Chars("bat")))) // true

  println(alu.execute(Identifier("less"), List(Exact(2), Exact(5)))) // true
  println(alu.execute(Identifier("more"), List(Exact(2), Exact(5)))) // false
  println(alu.execute(Identifier("less"), List(Exact(2), Inexact(5.0)))) // true
  println(alu.execute(Identifier("more"), List(Exact(2), Inexact(5.0)))) // false
  println(alu.execute(Identifier("less"), List(Chars("cat"), Inexact(5.0)))) // false
  println(alu.execute(Identifier("more"), List(Chars("cat"), Inexact(5.0)))) // true

  println(alu.execute(Identifier("write"), List(Chars("cat"), Chars("bat"))))
//  println(alu.execute(Identifier("div"), List(Inexact(32.0), Exact(2), Exact(0)))) // Exception: Can't divide by 0
//  println(alu.execute(Identifier("div"), List(Inexact(32.0), Exact(0), Exact(2)))) // Exception: Can't divide by 0
  println(alu.execute(Identifier("not"), List(Boole.FALSE))) 
  println(alu.execute(Identifier("equals"), List(Exact(2), Inexact(2.0))))
  println(alu.execute(Identifier("unequals"), List(Exact(2), Inexact(3.0))))


