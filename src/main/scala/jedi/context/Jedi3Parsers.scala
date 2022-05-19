package jedi.context

import scala.util.parsing.combinator._
import jedi.expression._
import jedi.value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ ":=" ~ expression ^^ {
    case id ~ ":=" ~ exp => Assignment(id, exp)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~> "(" ~> expression ~ ")" ~ expression ^^ {
    case cond ~ ")" ~ body => Iteration(cond, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case exp => FunCall(Identifier("dereference"), List(exp))
  }

  def break: Parser[Break] = "break" ^^ {
    case "break" => Break()
  }


  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block |  assignment | dereference | break | literal | "("~>expression<~")"

}