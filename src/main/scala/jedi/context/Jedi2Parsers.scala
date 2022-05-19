package jedi.context

import scala.util.parsing.combinator._
import jedi.expression._
import jedi.value._

class Jedi2Parsers extends Jedi1Parsers {

  // Notes: ~> or <~ means that you don't have to explicitly reference the preceding symbol in match case

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(id ~ Nil) => List(id)
    case Some(id ~ more) => id::more
    case None => List()
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~> params ~ expression ^^ {
    case param ~ exp => Lambda(param, exp)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case exp ~ Nil => Block(List(exp))
    case exp ~ more => Block(exp::more)
  }


  // override of term parser
  override def term: Parser[Expression] = lambda | funCall | block | literal | "("~>expression<~")"
}
