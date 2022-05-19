package jedi.expression

import jedi.context.*
import jedi.value.Boole

import scala.language.postfixOps

case class Disjunction(operands: List[Expression]) extends SpecialForm:
  def execute(env: Environment) =
    if(operands.length < 2) throw new Exception("Not enough operands")
    // executes operands left to right until one is true, otherwise false
    // args must be booles, otherwise throw TypeException
    var result = false
    operands.foreach(op =>
      val arg = op.execute(env)
      if(!arg.isInstanceOf[Boole]) throw new TypeException("Arguments to || must be of type Boole")
      result = arg.toString.toBoolean || result
      if(result) return Boole(result)
    )
    Boole(result)
