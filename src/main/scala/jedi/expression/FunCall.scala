package jedi.expression

import jedi.context.{alu, Environment}
import jedi.value.Closure

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression:
  def execute(env: Environment) =
    val args = operands.map(_.execute(env))
    if(env.contains(operator) && env(operator).isInstanceOf[Closure])
      val closure = env.apply(operator).asInstanceOf[Closure]
      closure.apply(args)
    else alu.execute(operator, args)