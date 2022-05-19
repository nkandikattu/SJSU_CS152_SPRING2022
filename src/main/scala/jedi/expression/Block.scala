package jedi.expression

import jedi.value.Value
import jedi.context.Environment

case class Block(expressions: List[Expression]) extends SpecialForm:
  def execute(env: Environment): Value =
    val tempEnv = new Environment(env)
    val results: List[Value] = expressions.map(exp => exp.execute(tempEnv))
    results.last