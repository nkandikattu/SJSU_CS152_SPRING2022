package jedi.expression

import jedi.context.Environment
import jedi.value.{Value, Closure}

case class Lambda(parameters: List[Identifier], body: Expression) extends SpecialForm:
  def execute(env: Environment): Value =
    new Closure(parameters, body, env)
