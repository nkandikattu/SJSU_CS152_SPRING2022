package jedi.value

import jedi.context.Environment
import jedi.expression.{Expression, Identifier}

class Closure(parameters: List[Identifier], body: Expression, defEnv: Environment) extends Value:
  def apply(args: List[Value]) =
    val TE = Environment(defEnv)
    TE.bulkPut(parameters, args)
    body.execute(TE)