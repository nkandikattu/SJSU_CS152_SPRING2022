package jedi.expression

import jedi.value.Value
import jedi.context.Environment

trait Literal extends Expression with Value:
  def execute(env: Environment) = this