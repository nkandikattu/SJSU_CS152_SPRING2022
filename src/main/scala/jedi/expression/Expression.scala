package jedi.expression

import jedi.value.Value
import jedi.context.Environment

trait Expression:
  def execute(env: Environment): Value