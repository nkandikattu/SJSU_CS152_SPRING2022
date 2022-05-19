package jedi.expression

import jedi.context.Environment
import jedi.value.{Boole, Notification}


case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends Expression:
  def execute(env: Environment) =
    if(condition.execute(env) == Boole.TRUE)
      consequent.execute(env)
    else
      if(alternative.!=(null)) alternative.execute(env)
      else Notification.UNSPECIFIED