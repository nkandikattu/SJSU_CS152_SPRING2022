package jedi.expression

import jedi.context.Environment
import jedi.value.Notification

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm:
  def execute(env: Environment) =
    env.put(identifier, expression.execute(env))
    Notification.OK

