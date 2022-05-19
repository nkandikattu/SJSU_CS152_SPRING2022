package jedi.expression
import jedi.context.{Environment, BreakException}
import jedi.value.{Notification, Value}

case class Break() extends SpecialForm:
  def execute(env: Environment) =
    throw new BreakException()
    new Notification("break")
