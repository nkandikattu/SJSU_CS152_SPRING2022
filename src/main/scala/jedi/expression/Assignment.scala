package jedi.expression

import jedi.context.{Environment, TypeException}
import jedi.value.{Notification, Value, Variable}

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm:
  def execute(env: Environment): Value =
    val v = vbl.execute(env)
    v match
      case v: Variable => 
        v.content = update.execute(env)
        Notification.DONE
      case _ => throw new TypeException("Cannot assign non-variable")
