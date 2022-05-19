package jedi.expression

import jedi.context.{Environment, TypeException, BreakException}
import jedi.value.{Boole, Notification, Value}

case class Iteration(condition: Expression, body: Expression) extends SpecialForm:
  def execute(env: Environment): Value =
    var c1 = condition.execute(env)
    if (!c1.isInstanceOf[Boole]) throw new TypeException("While condition must be Boole")
    var c2 = c1.asInstanceOf[Boole]
    var break = false
    while (c2.value  && !break)
      try
        body.execute(env)
        c1 = condition.execute(env).asInstanceOf[Boole]
        if (!c1.isInstanceOf[Boole]) throw new TypeException("While condition must be Boole")
        c2 = c1.asInstanceOf[Boole]
      catch
        case e: BreakException => break = true
    Notification.DONE