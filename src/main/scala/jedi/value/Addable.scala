package jedi.value

import jedi.expression.Literal

trait Addable extends Literal:
  def +(other: Value): Addable
