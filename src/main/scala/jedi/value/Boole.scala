package jedi.value

import jedi.expression.Literal

case class Boole(value: Boolean) extends Literal:
  def &&(other: Value): Boole = Boole(this.value && other.toString.toBoolean)
  def ||(other: Value): Boole = Boole(this.value || other.toString.toBoolean)
  def unary_! = Boole(!this.value)
  override def toString: String = value.toString

object Boole:
  def apply(value: Boolean) = new Boole(value)
  val TRUE = new Boole(true)
  val FALSE = new Boole(false)