package jedi.value

class Notification(value: String) extends Value:
  override def toString = value

object Notification:
  def apply(value: String) = new Notification(value)
  val OK = new Notification("OK")
  val DONE = new Notification("DONE")
  val UNSPECIFIED = new Notification("UNSPECIFIED")