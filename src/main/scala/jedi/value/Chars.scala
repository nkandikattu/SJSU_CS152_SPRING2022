package jedi.value

case class Chars(value: String) extends Addable with Ordered[Value]:

  def +(other: Value): Addable = Chars(this.value.concat(other.toString))

  def size: Exact = Exact(value.length)

  def subChars(to: Exact, from: Exact): Chars =
    Chars(value.substring(to.toString.toInt, from.toString.toInt))

  override def toString: String = value

  override def compare(other: Value): Int =
    other match
      case other if this.value > other.toString => 1
      case other if this.value < other.toString => -1
      case _ => 0

