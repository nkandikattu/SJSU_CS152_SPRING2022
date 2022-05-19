package collections

object tester extends App {
  def numFail[T, S](pairs: List[(T, S)], f: T => S): Int =
    pairs.filter((x, y) => f(x) != y).length

  val squares = List((0, 0), (2, 4), (3, 9), (4, 16), (5, 25))
  def square(n: Int) = n + n
  println(numFail(squares, square)) // = 3
}
