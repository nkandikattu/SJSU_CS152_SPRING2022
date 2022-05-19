package collections

object problem10 extends App {

  def ones(): LazyList[Int] = 1 #:: ones()

  def nonNegInts(n: Int): LazyList[Int] = n #:: nonNegInts(n+1)

  def nonNegEvenInts(n: Int): LazyList[Int] = nonNegInts(n).map(_ * 2)

  def intSquares(n: Int): LazyList[Int] = nonNegInts(n).map(math.pow(_, 2).toInt)

  println("Testing Ones")
  val oneStream = ones()
  println(oneStream(0))   // 1
  println(oneStream(1))   // 1
  println(oneStream(5))   // 1
  println(oneStream(25))  // 1
  println(oneStream)

  println("Testing nonNegInts")
  val posIntStream = nonNegInts(0)
  println(posIntStream(0))    // 0
  println(posIntStream(1))    // 1
  println(posIntStream(5))    // 5
  println(posIntStream(12))   // 12
  println(posIntStream(25))   // 25
  println(posIntStream)

  println("Testing nonNegEvenInts")
  val posEvenIntStream = nonNegEvenInts(0)
  println(posEvenIntStream(0))    // 0
  println(posEvenIntStream(1))    // 2
  println(posEvenIntStream(5))    // 10
  println(posEvenIntStream(12))   // 24
  println(posEvenIntStream(25))   // 50
  println(posEvenIntStream)

  println("Testing intSquares")
  val intSquareStream = intSquares(0)
  println(intSquareStream(0))    // 0
  println(intSquareStream(1))    // 1
  println(intSquareStream(5))    // 25
  println(intSquareStream(12))   // 144
  println(intSquareStream(25))   // 625
  println(intSquareStream)

}
