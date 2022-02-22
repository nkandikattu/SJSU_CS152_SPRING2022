package combinators

class Lab3 {
  // problem 1

  def compose[A,B,C](f: B=>C, g: A=>B): A => C =
    def r(x: A): C = f(g(x))
    r _


  // problem 2

  def id[T](x: T) = x
  def selfIter[T](f: T => T, n: Int): T => T =
    if (n == 1) f
    else compose(f, selfIter(f, n-1))


  // problem 3

  def countPass[T](elems: Array[T], test: T => Boolean): Int =
    var count = 0
    for(i <- 0 until(elems.length))
      if(test(elems(i)))
        count += 1
    count


  // problem 5

  def makeIter(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int =
    def result(n: Int): Int =
        if (n == 0) baseVal else combiner(n, result(n-1))
    result _

  // problem 6

  def deOptionize[T, S](f: T => Option[S]): T => S =
    def g(n: T): S =
      if (f(n) == None) throw new Exception("Invalid input: " + n)
      else
        val f_res: Option[S] = f(n)
        val res = f_res.getOrElse(0).asInstanceOf[S]
        res
    g _


}



object lab3Tests extends Lab3 with App {

  println("testing compose")
  val firstDigit = compose((s: String) => s(0), (n: Int) => n.toString)
  println(firstDigit(12345))  // = '1'
  println(firstDigit(99999))  // = '9'

  println("Testing selfIter")
  def inc(x: Double) = x + 1
  def double(x: Double) = 2 * x
  val inc4 = selfIter(inc, 4)
  val pow4 = selfIter(double, 4)

  println(inc4(7)) // = 11
  println(pow4(1)) // = 16

  println("testing countPass")
  println(countPass(Array(1, 2, 3, 4, 5, 6, 7), (n: Int) => n % 2 == 0)) // = 3

  println("testing makeIter")
  val tri = makeIter(0, _ + _)
  val fact = makeIter(1, _ * _)

  println(tri(5))  // = 15
  println(fact(5)) // = 120

  println("testing deOptionize")
  def parseDigits(digits: String): Option[Int] =
    if (digits.matches("[0-9]+")) Some(digits.toInt) else None

  val parseDigits2 = deOptionize(parseDigits)

  try {
    println(parseDigits2("12345")) // = 12345
    println(parseDigits2("12x45")) // throws exception
  } catch {
    case e: Exception => println(e.getMessage)
  }

}

/*
testing compose
1
9
Testing selfIter
11.0
16.0
testing countPass
3
testing makeIter
15
120
testing deOptionize
12345
Invalid input: 12x45
*/