package recursion
import scala.annotation.tailrec

object Fibonacci extends App {

  // non-tail recursive version
  def fib(n: Int): Int = if (n == 0 || n == 1) 1 else fib(n-1) + fib(n-2)

  // tail recursive version
  def tailFib(n: Int): Int =
    @tailrec 
    def helper(a: Int, b: Int, n: Int): Int =
      if (n >= 0) helper(b, a+b, n-1) else a
    helper(1, 1, n)

  // the first 11 Fibonaccis
  for(i <- 0 to 10)
    println(s"fib(${i}) = ${fib(i)}")
    println(s"tailFib(${i}) = ${tailFib(i)}")

}

/*
output:

fib(0) = 1
tailFib(0) = 1
fib(1) = 1
tailFib(1) = 2
fib(2) = 2
tailFib(2) = 3
fib(3) = 3
tailFib(3) = 5
fib(4) = 5
tailFib(4) = 8
fib(5) = 8
tailFib(5) = 13
fib(6) = 13
tailFib(6) = 21
fib(7) = 21
tailFib(7) = 34
fib(8) = 34
tailFib(8) = 55
fib(9) = 55
tailFib(9) = 89
fib(10) = 89
tailFib(10) = 144
*/