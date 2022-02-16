package recursion

import scala.annotation.tailrec

class Base {
  def inc(x: BigInt): BigInt = x + 1
  def dec(x: BigInt): BigInt = x - 1
  def isZero(x: BigInt): Boolean = x == 0
}

class Hyper extends Base {

  def add(n: BigInt, m: BigInt): BigInt =
    var sum = n
    for (i <- 0 until m.intValue) sum = inc(sum)
    sum

  def mul(n: BigInt, m: BigInt): BigInt =
    var product = add(n,0)
    for (i <- 1 until m.intValue) product = add(n, product)
    product

  def exp(n: BigInt): BigInt =
    var result = mul(2,1)
    for (i <- 1 until(n.intValue)) result = mul(2, result)
    result

  def hyperExp(n: BigInt): BigInt =
    var result = exp(2)
    for (i <- 1 until(n.intValue)) result = exp(result)
    result

  // etc.
}

class TailHyper extends Base {

  final def add(n: BigInt, m: BigInt): BigInt =
    var sum = n
    @tailrec
    def helper(count: Int, result: BigInt): BigInt =
      if (m < count) result else helper(count + 1, inc(result))
    helper(1, sum)

  def mul(n: BigInt, m: BigInt): BigInt =
    var product = 0
    @tailrec
    def helper(count: Int, result: BigInt): BigInt =
      if (m < count) result else helper(count + 1, add(n, result))
    helper(1, product)

  def exp(n: BigInt): BigInt =
    var product = 1
    @tailrec
    def helper(count: Int, result: BigInt): BigInt =
      if (n < count) result else helper(count + 1, mul(2, result))
    helper(1, product)

  def hyperExp(n: BigInt): BigInt =
    var product = 2
    @tailrec
    def helper(count: Int, result: BigInt): BigInt =
      if (n < count) result else helper(count + 1, exp(result))
    helper(1, product)

  // etc.
}

object HyperTest extends TailHyper with App {

  println("exp(10) = " + exp(10))           // 1024
  println("hyperExp(2) = " + hyperExp(2))   // 16
  println("hyperExp(3) = " + hyperExp(3))   // 65536
  println("hyperExp(4) = " + hyperExp(4))   // still waiting

}