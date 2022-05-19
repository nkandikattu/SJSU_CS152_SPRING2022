package collections

object pipes {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    elems.filter(_ % 2 != 0).map(math.pow(_,3)).reduce(_+_).toInt
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    lists.map(_.reduce(_+_)).reduce(_+_)
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    vals.filter(test(_)).length
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    vals.filter(test(_)).length.>=(1)
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    vals.filter(test(_)).length.>=(vals.length)
}

// iterative implementations
object iters {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    var sum = 0
    elems.foreach(
      a => if(a % 2 != 0) sum = sum + math.pow(a, 3).toInt
    )
    sum
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    var sum = 0.0
    lists.foreach(
      x => x.foreach(
        y => sum = sum + y
      )
    )
    sum
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    var numPass = 0
    vals.foreach(x => if(test(x)) numPass = numPass + 1)
    numPass
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    var numPass = 0
    for(value <- vals)
      if(test(value)) numPass = numPass + 1
    numPass >= 1
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    for(value <- vals)
      if(!test(value)) return false
    true
}

// tail recursive implementations
object tails {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    def helper(list: List[Int], result: Int): Int =
      if(list == Nil) result
      else if(list.head % 2 != 0) helper(list.tail, result + math.pow(list.head, 3).toInt)
      else helper(list.tail, result)
    helper(elems, 0)
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    def helper(l: List[List[Double]]) : Double =
      def helper2(l2: List[Double]): Double =
        if (l2 == Nil) 0
        else l2.head + helper2(l2.tail)
      if (l == Nil) 0
      else helper2(l.head) + helper(l.tail)
    helper(lists)
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    def helper(list: List[T], result: Int): Int =
      if(list == Nil) result
      else if(test(list.head)) helper(list.tail, result+1)
      else helper(list.tail, result)
    helper(vals, 0)
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    if(vals == Nil) false
    else if(test(vals.head)) true
    else somePass(vals.tail, test)
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    if(vals == Nil) true
    else test(vals.head) && allPass(vals.tail, test)
}

// classic recursive implementations (i.e., not tail recursive)
object recur {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    if(elems == Nil) 0
    else if(elems.head % 2 != 0) math.pow(elems.head, 3).toInt + socs(elems.tail)
    else socs(elems.tail)
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    def helper(list: List[Double]): Double =
      if (list == Nil) 0
      else list.head + helper(list.tail)
    if (lists == Nil) 0
    else helper(lists.head) + sos(lists.tail)
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    if(vals == Nil) 0
    else if(test(vals.head)) 1 + countPass(vals.tail, test)
    else countPass(vals.tail, test)
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    if(vals == Nil) false
    else test(vals.head) || somePass(vals.tail, test)
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    def helper(list: List[T], testFunc: Boolean): Boolean =
      if(list == Nil) testFunc
      else helper(list.tail, testFunc && test(list.head))
    helper(vals, true)
}

object ListProcs extends App {

  println("Testing pipelines")
  println("" + pipes.socs(List(1, 2, 3))) // 28
  println("" + pipes.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + pipes.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + pipes.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + pipes.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing iterations")
  println("" + iters.socs(List(1, 2, 3))) // 28
  println("" + iters.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + iters.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + iters.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + iters.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing recursions")
  println("" + recur.socs(List(1, 2, 3))) // 28
  println("" + recur.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + recur.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + recur.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + recur.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing tail-recursions")
  println("" + tails.socs(List(1, 2, 3))) // 28
  println("" + tails.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + tails.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + tails.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + tails.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false


}