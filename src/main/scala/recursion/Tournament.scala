package recursion

object Tournament extends App {

  // prob A wins if A needs n wins and B needs m
  def probability(n: Int, m: Int): Double =
    n match
      case n if (n < 0 || m < 0) => throw new Exception("Parameters must be non-negative")
      case n if (n == m) => 0.5
      case n if (n == 0 && m > 0) => 1
      case n if (n > 0 && m == 0) => 0
      case _ =>
        0.5 * (probability(n-1,m) + probability(n,m-1))


  // Team A sweeps the World Series
  for(i <- 4 to 0 by -1)
    println("probability A wins = " + probability(i, 4))

}

/*
Output

probability A wins = 0.5
probability A wins = 0.65625
probability A wins = 0.8125
probability A wins = 0.9375
probability A wins = 1.0

*/