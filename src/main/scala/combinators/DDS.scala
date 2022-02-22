package combinators

class DDS {

  // problem 1
  final def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
    if (halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)

  // problem 2
  val population = controlLoop(1, 0,
    (pop: Int, cycle: Int) => pop >= math.pow(10,5),
    (pop: Int, cycle: Int) => pop * 2)

  // problem 3
  def solve(f: Double=> Double): Double =
    val delta = 1e-7
    def df(x: Double) = (f(x + delta) - f(x))/delta
    def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
    controlLoop(1.0, 0, goodEnough, improve)


  // problem 4
  def sqrt(x: Double) = solve((f: Double)  => f * f - x)

  // problem 5
  def cubeRoot(x: Double) = solve((f: Double)  => f * f * f - x)

  // problem 6
  def nthRoot(x: Double, n: Int) = solve((f: Double)  => math.pow(f, n) - x)

  // problem 7

  // = value of an investment of $principle at an annual rate r compounded
  // periods times over 1 year
  def value(principle: Double, rate: Double, periods: Int): Double =
    principle * math.pow(1+((rate/100)/periods), periods)


}

object TestDDS extends DDS with App {
  println(population)   // 131072
  println(sqrt(49))     // 7
  println(sqrt(36))     // 6
  println(sqrt(81))     // 9
  println(cubeRoot(12167))  // 23
  println(nthRoot(15625, 6)) // 5
  // assuming that Annual Rate is given in x%
  println(value(10000, 2.5, 12)) // 10252.88

  // etc.

}