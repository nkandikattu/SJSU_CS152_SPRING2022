package oop

trait Instruction:
  def execute: Double

object Accumulator:
  var register: Double = 0
  var program: List[Instruction] = List[Instruction]()
  def run() =
    register = 0
    program.foreach(x => register = x.execute)

class Add(arg: Double) extends Instruction:
  def execute: Double = Accumulator.register + arg

class Mul(arg: Double) extends Instruction:
  def execute: Double = Accumulator.register * arg

class Rep(num: Int, body: Instruction) extends Instruction:
  def execute: Double =
    var total: Double = Accumulator.register
    for (i <- 0 until num)
      if(body.isInstanceOf[Add]) total += body.execute
      else if(body.isInstanceOf[Mul]) total *= body.execute
    total

object testAccumulator extends App:
  // Problem 1
  // computing 3 * 4 + 9
  Accumulator.program = List(Add(3), Mul(4), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  println("register = " + Accumulator.register)
  // computing (((10 * 2) + 3) * 5)
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  println("register = " + Accumulator.register)


  // Problem 3
  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 42