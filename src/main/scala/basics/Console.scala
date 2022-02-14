package basics

import scala.io._

class UserError(gripe: String) extends Exception(gripe)

abstract class Console {

  var verbose = false // print stack traces if true


  // override in an extension
  def execute(cmmd: String): String


  def repl: Unit =
    var more = true
    var amt = ""
    while (more) {
      try
        amt = StdIn.readLine("Enter equation: ")
        if (amt == "quit") more = false
        else execute(amt)
      catch
        case e: UserError =>
          println(e.getMessage)
          if (verbose) println(e.printStackTrace)
        case e: Exception =>
          println(e)
          if (verbose) println(e.printStackTrace)
          more = false
//      finally
//        println("Type \"quit\" to quit")
    }
    println("bye")


}

class MathConsole extends Console {
  // your code here
  override def execute(cmmd: String): String =

    var input: Array[String] = cmmd.split("\\s+")
    if (input.length <= 1) return "Provide at least 1 argument"

    for (i <- 1 until(input.length))
      if (!input(i).contains("0123456789"))
        return "Arugments must be numbers"

    var operator = input(0)
    operator match {
      case "add" =>
        var sum: Float = 0
        for (i <- 1 until(input.length))
          sum += input(i).toFloat
        sum.toString
      case "sub" =>
        var result: Float = 0
        for (i <- 1 until(input.length))
          result += input(i).toFloat
        result.toString
      case "mul" =>
        var product: Float = 0
        for (i <- 1 until(input.length))
          product *= input(i).toFloat
        product.toString
      case "div" =>
        var quotient: Float = 0
        for (i <- 1 until(input.length))
          if (input(i).toFloat == 0)
            return "No division by 0"
          quotient *= input(i).toFloat
        quotient.toString
      case _ =>
        "Unrecognized operator:" + operator

    }


}

object MathConsole {
  def main(args: Array[String]): Unit = {
    val cui = MathConsole()
    cui.repl
  }
}


