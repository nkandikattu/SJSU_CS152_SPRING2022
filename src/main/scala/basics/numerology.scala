package basics

object numerology extends App {

  // problem 1
  def kingdom(n: Int): Int =
    n match
      case n if n > 10 =>
        if (n % 2 == 0)
          if (n % 100 == 0)
            2
          else 1
        else 4
      case n if n <= 10 => 3
      case _ => 4


  // testing
  println(s"kingdom(11) = ${kingdom(11)}")   // 4
  println(s"kingdom(16) = ${kingdom(16)}")   // 1
  println(s"kingdom(5) = ${kingdom(5)}")     // 3
  println(s"kingdom(200) = ${kingdom(200)}") // 2



  // problem 3

  // original version
  def species(n: Int) =
    if (0 < n) if (n % 2 == 0) 1 else 2

  // corrected version
  def species2(n: Int) =
    if (n > 0)
      if (n % 2 == 0)
        1
      else 2
    else 2
  // The original version is incorrect because there is no species
  // number assigned for negative values of n.
  // Example: if n = -5, in the original version, it would not pass
  // the first if statement, but no species value is assigned either.
  // Because of this error, no matter the input, species() will never
  // work for any value, regardless of if it passes the two if-statements.



  // problem 4

  // odd positives are realm 1
  def realm1(n: Int): Int =
    if (n > 0)
      if (n % 2 != 0)
        1
      else throw new Exception("Not odd")
    else throw new Exception("Not positive")


  // even positives not divisible by 3 are realm 2
  def realm2(n: Int): Int =
    if (n > 0)
      if (n % 2 == 0)
        if (n % 3 != 0)
          2
        else throw new Exception("Divisible by 3")
      else throw new Exception("Not even")
    else throw new Exception("Not positive")


  // even positives divisible by 6 and 7 are realm 3
  def realm3(n: Int): Int =
    if (n > 0)
      if (n % 6 == 0)
        if(n % 7 == 0)
          3
        else throw new Exception("Not divisible by 7")
      else throw new Exception("Not divisible by 6")
    else throw new Exception("Not positive")

  def realm(n: Int): Int =
    try
      realm1(n)
    catch
      case e: Exception =>
        try
          realm2(n)
        catch
          case e: Exception =>
            try
              realm3(n)
            catch
              case e: Exception => 0


  println(s"realm(0) = ${realm(0)}")   // 0
  println(s"realm(4) = ${realm(4)}")   // 2
  println(s"realm(42) = ${realm(42)}") // 3
  println(s"realm(9) = ${realm(9)}")   // 1

  // problem 5

  // odd positives are realm 1
  def realm1Opt(n: Int): Option[Int] =
    if (n > 0)
      if (n % 2 != 0)
        Some(1)
      else None
    else None



  // even positives not divisible by 3 are realm 2
  // another style: check for bad news first
  def realm2Opt(n: Int): Option[Int] =
    if (n > 0)
      if (n % 2 == 0)
        if (n % 3 != 0)
          Some(2)
        else None
      else None
    else None

  // even positives divisible by 6 and 7 are realm 3
  def realm3Opt(n: Int): Option[Int] =
    if (n > 0)
      if (n % 6 == 0)
        if(n % 7 == 0)
          Some(3)
        else None
      else None
    else None

  def realmOpt(n: Int) =
    try
      realm1(n)
    catch
      case e: Exception =>
        try
          realm2(n)
        catch
          case e: Exception =>
            try
              realm3(n)
            catch
              case e: Exception => 0


  println(s"realmOpt(0) = ${realmOpt(0)}")   // 0
  println(s"realmOpt(4) = ${realmOpt(4)}")   // 2
  println(s"realmOpt(42) = ${realmOpt(42)}") // 3
  println(s"realmOpt(9) = ${realmOpt(9)}")   // 1



}