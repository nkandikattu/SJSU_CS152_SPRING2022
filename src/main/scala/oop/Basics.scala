package oop


// ++++++++++++++++++++++++
// Implementing a reference class
// ++++++++++++++++++++++++

// given
enum LetterGrade:
  case A, B, C, D, F

class Assignment(val studentName: String,  var number: Int, private var _grade: Int):
  if(_grade < 0 || _grade > 100) throw Exception("Invalid Grade")
  def grade = _grade
  def grade_=(num: Int) =
    if(num < 0 || num > 100) throw Exception("Invalid Grade")
    _grade = num
  def letterGrade =
    grade match
      case grade if 90 <= grade && grade <= 100 => LetterGrade.A
      case grade if 80 <= grade && grade < 90 => LetterGrade.B
      case grade if 70 <= grade && grade < 80 => LetterGrade.C
      case grade if 60 <= grade && grade < 70 => LetterGrade.D
      case grade if grade < 60 => LetterGrade.F

  override def toString: String = studentName + " assn " + number + ": " + _grade + " (= " + letterGrade + ")"



object testAssignment extends App:

  try
    val simpson = Assignment("Simpson", 1, -88)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade

  val jones = Assignment("Jones", 1, 88)
  val hanson = Assignment("Hanson", 1, 95)

  println(jones.grade) // 88
  println(jones.letterGrade) // B
  jones.grade = jones.grade + 10
  println(jones.grade) //98
  println(jones.letterGrade) // A
  println(jones) // Jones assn 1: 98 (= A)

  println(hanson.grade) // 95
  println(hanson.letterGrade) // A
  try
    hanson.grade = hanson.grade + 10
  catch
    case e: Exception => println(e.getMessage) // Invalid grade
  finally
    println(hanson) // Hanson assn 1: 95 (= A)
  try
    val smith = Assignment("Smith", 1, -10)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade


// ++++++++++++++++++++++++
// Implementing static variables & methods
// ++++++++++++++++++++++++

class Transaction(val fromAcct: Int,  var toAcct: Int, private var _amt: Double):
  if(_amt < 0) throw Exception("Invalid Amount")
  def amt = _amt
  def amt_=(num: Int) =
    if(num < 0) throw Exception("Invalid Amount")
    _amt = num

  def txID = 500 + txIDIncrementer()
  override def toString: String = "Transaction #" + txID + ": $" + _amt + " from acct " + fromAcct + " to acct " + toAcct

var count = 0
def txIDIncrementer(): Int =
  count += 1
  count

object testTransactions extends App:
  try
    val t1 = Transaction(119, 212, -20.50)
  catch
    case e: Exception => println(e.getMessage) // Invalid amount
  val ledger = List(
    Transaction(119, 212, 600.50),
    Transaction(212, 119, 1200),
    Transaction(212, 119, 98.75)
  )
  ledger.foreach(println) // how to create unique IDs
/*
Transaction #500: $600.5 from acct 119 to acct 212
Transaction #501: $1200.0 from acct 212 to acct 119
Transaction #502: $98.75 from acct 212 to acct 119
*/

// ++++++++++++++++++++++++
// Implementing a value class
// ++++++++++++++++++++++++

class Time(val hour: Int,  val minute: Int = 0) extends Ordered[Time]:

  def this(time: String) =
    this(time.slice(0, time.indexOf(":")).toInt, time.slice(time.indexOf(":") + 1, time.length).toInt)

  if(hour < 0 || hour >= 24) throw Exception("Invalid Hour")
  if(minute < 0 || minute >= 60) throw Exception("Invalid Minute")

  def +(other: Time): Time =
    var newHour = this.hour + other.hour
    var newMinute = this.minute + other.minute
    if(newMinute >= 60)
      newHour += 1
      newMinute -= 60
    if(newHour >= 24)
      newHour -= 24
    Time(newHour, newMinute)

  override def equals(obj: Any): Boolean =
    obj match
      case t: Time => t.isInstanceOf[Time]
        && t.hour == this.hour
        && t.minute == this.minute
      case _ => false

  override def toString: String =
    if(minute == 0) hour + ":00"
    else hour + ":" + minute

  override def hashCode = this.toString.hashCode

  override def compare(that: Time): Int =
    that match
      case that if this.hour < that.hour => -1
      case that if this.hour == that.hour && this.minute == that.minute => 0
      case that if this.hour == that.hour && this.minute < that.minute => -1
      case that if this.hour == that.hour && this.minute > that.minute => 1
      case _ => 1


class PreciseTime(override val hour: Int, override val minute: Int = 0, val second: Int = 0)
  extends Time(hour: Int, minute: Int):

  if(second < 0 || second >= 60) throw Exception("Invalid Second")

  override def equals(obj: Any): Boolean =
    obj match
      case t: PreciseTime => t.isInstanceOf[PreciseTime]
        && t.hour == this.hour
        && t.minute == this.minute
        && t.second == this.second
      case _ => false

  override def toString: String =
    if(minute == 0 && second == 0)
      hour + ":00:00"
    else if(second == 0)
      hour + ":" + minute + ":00"
    else if(minute == 0)
      hour + ":00:" + second
    else
      hour + ":" + minute + ":" + second

  override def hashCode = this.toString.hashCode

  def compare(that: PreciseTime): Int =
    that match
      case that if this.hour < that.hour => -1
      case that if this.hour == that.hour && this.minute < that.minute => -1
      case that if this.hour == that.hour
        && this.minute == that.minute
        && this.second == that.second => 0
      case that if this.hour == that.hour
        && this.minute == that.minute
        && this.second > that.second => -1
      case that if this.hour == that.hour
        && this.minute == that.minute
        && this.second < that.second => 1
      case that if this.hour == that.hour && this.minute > that.minute => 1
      case _ => 1


object testTime extends App:
  try
    val t = Time(24, 50)
  catch
    case e: Exception => println(e.getMessage) // Invalid hour
  try
    val t = Time(12, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid minute

  val t1 = Time(10, 30)
  val t2 = Time(15, 45)
  val t3 = Time(10, 30)
  val t4 = Time("18:45")
  val t5 = Time(17)
  val t6 = t1 + t2
  println(t1) // 10:30
  println(t2) // 15:45
  println(t3) // 10:30
  println(t4) // 18:45
  println(t5) // 17:00
  println(t6) // 2:15
  println(t1 == t3) // true
  println(t1 != t5) // true
  println(t1 < t2)  // true
  println(t4 < t2)  // false
  println(t1 <= t3) // true

  val schedule = Map(
    t1 -> "coffee break",
    t2 -> "nap",
    t4 -> "cocktail hour"
  )
  println(schedule) // Map(10:30 -> coffee break, 15:30 -> nap, 18:45 -> cocktail hour)
  println(schedule(t3)) // coffee break

  try
    val pt = PreciseTime(12, 0, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid second

  val pt2 = PreciseTime(18)
  val pt1 = PreciseTime(10, 30)
  println(pt1) // 10:30:00
  println(pt2) // 18:00:00
  println(t1) // 10:30
  println(pt1 == t1) // false
  try
    println(schedule(pt1))
  catch
    case e: Exception => println(e.getMessage) // key not found: 10:30:00
