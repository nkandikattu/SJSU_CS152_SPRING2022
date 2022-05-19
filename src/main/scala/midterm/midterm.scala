package midterm

//                                   Midterm Solutions

// ++++++++++++++++++++++++++++++++
// Problem 1 : errors
// ++++++++++++++++++++++++++++++++

def errors[T](model: T=>T, subject: T=>T, inputs: List[T]): Int =
  def helper(result: Int, unseen: List[T]): Int =
    if(unseen == Nil) result
    else if(model(unseen.head) != subject(unseen.head)) helper(result+1, unseen.tail)
    else helper(result, unseen.tail)
  helper(0, inputs)

@main def testErrors() =
  // instructor's model solution:
  def cube(x: Double) = x * x * x
  // student's subject solution
  def cube1(x: Double) = math.abs(x * x * x)
  // inputs
  val inputs = List(0.0, -1.0, -2.0, 3.0, 4.0)
  println(errors(cube, cube1, inputs)) // = 2

// ++++++++++++++++++++++++++++++++
// Problem 2 bind
// ++++++++++++++++++++++++++++++++

def bind[S, T, U](f: S => Option[T], g: U => Option[S]): U=>Option[T] =
  def helper(x: U): Option[T] =
    g(x) match
      case Some(x) => f(x)
      case None => None
  helper

@main def testBind() =
  def invert(x: Double): Option[Double] = if (x == 0) None else Some(1/x)
  def sqrt(x: Double): Option[Double] = if (x < 0) None else Some(math.sqrt(x))
  def invertSqrt = bind(invert, sqrt)
  println(invertSqrt(100))   // = Some(0.1)
  println(invertSqrt(-100) ) // = None
  println(invertSqrt(0))     // = None


// ++++++++++++++++++++++++++++++++
// Problem 3 Flight Tracker
// ++++++++++++++++++++++++++++++++

class Flight(val number: Int, private var _eta: (Int, Int), private var _status: Int):
  if(_eta._1 < 0 || _eta._1 >= 24) throw Exception("Invalid ETA")
  if(_eta._2 < 0 || _eta._2 >= 60) throw Exception("Invalid ETA")
  if(_status < -1 || _status >= 3) throw Exception("Invalid Status")

  def eta = _eta
  def eta_=(newEta: (Int, Int)) =
    if(newEta._1 < 0 || newEta._1 >= 24) throw Exception("Invalid ETA")
    if(newEta._2 < 0 || newEta._2 >= 60) throw Exception("Invalid ETA")
    _eta = newEta

  def status = _status
  def status_=(newStatus: Int) =
    if(newStatus < -1 || newStatus >= 3) throw Exception("Invalid Status")
    _status = newStatus

  override def toString: String =
    if(_status == -1)
      "flight #" + number + " arrives at " + _eta._1 + ":" + _eta._2 + " status: cancelled"
    else if(_status == 0)
      "flight #" + number + " arrives at " + _eta._1 + ":" + _eta._2 + " status: boarding"
    else if(_status == 1)
      "flight #" + number + " arrives at " + _eta._1 + ":" + _eta._2 + " status: in transit"
    else
      "flight #" + number + " arrives at " + _eta._1 + ":" + _eta._2 + " status: arrived"


@main def flightTracker =
  val flight1 = Flight(314, (15, 10), 0)
  println(flight1.toString) // flight #314 arrives at 15:10 status: boarding
  flight1.eta = (15, 35)
  flight1.status = 1
  println(flight1.toString) // flight #314 arrives at 15:35 status: in transit
  try
    val flight2 = Flight(802, (15, 60), 1)
  catch
    case e: Exception => println(e.getMessage) // Invalid eta
  try
    flight1.eta = (25, 45)
  catch
    case e: Exception => println(e.getMessage) // Invalid eta
  try
    flight1.status= 3
  catch
    case e: Exception => println(e.getMessage) // Invalid status