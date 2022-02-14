package basics

class PolynomialUtils {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] =
    val a = p(0)
    val b = p(1)
    val c = p(2)
    val dis = math.pow(b,2) - 4 * a * c
    val root = math.sqrt(math.abs(dis))
    val root1 = (-b + root) / (2 * a)
    val root2 = (-b - root) / (2 * a)
    Some((root1, root2))


  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
    val a = p(0)
    val b = p(1)
    val c = p(2)
    val termC = b
    val termB = a * 2
    val termA = 0
    (termA, termB, termC)


  def eval(a: Double, p: (Double, Double, Double)): Double =
    val termA = p(0) * math.pow(a,2)
    val termB = p(1) * a
    val termC = p(2)
    termA + termB + termC


  def toString(p: (Double, Double, Double)) =
    val a = p(0)
    val b = p(1)
    val c = p(2)
    var str = ""
    if (a != 0) str = str + s"${a}x^2"
    if (b != 0) str = str + s" + ${b}x"
    if (c != 0) str = str + s" + ${c}"
    str = str.trim
    if(str.startsWith("+ ")) str = str.substring(2)
    str
}


object Polynomials extends PolynomialUtils with App {
  val poly = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)

  println("poly = " + toString(poly))
  println("eval(6, poly) = " + eval(6, poly))
  println("eval(2, poly) = " + eval(2, poly))
  println("eval(-5, poly) = " + eval(-5, poly))

  println("roots(poly) = " + roots(poly))

  println("deriv(poly) = " + toString(deriv(poly)))
  println("deriv2(poly) = " + toString(deriv(deriv(poly))))

}

/*
output
poly = 3.0x^2 + 9.0x + -30.0
eval(6, poly) = 132.0
eval(2, poly) = 0.0
eval(-5, poly) = 0.0
roots(poly) = Some((2.0,-5.0))
deriv(poly) = 6.0x + 9.0
deriv2(poly) = 6.0
*/