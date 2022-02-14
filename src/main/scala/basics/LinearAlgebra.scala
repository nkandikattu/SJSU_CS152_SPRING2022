package basics

class LinearAlgebraUtils {

  def dim(mat: Array[Array[Double]]): (Int, Int) =
    var rowCheck = new Array[Int](mat.length)
    for (i <- 0 until(mat.length))
      var rowLength = 0
      for (j <- 0 until(mat(i).length))
        rowLength += 1
      rowCheck(i) = rowLength

    for (i <- 0 until(rowCheck.length-1))
      if (rowCheck(i) != rowCheck(i+1))
        throw new Exception("Rows are different lengths")

    var dim1 = 0
    var dim2 = 0
    for (i <- mat) dim1 += 1
    for (i <- mat(0)) dim2 += 1
    (dim1, dim2)

  def dot(vec1: Array[Double], vec2: Array[Double]): Double =
    if (vec1.length != vec2.length) throw new Exception("Dimensions don't match")
    var dotProd: Double = 0

    for (i <- 0 until(vec1.length))
        dotProd += vec1(i) * vec2(i)
    dotProd


  def product(mat: Array[Array[Double]], vec: Array[Double]): Array[Double] =
    if (vec.length != dim(mat)._1) throw new Exception("Dimensions don't match")
    var prodArr = new Array[Double](vec.length)

    for (i <- 0 until(mat.length))
      for (j <- 0 until mat(i).length)
          prodArr(i) += vec(j) * mat(i)(j)
    prodArr


  def transpose(mat: Array[Array[Double]]): Array[Array[Double]] =
    var pose: Array[Array[Double]] = Array(Array(0.0, 0.0, 0.0), Array(0.0, 0.0, 0.0), Array(0.0, 0.0, 0.0))
    for (i <- 0 until(dim(mat)._1))
      for (j <- 0 until(dim(mat)._2))
        pose(j)(i) = mat(i)(j)
    pose


  def sum(vec1: Array[Double], vec2: Array[Double]): Array[Double] =
    if(vec1.length != vec2.length) throw new Exception("Dimensions don't match")

    def vecSum = new Array[Double](vec1.length)
    for (i <- 0 until(vec1.length))
      vecSum(i) = vec1(i) + vec2(i)
    vecSum


  def sum(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]] =
    if(dim(mat1) != dim(mat2)) throw new Exception("Dimensions don't match")

    var matSum: Array[Array[Double]] = mat1
    for (i <- 0 until(mat1.length))
      for (j <- 0 until(mat1(i).length))
        matSum(i)(j) = mat1(i)(j) + mat2(i)(j)
    matSum


  def trace(mat: Array[Array[Double]]) =
    var sum: Double = 0
    for (i <- 0 until(mat.length))
      for (j <- 0 until(mat(i).length))
        if (i == j)
          sum += mat(i)(j)
    sum


  def toString(vec: Array[Double]): String =
    var str: String = "["
    for (i <- 0 until(vec.length))
      str = str + String.valueOf(vec(i)) + " "
    str = str.trim + "]"
    str


  def toString(mat: Array[Array[Double]]): String =
    var str: String = ""
    for (i <- 0 until(mat.length))
      var strRow = "["
      for (j <- 0 until(mat(i).length))
        strRow += String.valueOf(mat(i)(j)) + " "
      strRow = strRow.trim + "]"
      str += strRow + "\n"
    str

}

object LinearAlgebra extends LinearAlgebraUtils with App {
  try {
    def vec1 = Array(5.0, 2.0, 6.0)

    def mat1 = Array(Array(1.0, 2.0, 3), Array(4.0, 5, 6), Array(7.0, 8, 9))

    def mat2 = Array(Array(10.0, 11, 12), Array(13.0, 14, 15), Array(16.0, 17, 18))

    println("vec1 = " + toString(vec1))
    println("mat1 = ")
    println(toString(mat1))
    println("mat2 = ")
    println(toString(mat2))
    println("dim(mat1) = " + dim(mat1))
    println("trace(mat1) = " + trace(mat1))
    println("transpose(mat2) = ")
    println(toString(transpose(mat2)))
    println("sum(mat1, mat2) = ")
    println(toString(sum(mat1, mat2)))
    println("product(mat1, vec1) = " + toString(product(mat1, vec1)))

    val vec2 = Array(0.0, 4.0, 9.0, 8.0)
    println("product(mat1, vec1) = " + toString(product(mat1, vec2)))
  } catch {
    case e: Exception => println(e)
  }

}

/*
output:
vec1 = [5.0 2.0 6.0]
mat1 =
[1.0 2.0 3.0]
[4.0 5.0 6.0]
[7.0 8.0 9.0]

mat2 =
[10.0 11.0 12.0]
[13.0 14.0 15.0]
[16.0 17.0 18.0]

dim(mat1) = (3,3)
trace(mat1) = 15.0
transpose(mat2) =
[10.0 13.0 16.0]
[11.0 14.0 17.0]
[12.0 15.0 18.0]

sum(mat1, mat2) =
[11.0 13.0 15.0]
[17.0 19.0 21.0]
[23.0 25.0 27.0]

product(mat1, vec1) = [27.0 66.0 105.0]
java.lang.Exception: Dimensions don't match
*/