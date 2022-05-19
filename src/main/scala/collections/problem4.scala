package collections

object problem4 extends App{

  class Transaction(val amt: Double, val fromAcct: Int, val toAcct: Int)

  def balance1(acct: Int, ledger: List[Transaction]): Double =
    ledger.filter(x => (x.fromAcct == acct || x.toAcct == acct))
      .map((tx: Transaction) => if(tx.fromAcct == acct) (-1*tx.amt, tx.fromAcct, tx.toAcct) else (tx.amt, tx.fromAcct, tx.toAcct))
      .map(_._1)
      .reduce(_+_)

  def balance2(acct: Int, ledger: List[Transaction]): Double = {
    val deposits = ledger.filter(_.toAcct == acct).map(_.amt).reduce(_ + _)
    val withdrawls = ledger.filter(_.fromAcct == acct).map(_.amt).reduce(_ + _)
    deposits - withdrawls
  }


  val ledger = List(
    Transaction(20, 1, 2), // 1 = -20, 2 = 20, 3 = 0
    Transaction(10, 1, 3), // 1 = -30, 2 = 20, 3 = 10
    Transaction(10, 2, 3), // 1 = -30, 2 = 10, 3 = 20
    Transaction(10, 3, 1), // 1 = -20, 2 = 10, 3 = 10
    Transaction(10, 3, 1)) // 1 = -10, 2 = 10, 3 = 0

  println(balance1(1, ledger))
  println(balance1(2, ledger))
  println(balance1(3, ledger))

  println(balance2(1, ledger))
  println(balance2(2, ledger))
  println(balance2(3, ledger))

  val block = List(
    Transaction(125.50, 100, 200),
    Transaction(100.50, 200, 100),
    Transaction(300.20, 400, 200),
    Transaction(90, 200, 400),
    Transaction(50, 100, 400))
  println("balance = $" + balance1(200, block)) // balance = $235.2
  println("balance = $" + balance2(200, block)) // balance = $235.2

}