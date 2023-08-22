class Account(initialAmount: Double) {
  var balance: Double = initialAmount

  def deposit(amount: Double): Unit = {
    balance += amount
  }

  def withdraw(amount: Double): Boolean = {
    if (balance >= amount) {
      balance -= amount
      true
    } else {
      false
    }
  }

  def transfer(amount: Double, transferacc: Account): Boolean = {
    if (withdraw(amount)) {
      transferacc.deposit(amount)
      true
    } else {
      false
    }
  }

  def applyInterest: Unit = {
    if (balance > 0) {
      balance *= 1.05
    } else {
      balance *= 1.1
    }
  }

  def getBalance: Double = balance
}

class Bank(newacc: List[Account]) {
  var acc: List[Account] = newacc

  def getNegativeBalance: List[Account] =
    acc.filter(account => account.getBalance < 0)

  def getTotalBalance: Double = acc.map(_.getBalance).sum

  def applyInterestToAccs: Unit = acc.foreach(_.applyInterest)

  def getAllAccountBalances: List[Double] = acc.map(_.balance)

}

object Q3Q4 extends App {
  val acc1 = new Account(5000.0)
  val acc2 = new Account(2500.0)
  val acc3 = new Account(-1000.0)
  val acc4 = new Account(-3000.0)

  val bank = new Bank(List(acc1, acc2, acc3, acc4))

  println("Accounts with negative balances:")
  bank.getNegativeBalance.foreach(account => println(account.getBalance))

  println("Total balance of all acc: " + bank.getTotalBalance)

  println("Balances after applying interest:")
  bank.applyInterestToAccs
  println(bank.getAllAccountBalances)

}
