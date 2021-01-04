package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    val l = if (c > r / 2) c else r - c
    def loop(i: Int, acc: Int): Int = {
      if (i == l + 1) acc
      else loop(i + 1, acc * (r - l + i) / i)
    }
    loop(1, 1);
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(list: List[Char], acc: Int): Int =
      if (list.isEmpty) acc
      else if (list.head == '(') loop(list.tail, acc + 1)
      else if (list.head == ')' && acc > 0) loop(list.tail, acc - 1)
      else if (list.head == ')' && acc <= 0) -1
      else loop(list.tail, acc)
    loop(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0 && !coins.isEmpty)
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else if (money == 0) 1 else 0
  }
}
