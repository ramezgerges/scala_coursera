package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def balanceIter(leftParens: Int, chars: List[Char]): Boolean =
      if (leftParens < 0) false
      else if (chars.isEmpty) leftParens == 0
      else if (chars.head == ')') balanceIter(leftParens - 1, chars.tail)
      else if (chars.head == '(') balanceIter(leftParens + 1, chars.tail)
      else balanceIter(leftParens, chars.tail)

    balanceIter(0, chars)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0) 0
    else if (coins.isEmpty) if money == 0 then 1 else 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)