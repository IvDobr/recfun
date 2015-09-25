package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (0 == c || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def part(chars: List[Char], counter: Int): Int =
      if (0 > counter || chars.isEmpty) counter
      else if ('(' == chars.head) part(chars.tail, counter+1)
      else if (')' == chars.head) part(chars.tail, counter-1)
      else part(chars.tail, counter)

    0 == part(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (0 == money) 1
    else if (0 > money || 0 == coins.size) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
