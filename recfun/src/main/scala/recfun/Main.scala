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
  def pascal(c: Int, r: Int): Int = 
    if(r==0 || c==0) 1
    else if(c == 1) r
    else if (r==c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = { 
    def bal(char: List[Char]): Int = {
      if (char.isEmpty) 0
      else {
        val x: Int = bal(char.tail)
        if (x>0) x
        else if (char.head == '(') x+1
        else if(char.head == ')') x-1
        else x
      }
    }
    bal(chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def cC(money: Int, coins: List[Int], curTot: Int): Int = {
      if (money==0 || coins.isEmpty) 0
      else if (curTot == money) 1
      else if (curTot > money) 0
      else cC(money, coins.tail, curTot) + cC(money, coins, curTot + coins.head)
    }
    cC(money, coins, 0)
  }
}
