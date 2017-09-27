package recfun
import common._
import org.scalacheck.Prop.True

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
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r){
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def recursiveBalance(chars: List[Char], parenCount: Int): Boolean = {
      if(parenCount < 0) {
        // At any point if we have too many close parens, short circuit out
        false
      } else if(chars.isEmpty) {
        // We are on the last character.  If 0, we have balanced paren counts
        // otherwise we have too many open parens
        if(parenCount == 0) {
          true
        } else {
          false
        }
      } else {
        chars.head.toString match {
          case "(" => recursiveBalance(chars.tail, parenCount + 1)
          case ")" => recursiveBalance(chars.tail, parenCount - 1)
          case _ => recursiveBalance(chars.tail, parenCount)
        }
      }
    }

    recursiveBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) {
      1
    } else if (money > 0 && coins.nonEmpty) {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    } else {
      0
    }

  }
}
