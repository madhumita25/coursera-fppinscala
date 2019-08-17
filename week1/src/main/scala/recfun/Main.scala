package recfun

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
    def pascal(c: Int, r: Int): Int = (c,r) match {
      case (0,0) => 1
      case (0, _) => 1
      case (_, 0) => 1
      case (x,y) =>  if (x == y) 1 else (pascal(x-1,y-1) + pascal(x,y-1))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def diff(chars: List[Char], numClose:Int ): Boolean = {
        chars match {
          case Nil => (numClose == 0)
          case '(' :: Nil => false
          case ')' :: Nil => (numClose == 1)
          case _ :: Nil => (numClose == 0)
          case '(' :: tail => diff(tail,numClose+1)
          case ')':: tail => diff(tail, numClose-1)
          case _ :: tail => diff(tail, numClose)
        }
      }

      diff(chars, 0) == true

  }
  
  /**
   * Exercise 3
*/
    def countChange(money: Int, coins: List[Int]): Int = {

        (money,coins) match {
          case (0,l) => 1
          case (x,Nil) => 0
          case (x,l) if (x < 0)  => 0
          case (x,l) => (countChange(x-l.head, l) + countChange(x, l.tail))
        }

    }
  }
