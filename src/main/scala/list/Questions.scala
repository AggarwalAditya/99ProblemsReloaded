package list

import scala.annotation.tailrec

object Questions extends App {

  def question1(list: List[Int]): Int = {
    if(list.isEmpty) -1 else list.last
  }

  def question1B(list: List[Int]): Int = {
    @tailrec
    def go(list: List[Int], elem: Int): Int = {
      list match {
        case Nil => elem
        case h::t => go(t,h)
      }
    }
    go(list,-1)
  }


  println(question1B(List(1)))

}
