package list

import scala.annotation.tailrec

object Questions extends App {

  def question1(list: List[Int]): Int = {
    if (list.isEmpty) -1 else list.last
  }

  def question1B(list: List[Int]): Int = {
    @tailrec
    def go(list: List[Int], elem: Int): Int = {
      list match {
        case Nil => elem
        case h :: t => go(t, h)
      }
    }

    go(list, -1)
  }


  def question2(list: List[Int]): Int = {
    if (list.size <= 1) -1 else list(list.size - 2)
  }


  @tailrec
  def question2B(list: List[Int]): Int =
    list match {
      case h :: _ :: Nil => h
      case _ :: t => question2B(t)
      case _ => -1
    }


  //println(question1B(List(1)))
  println(question2B(List(1,2,3)))
  println(question2B(List(1,2)))
  println(question2B(List(1)))
  println(question2B(List.empty))

}
