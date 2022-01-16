package list

import java.util.NoSuchElementException
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


  @tailrec
  def question3(list: List[Int], k: Int): Int = {
    (k, list) match {
      case (_, Nil) => -1
      case (0, h :: _) => h
      case (_, _ :: t) => question3(t, k - 1)
    }
  }


  //println(question1B(List(1)))
  //  println(question2B(List(1,2,3)))
  //  println(question2B(List(1,2)))
  //  println(question2B(List(1)))
  //  println(question2B(List.empty))

  println(question3(List(1, 2, 3, 4, 5), 9))
  println(question3(List(1, 2, 3, 4, 5), 2))
  println(question3(List(1, 2, 3, 4, 5), 0))

}
