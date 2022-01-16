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

  def question4(list: List[Int]): Int = {
    @tailrec
    def go(list: List[Int], k: Int): Int = {
      list match {
        case Nil => k
        case _ :: t=> go(t, k+1)
      }
    }
    go(list,0)
  }

  def question5(list: List[Int]): List[Int] = {
    @tailrec
    def go(list: List[Int], res: List[Int]): List[Int] = {
      list match {
        case Nil => res
        case h::t => go(t, h::res)
      }
    }
    go(list, Nil)
  }


  def question6(list: List[Int]): Boolean = {
    list.equals(question5(list))
  }

  //println(question1B(List(1)))
  //  println(question2B(List(1,2,3)))
  //  println(question2B(List(1,2)))
  //  println(question2B(List(1)))
  //  println(question2B(List.empty))

//  println(question3(List(1, 2, 3, 4, 5), 9))
//  println(question3(List(1, 2, 3, 4, 5), 2))
//  println(question3(List(1, 2, 3, 4, 5), 0))

//  println(question4(List(1,2,3,4,5)))
//  println(question4(List.empty))


//  println(question5(List(1,2,3,4,5)))
//  println(question5(List.empty))

  println(question6(List(1,2,1)))
  println(question6(List(1)))
  println(question6(List()))
  println(question6(List(1,2,3)))

}
