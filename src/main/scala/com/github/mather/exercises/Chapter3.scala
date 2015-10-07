package com.github.mather.exercises

object Chapter3 {

  sealed trait List[+A] {
    def tail: List[A]
  }

  case object Nil extends List[Nothing] {
    // ひとまず Nil に対する tail は Exception を返すようにする
    def tail: List[Nothing] = throw new Exception
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }

}
