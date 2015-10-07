package com.github.mather.exercises

object Chapter3 {

  sealed trait List[+A] {
    def tail: List[A]
    def drop(n: Int): List[A]
    def dropWhile(pred: A => Boolean): List[A]
  }

  case object Nil extends List[Nothing] {
    // ひとまず Nil に対する tail は Exception を返すようにする
    def tail: List[Nothing] = throw new Exception
    def drop(n: Int): List[Nothing] = n match {
      case 0 => Nil
      case k if k < 0 => throw new IllegalArgumentException
      case k => throw new Exception
    }
    def dropWhile(pred: Nothing => Boolean): List[Nothing] = Nil
  }

  case class Cons[+A](head: A, tail: List[A]) extends List[A] {
    def drop(n: Int): List[A] = n match {
      case 0 => this
      case k if k < 0 => throw new IllegalArgumentException
      case k => tail.drop(k-1)
    }

    def dropWhile(pred: A => Boolean): List[A] = {
      if (pred(head)) tail.dropWhile(pred)
      else this
    }
  }

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

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, k) if k < 0 => throw new IllegalArgumentException
    case (xs, 0) => xs
    case (Nil, k) => Nil
    case (Cons(x, xs), k) => drop(xs, k-1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case xs => xs
  }

}
