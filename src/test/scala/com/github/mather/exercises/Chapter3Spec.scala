package com.github.mather.exercises

import org.scalatest._

class Chapter3Spec extends FlatSpec with Matchers {

  import Chapter3._

  "3-1: List matching" should "works correctly" in {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x should be(3)
  }

  "3-2: tail" should "return list except first element" in {
    List(1,2,3).tail should be(List(2,3))
    List(1).tail should be(Nil)
    the [Exception] thrownBy List().tail
  }

  "3-3: drop" should "return dropped list" in {
    List(1,2,3,4,5).drop(3) should be(List(4,5))
    the [Exception] thrownBy List(1,2,3,4,5).drop(6)
  }
}
