package com.github.mather.exercises

import org.scalatest._

class Chapter3Spec extends FlatSpec with Matchers {

  import DataStructure._
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

  "3-2: setHead" should "replace first element of List" in {
    List(1,2,3).setHead(5) should be(List(5,2,3))
    Nil.setHead(10) should be(Nil)

    setHead(Nil, 1) should be(Nil)
    setHead(List(1,2,3), 5) should be(List(5,2,3))
  }

  "3-3: tail" should "return list except first element" in {
    List(1,2,3).tail should be(List(2,3))
    List(1).tail should be(Nil)
    the [Exception] thrownBy List().tail

    tail(Nil) should be(Nil)
    tail(List(10,20,30)) should be(List(20,30))
  }

  "3-4: drop" should "return dropped list" in {
    List(1,2,3,4,5).drop(3) should be(List(4,5))
    the [Exception] thrownBy List(1,2,3,4,5).drop(6)
    drop(List(1,2,3,4,5), 3) should be(List(4,5))
    drop(List(1,2,3), 5) should be(Nil)
  }

  "3-5: dropWhile" should "drop element which satisfy predicate" in {
    List(1,2,5,4,3).dropWhile(_ < 5) should be(List(5,4,3))
    List(1,2,5,4,3).dropWhile(_ > 0) should be(Nil)

    dropWhile(List(1,2,5,4,3))(_ < 5) should be(List(5,4,3))
    dropWhile(List(1,2,5,4,3))(_ > 0) should be(Nil)
  }
}
