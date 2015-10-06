package com.github.mather.exercises

import org.scalatest._

class Chapter2Spec extends FlatSpec with Matchers {

  // Exercise 2-1
  "fib() function" should "return Fibonacci number correctly" in {
    Chapter2.fib(1) should be(0)
    Chapter2.fib(2) should be(1)
    Chapter2.fib(6) should be(5)
  }

  // Exercise 2-2
  "isSorted" should "check array is sorted" in {
    Chapter2.isSorted(Array(1,2,3), (a: Int, b: Int) => a <= b) should be(true)
    Chapter2.isSorted(Array(1,3,2), (a: Int, b: Int) => a <= b) should be(false)
    Chapter2.isSorted(Array("a", "cc", "bbb"), (a: String, b: String) => a.length <= b.length) should be(true)
  }

  // Exercise 2-3
  "curry" should "return curryed function" in {
    val add = (a:Int, b: Int) => a + b
    val curryedAdd = Chapter2.curry(add)
    curryedAdd(1)(2) should be(3)
  }
}
