package com.github.mather.exercises

import org.scalatest._

class Chapter2Spec extends FlatSpec with Matchers {

  "fib() function" should "return Fibonacci number correctly" in {
    Chapter2.fib(1) should be(0)
    Chapter2.fib(2) should be(1)
    Chapter2.fib(6) should be(5)
  }

}
