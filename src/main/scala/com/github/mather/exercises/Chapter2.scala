package com.github.mather.exercises

object Chapter2 {

  // Exercise 2-1
  // n-th Fibonacci number with recursive function
  def fib(n: Int): Int = {
    @annotation.tailrec
    def step(remains: Int, a: Int, b: Int): Int = remains match {
      case i if i < 2 => 0
      case 2          => b
      case i          => step(remains - 1, b, a + b)
    }
    step(n, 0, 1)
  }


}
