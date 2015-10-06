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


  // Exercise 2-2
  // isSorted: Check if given array is sorted with given condition.
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    (for ( i <- 0 until (as.length - 1) ) yield (as(i), as(i+1))).
      forall{ case (a, b) => ordered(a, b) }
  }


  // Exercise 2-3
  // curry: return curryed function
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a: A) => f(a, _)
}
