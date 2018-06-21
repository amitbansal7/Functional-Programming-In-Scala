import scala.annotation.tailrec

object chapter2 extends App {

  // Exercise 2.1
  def fib(n: Int): Int = {
    def solve(n: Int, curr: Int, prev: Int): Int = {
      if (n <= 0) curr
      else solve(n - 1, prev + curr, curr)
    }

    solve(n - 1, 1, 0)
  }

  println(fib(6))

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  println(findFirst(Array[Int](1, 2, 3, 4, 5, 6), (a: Int) => a == 2))

  //Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(as: Array[A], i: Int, ordered: (A, A) => Boolean): Boolean = {
      if (i >= as.length) true
      else if (ordered(as(i - 1), as(i))) loop(as, i + 1, ordered)
      else false
    }

    loop(as, 1, ordered)
  }

  println(isSorted(Array(1, 2, 3, 7, 5, 6), (a: Int, b: Int) => a <= b))
  println(isSorted(Array(1, 2, 3, 7, 5, 6), (a: Int, b: Int) => a <= b))

  //2.6 Following types to implementations
  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  //Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  //Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  //Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
