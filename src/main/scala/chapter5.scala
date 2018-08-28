
object chapter5 extends App {

  sealed trait Stream[+A] {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    /**
      * Exercise 5.1
      * Write a function to convert a Stream to a List, which will force
      * its evaluation and let you look at it in the REPL. You can convert
      * to the regular List type in the standard library. You can place this
      * and other functions that operate on a Stream inside the Stream trait.
      */
    def toList: List[A] = {
      def hlpr(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(x, xs) => hlpr(xs(), x() :: acc)
        case _ => List()
      }

      hlpr(this, List()).reverse
    }


    /**
      * Exercise 5.2
      * Write the function take(n) for returning the first n elements of a Stream,
      * and drop(n) for skipping the first n elements of a Stream.
      */

    def take(n: Int): Stream[A] = this match {
      case Cons(x, xs) if n > 1 => cons(x(), xs().take(n - 1))
      case Cons(x, _) if n == 1 => cons(x(), empty)
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, xs) if n > 0 => xs().drop(n - 1)
      case _ => this
    }


    def foldRight[B](z : => B)(f:(A, => B) => B):B = this match {
      case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
      case _ => z
    }

    /**
      * Exercise 5.3
      * Write the function takeWhile for returning all starting elements of a
      * Stream that match the given predicate.
      */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(x, xs) if p(x()) => cons(x(), xs().takeWhile(p))
      case _ => empty
    }

    /**
      * Exercise 5.4
      * Implement forAll, which checks that all elements in the Stream match a
      * given predicate. Your implementation should terminate the traversal as
      * soon as it encounters a nonmatching value.
      */
    def forAll(p: A => Boolean):Boolean = this match {
      case Cons(x, xs) => p(x()) && xs().forAll(p)
      case Empty => true
      case _ => false
    }

    /**
      * Exercise 5.5
      * Use foldRight to implement takeWhile.
      */
    def takeWhile_1(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((x, xs) =>
        if (p(x)) cons(x, xs)
        else empty
      )

    /**
      * Exercise 5.6
      * Hard: Implement headOption using foldRight.
      */
    def headOption_1(): Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    /**
      * Exercise 5.7
      * Implement map, filter, append, and flatMap using foldRight.
      * The append method should be non-strict in its argument.
      */

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((x, xs) =>
        if (f(x)) cons(x, xs)
        else xs)

    def append[B >: A](l: => Stream[B]): Stream[B] =
      foldRight(l)((x, xs) => cons(x, xs))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((x, xs) => f(x) append xs)

    /**
      * Exercise 5.8
      * Generalize ones slightly to the function constant, which
      * returns an infinite Stream of a given value.
      */
    def constant[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail

    }

    /**
      *Exercise 5.9
      * Write a function that generates an infinite stream of integers,
      * starting from n, then n + 1, n + 2, and so on.
      */
    def from(a: Int): Stream[Int] =
      Cons(() => a, () => from(a + 1))


    /**
      * Exercise 5.10
      * Write a function fibs that generates the infinite stream of
      * Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
      */
    def fibs(): Stream[Int] = {
      def util(prev: Int, cur: Int): Stream[Int] =
        Cons(() => cur, () => util(cur, prev + cur))

      util(0, 1)
    }


    /**
      * Exercise 5.11
      * Write a more general stream-building function called unfold.
      * It takes an initial state, and a function for producing both the
      * next state and the next value in the generated stream.
      */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => Cons(() => a, () => unfold(s)(f))
      case _ => Empty
    }

    /**
      * Exercise 5.12
      * Write fibs, from, constant, and ones in terms of unfold.
      */

    def fibsViaUnfold() =
      unfold((0, 1)) { case (prev, curr) => Some((prev, (curr, prev + curr))) }

    def fromViaUnfold(a: Int) =
      unfold(a)((x) => Some((x, x + 1)))

    def constantViaUnfold(a: Int) =
      unfold(a)((x) => Some((x, x)))

    def onesViaUnfold() =
      unfold(1)(_ => Some((1, 1)))

    /**
      * Exercise 5.13
      * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
      * and zipAll. The zipAll function should continue the traversal as long as
      * either stream has more elements—it uses Option to indicate whether each
      * stream has been exhausted.
      */
    def mapViaUnfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(x, xs) => Some((f(x()), xs()))
        case _ => None
      }

    def takViaUnfold(n: Int): Stream[A] =
      unfold((this, n)) {
        case (Cons(x, _), 1) => Some(x(), (empty, 0))
        case (Cons(x, xs), n) if n > 1 => Some(x(), (xs(), n - 1))
        case _ => None
      }

    def takeWhileViaUnfold(f:A => Boolean):Stream[A] =
      unfold(this){
        case Cons(x, xs) if f(x()) => Some((x(), xs()))
        case _ => None
      }

    def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(x, xs), Cons(y, ys)) =>
          Some(((f(x(), y())), (xs(), ys())))
        case _ => None
      }
    

  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  case object Empty extends Stream[Nothing]

}
