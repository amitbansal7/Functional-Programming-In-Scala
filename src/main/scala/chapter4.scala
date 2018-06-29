
object chapter4 extends App {

  /**
    * Exercise : 4.1
    * Implement all of the preceding functions on Option. As you implement each function,
    * try to think about what it means and in what situations you’d use it. We’ll explore when
    * to use each of these functions next. Here are a few hints for solving this exercise:
    *  It’s fine to use pattern matching, though you should be able to implement all the
    * functions besides map and getOrElse without resorting to pattern matching.  For map and flatMap,
    * the type signature should be enough to determine the implementation.
    *  getOrElse returns the result inside the Some case of the Option, or if the Option
    * is None, returns the given default value.
    *  orElse returns the first Option if it’s defined; otherwise, it returns the second
    * Option.
    */
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f) getOrElse None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if (f(a)) => this
      case _ => None
    }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  val m = mean(List(1d, 2d, 3d))

  /**
    * Exercise 4.2
    * Implement the variance function in terms of flatMap.
    * If the mean of a sequence is m, the variance is the mean
    * of math.pow(x - m, 2) for each element x in the sequence.
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
    * Exercise 4.3
    * Write a generic function map2 that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too.
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  /**
    * Exercise 4.4
    * Write a function sequence that combines a list of Options into one Option
    * containing a list of all the Some values in the original list. If the original
    * list contains None even once, the result of the function should be None; otherwise
    * the result should be Some with a list of all the values.
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(List())
    case None :: _ => None
    case Some(x) :: xs => sequence(xs) match {
      case None => None
      case Some(a) => Some(x :: a)
    }
  }

  println(sequence(List(Some(1), Some(2), Some(3), Some(4))))
  println(sequence(List(Some(1), None, Some(3), Some(4))))


  /**
    * Exercise 4.5
    * Implement this function. It’s straightforward to do using map and sequence,
    * but try for a more efficient implementation that only looks at the list once.
    * In fact, imple- ment sequence in terms of traverse.
    */

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case _ => None
    }
  }

  def mapWithSeq[A, B](a: List[Option[A]], f: A => B): Option[List[B]] = a match {
    case Nil => Some(List())
    case Some(x) :: xs => Try(f(x)) match {
      case Some(xa) => mapWithSeq(xs, f) match {
        case Some(y) => Some(xa :: y)
        case None => None
      }
      case None => None
    }
    case None :: _ => None
  }

  println(mapWithSeq(List(Some(1), Some(2), Some(3), Some(4)), (a:Int) => a.toDouble))
  println(mapWithSeq(List(Some(1), None, Some(3), Some(4)), (a:Int) => a.toDouble))


  /**
    * Exercise 4.6
    * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
    * Right value.
    */
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for (x <- this; y <- b) yield (f(x, y))
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  /**
    * Exercise 4.7
    * Implement sequence and traverse for Either.
    * These should return the first error that’s encountered, if there is one.
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(List())
    case Right(x) :: xs => sequence(xs) match {
      case Left(e) => Left(e)
      case Right(a) => Right(x :: a)
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(List())
    case x :: xs => traverse(xs)(f) match {
      case Left(e) => Left(e)
      case Right(a) => f(x) match {
        case Left(e) => Left(e)
        case Right(xx) => Right(xx :: a)
      }
    }
  }

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(List())
      case x :: xs => (f(x) map2 traverse(xs)(f)) (_ :: _)
    }
  }

}
