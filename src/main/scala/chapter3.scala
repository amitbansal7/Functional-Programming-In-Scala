import scala.annotation.tailrec

object chapter3 extends App{

  //Exercise 3.1
  /**
    * ans = 3
    */


  /**
    * Exercise 3.2
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time. What are different choices you
    * could make in your implementation if the List is Nil? We’ll return to this
    * question in the next chapter.
    */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil     => Nil
    case _ :: xs => xs
  }

  println(tail(List(1, 2, 3, 4, 5)))


  /**
    * Exercise 3.3
    * Using the same idea, implement the function setHead for replacing the
    * first element of a List with a different value.
    */
  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Nil     => List(a)
    case _ :: xs => a :: xs
  }

  println(setHead(12, List(1, 2, 3, 4, 5)))

  /** Exercise 3.4
    * Generalize tail to the function drop, which removes the first n elements from a list.
    * Note that this function takes time proportional only to the number of elements being
    * dropped—we don’t need to make a copy of the entire List.
    * def drop[A](l: List[A], n: Int): List[A]
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(l.tail, n - 1)
  }

  println(drop(List(1, 2, 3, 4, 5, 6), 3))


  /** Exercise 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    * def dropWhile[A](l: List[A], f: A => Boolean): List[A]
    *
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      f(x) match {
        case true  => dropWhile(xs, f)
        case false => x :: xs
      }
  }

  println(dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a <= 10))
  println(dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a <= 3))

  /**
    * Exercise 3.6
    * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can’t this function be implemented in constant time like tail?
    * def init[A](l: List[A]): List[A]
    */
  def init[A](l: List[A]): List[A] = l match {
    case x :: Nil => Nil
    case x :: xs  => x :: init(xs)
  }

  println(init(List(1, 2, 3, 4)))

  /**
    * 3.3.2 Improving type inference for higher-order functions
    */
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      f(x) match {
        case true  => dropWhile2(xs)(f)
        case false => x :: xs
      }
  }

  println(dropWhile2(List(1, 2, 3, 4, 5))(_ <= 3))

  /**
    * Exercise 3.9
    * Compute the length of a list using foldRight. def length[A](as: List[A]): Int
    */
  def length[A](as: List[A]): Int =
    as.foldRight(0)((_, a) => a + 1)

  println(length(List(1, 2, 3, 4)))

  /**
    * Exercise 3.10
    * Our implementation of foldRight is not tail-recursive and will result in a
    * StackOver- flowError for large lists (we say it’s not stack-safe).
    * Convince yourself that this is the case, and then write another general list-recursion function,
    * foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter. Here is its
    * signature:
    * def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
    */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  @tailrec
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => foldRight(xs, f(x, z))(f)
  }

  /**
    * Exercise 3.11
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  val l = List(1, 2, 3, 4, 5)

  println("sum  = " + foldLeft(l, 0)(_ + _))

  println("product = " + foldLeft(l, 1)(_ * _))

  println("length  = " + foldLeft(l, 0)((a, _) => a + 1))


  /**
    * Exercise 3.12
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    */
  def reverse(as: List[Int]): List[Int] =
    as.foldLeft(List[Int]())((a, b) => b :: a)

  println(reverse(l))


  /**
    * Exercise 3.13
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
    * Implementing foldRight via foldLeft is useful because it lets us implement foldRight
    * tail-recursively, which means it works even for large lists without overflow- ing the stack.
    */
  def foldRightViaFoldLeft[A, B](as: List[A], a: B)(f: (A, B) => B): B =
    foldLeft(as, a)((b: B, a: A) => f(a, b))

  def foldLeftViaFoldRight[A, B](as: List[A], a: B)(f: (B, A) => B): B =
    foldRight(as, a)((a, b) => f(b, a))

  println(foldRightViaFoldLeft(List(2, 5, 7), 0)(_ - _))
  println(List(2, 5, 7).foldRight(0)(_ - _))

  println(foldLeftViaFoldRight(List(2, 5, 7), 0)(_ - _))
  println(List(2, 5, 7).foldLeft(0)(_ - _))


  /**
    * Exercise 3.14
    * Implement append in terms of either foldLeft or foldRight.
    */
  def append[A](a: A, as: List[A]): List[A] =
    foldLeft(as, List(a))((a, b) => List(b):::a)

  println(append(4, List(1, 2, 3)))

  /**
    * Exercise: 3.15
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    */
  def concat[A](a: List[List[A]]): List[A] =
    foldLeft(a, Nil:List[A])(_:::_)

  println(concat(List(List(1,2), List(3,4))))


  /**
    * Exercise 3.16
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    */
  def addOne(l: List[Int]): List[Int] = {
    def solve(ls: List[Int], acc: List[Int]): List[Int] = ls match {
      case Nil => acc
      case x :: xs => solve(xs, acc ::: List(x + 1))
    }

    solve(l, Nil)
  }

  println(addOne(List(1,2,3,4,5)))

  /**
    * Exercise 3.17
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String.
    */
  def toStr(l: List[Double]): List[String] = {
    def solve(ls: List[Double], acc: List[String]): List[String] = ls match {
      case Nil => acc
      case x :: xs => solve(xs, acc ::: List(x.toString))
    }

    solve(l, List())
  }

  println(List(1d, 2d, 3d, 4d, 5d))

  /**
    * Exercise 3.18
    * Write a function map that generalizes modifying each element in a list
    * while maintain- ing the structure of the list.
    */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    def solve(l: List[A], f: A => B, acc: List[B]): List[B] = l match {
      case Nil => acc
      case x :: xs => solve(xs, f, acc ::: List(f(x)))
    }

    solve(as, f, List())
  }

  println(map(List(2, 4, 6, 8, 10))(x => x / 2))

  /**
    * Exercise 3.19
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int].
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x :: xs => f(x) match {
      case true => x :: filter(xs)(f)
      case false => filter(xs)(f)
    }
  }

  println(filter(List(1, 2, 3, 4, 5, 6, 7))(x => x % 2 == 0))

  /**
    * Exercise 3.20
    * Write a function flatMap that works like map except that the function given
    * will return a list instead of a single result, and that list should be inserted
    * into the final resulting list. Here is its signature:
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case x :: xs => f(x) ::: flatMap(xs)(f)
  }

  println(flatMap(List(1, 2, 3))(x => List(x, x)))

  /**
    * Exercise 3.21
    * Use flatMap to implement filter.
    */
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) =>
      f(a) match {
        case true => List(a)
        case false => List()
    })
  }

  println(filterUsingFlatMap(List(1, 2, 3, 4, 5, 6, 7))(x => x % 2 == 0))

  /**
    * Exercise 3.22
    * Write a function that accepts two lists and constructs a new list by adding correspond- ing elements.
    * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def add(al: List[Int], bl: List[Int]): List[Int] = {
    def solve(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = (a, b) match {
      case (Nil, Nil) => acc
      case (Nil, y :: ys) => solve(Nil, ys, acc ::: List(y))
      case (x :: xs, Nil) => solve(xs, Nil, acc ::: List(x))
      case (x :: xs, y :: ys) => solve(xs, ys, acc ::: List(x + y))
    }

    solve(al, bl, List())
  }

  println(add(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)))

  /**
    * Exercise 3.23
    * Generalize the function you just wrote so that it’s not specific to integers or addition.
    * Name your generalized function zipWith.
    */
  def zipWith[A, B, C](al: List[A], bl: List[B], f:(A, B) => C):List[C] = (al, bl) match {
    case (x::xs, y::ys) => f(x, y) :: zipWith(xs, ys, f)
    case _ => Nil
  }

  println(zipWith(List(1,2,3,4), List(1,2,3,4),((a:Int, b:Int) => a*b)))

  /**
    * Exercise 3.24
    * Hard: As an example, implement hasSubsequence for checking whether a List con- tains another
    * List as a subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4)
    * as subsequences, among others. You may have some difficulty finding a concise purely functional
    * implementation that is also effi- cient. That’s okay. Implement the function however comes most naturally.
    * We’ll return to this implementation in chapter 5 and hopefully improve on it. Note: Any two values
    * x and y can be compared for equality in Scala using the expression x == y.
    */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def checkFrom[A](a: List[A], sub: List[A]): Boolean = {
      if (a.size < sub.size) false
      else {
        (a, sub) match {
          case (_, Nil) => true
          case (x :: _, y :: _) if (x != y) => false
          case (_ :: xs, _ :: ys) => checkFrom(xs, ys)
        }
      }
    }

    sup match {
      case Nil => false
      case xs if (checkFrom(xs, sub)) => true
      case _ :: xs => hasSubsequence(xs, sub)
    }
  }

  println(hasSubsequence(List(1,2,3,4), List(4)))

  //Tree
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  /**
    * Exercise 3.25
    * Write a function size that counts the number of nodes (leaves and branches) in a tree.
    */

  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  val tree = new Branch[Int](new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)), new Branch[Int](new Leaf[Int](3), new Branch[Int](new Leaf[Int](4), new Leaf[Int](4))))
  println(size(tree))

  /**
    * Exercise 3.26
    * Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)
    */
  def maximum(root: Tree[Int]):Int = root match {
    case Leaf(a:Int) => a
    case Branch(l, r) => maximum(l) max (maximum(r))
  }
  println(maximum(tree))

  /**
    * Exercise 3.27
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth(root: Tree[Int]):Int = root match {
    case Leaf(a:Int) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  println(depth(tree))

  /**
    * Exercise 3.28
    * Write a function map, analogous to the method of the same name on List,
    * that modifies each element in a tree with a given function.
    */
  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  println(map(tree)(_.toDouble))

  /**
    * Exercise 3.29
    * Generalize size, maximum, depth, and map, writing a new function fold
    * that abstracts over their similarities. Reimplement them in terms of
    * this more general function. Can you draw an analogy between this fold
    * function and the left and right folds for List?
    */
  def fold[A, B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold(root: Tree[Int]): Int = {
    fold(root)(_ => 1)(1 + _ + _)
  }

  println(sizeViaFold(tree))

  def maxViaFold(root: Tree[Int]): Int = {
    fold(root)(a => a)(_ max _)
  }

  println(maxViaFold(tree))


  def depthViaFold[A](root: Tree[A]): Int = {
    fold(root)(_ => 0)((l, r) => 1 + (l max r))
  }

  println(depthViaFold(tree))
}
