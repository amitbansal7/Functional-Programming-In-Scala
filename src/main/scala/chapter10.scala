object chapter10 {

  trait Monoid[A]{
    def op(a1: A, a2: A):A
    def zero:A
  }

  /**
    * Exercise 10.1
    * Give Monoid  instances for integer addition and multiplication as well as the Boolean operators.
    * val intAddition: Monoid[Int]
    * val intMultiplication: Monoid[Int]
    * val booleanOr: Monoid[Boolean]
    * val booleanAnd: Monoid[Boolean]
    */

  val intAddition:Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication:Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero:Int = 1
  }

  val booleanOr:Monoid[Boolean] =  new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero :Boolean = false
  }

  val booleanAnd:Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero:Boolean = true
  }

  /**
    * Exercise 10.2
    * Give a Monoid  instance for combining Option  values
    */

  def optionMonoid[A]:Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero:Option[A] = None
  }

  /**
    * Exercise 10.3
    *  function having the same argument and return type is sometimes called an endofunction.
    *  Write a monoid for endofunctions.
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f.compose(g)
    def zero: A => A = (a: A) => a
  }

  /**
    * Exercise 10.4
    * Use the property-based testing framework we developed in part 2 to implement a property for the monoid laws.
    * Use your prop erty to test the monoids we’ve written. def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop
    */
      //TODO

  /**
    * Exercise 10.5
    * Implement foldMap .
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /**
    * Exercise 10.6
    * Hard:  The foldMap  function can be implemented using either foldLeft  or foldRight .
    * But you can also write foldLeft  and foldRight  using foldMap ! Try it.
    */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  /**
    * Exercise 10.7
    * Implement a foldMap  for IndexedSeq Your implementation should use the strategy of splitting the sequence in two,
    * recursively processing each half, and then adding the answers together with the monoid.
    */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size == 0) m.zero
    else if (v.size == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.size / 2)
      val left = foldMapV(l, m)(f)
      val right = foldMapV(r, m)(f)
      m.op(left, right)
    }
  }

  /**
    * Hard:  Also implement a parallel  version of foldMap  using the library we developed in chapter
    * Hint: Implement par , a combinator to promote Monoid[A]  to a Monoid [Par[A]],
    * and then use this to implement parFoldMap .
    * import fpinscala.parallelism.Nonblocking._
    * def par[A](m: Monoid[A]): Monoid[Par[A]]
    * def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B]
    */
      //TODO


  /**
    * Hard:  Use foldMap  to detect whether a given IndexedSeq[Int]  is ordered.
    * You’ll need to come up with a creative Monoid .
    */
  def ordered(arr: IndexedSeq[Int]): Boolean = {
    val monad = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (a1, a2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }

      def zero: Option[(Int, Int, Boolean)] = None
    }

    foldMapV(arr, monad)(x => Some((x, x, true))).map(_._3).getOrElse(true)
  }


}
