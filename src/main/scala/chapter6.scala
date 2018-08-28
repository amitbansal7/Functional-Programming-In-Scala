object chapter6 extends App{

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * Exercise 6.1
    * Write a function that uses RNG.nextInt to generate a random integer between
    * 0 and Int.maxValue (inclusive). Make sure to handle the corner case when nextInt
    * returns Int.MinValue, which doesn’t have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n - 1) else n, r)
  }


  /**
    * Exercise 6.2
    * Write a function to generate a Double between 0 and 1, not including 1.
    * Note: You can use Int.MaxValue to obtain the maximum positive integer value,
    * and you can use x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (d, r) = nonNegativeInt(rng)
    (d / (Int.MaxValue.toDouble + 1), r)
  }


  /**
    * Exercise 6.3
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
    * and a (Double, Double, Double) 3-tuple. You should be able to reuse the
    * functions you’ve already written.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((x, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((in, d), r) = intDouble(rng)
    ((d, in), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  /**
    * Exercise 6.4
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (x, r) = rng.nextInt
      val (x2, r2) = ints(count - 1)(r)

      (x :: x2, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }


  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
    * Exercise 6.5
    * Use map to reimplement double in a more elegant way.
    */
  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)
  }

  /**
    * Exercise 6.6
    * Write the implementation of map2 based on the following signature.
    * This function takes two actions, ra and rb, and a function f for combining
    * their results, and returns a new action that combines them:
    */

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  /**
    * Exercise 6.7
    * Hard: If you can combine two RNG transitions, you should be able to combine a
    * whole list of them. Implement sequence for combining a List of transitions into
    * a single transition. Use it to reimplement the ints function you wrote before.
    * For the latter, you can use the standard library function List.fill(n)(x) to make a list with x
    * repeated n times.
    */

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  /**
    * Exercise 6.8
    * Implement flatMap, and then use it to implement nonNegativeLessThan.
    */

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { x =>
      val mod = x % n
      if (x + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  /**
    * Exercise 6.9
    * Reimplement map and map2 in terms of flatMap. The fact that this is possible is
    * what we’re referring to when we say that flatMap is more powerful than map and map2.
    */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


  /**
    * Exercise 6.10
    * Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
    * on the State case class where possible. Otherwise you should put them in a State companion object.
    */

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  object State {
    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    //TODO
  }

  /**
    * Exercise 6.11
    *
    */

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies:Int, coins: Int)

  //TODO

}
