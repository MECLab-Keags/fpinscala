package chapter6

trait RNG {
  def nextInt(): (Int, RNG)
}
object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (x, rng1) = s(rng)
    (f(x), rng1)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  // we can now reimplement intDouble and doubleInt from exercise 6.3 using both :: Rand[A] => Rand[B] => Rand[(A,B)]
  def randIntDouble: Rand[(Int, Double)] = both(int, doubleAsRand)
  def randDoubleInt: Rand[(Double, Int)] = both(doubleAsRand, int)

  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, rngX) = rng.nextInt
    val result =
      if(x == Int.MinValue) Math.abs(x + 1)
      else Math.abs(x)
    (result, rngX)
  }

  // exercise 6.2: generate a Double between 0 and 1 (not including 1)
  def double(rng: RNG): (Double, RNG) = {
    val (x, rngX) = nonNegativeInt(rng)
    (x / (Int.MaxValue.toDouble + 1), rngX)
  }

  // exercise 6.3: generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i,d), rng2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (int, rng2) = rng1.nextInt
    ((d, int), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1,d2,d3), rng3)
  }

  // exercise 6.4: write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((Nil: List[Int], rng))((agg: (List[Int], RNG), _ : Int) => {
      val (xs: List[Int], rng1: RNG) = agg
      val (x, rng2) = rng1.nextInt
      (x :: xs, rng2)
    })
  }

  // example nonNegativeEven that uses the Rand and map
  def nonNegativeEven(rng: RNG): Rand[Int] =
    map(nonNegativeInt)(x => x - 1 % 2)

  // exercise 6.5: use map to reimplement double
  def doubleAsRand: Rand[Double] = {
    map(nonNegativeInt)(x => x.toDouble)
  }

  // exercise 6.6: write a new combinator that can combine two RNG actions into one
  // using a binary rather than unary function
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  // exercise 6.7: write a combinator to reduce a list i.e. List[Rand[A]] => Rand[List[A]]
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, acc) => map2(rand, acc)(_ :: _))

  // example nonNegativeLessThan this shows that map and map2 cant produce an effective random number
  // so we can implement this like the following
  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val (i, rng1) = nonNegativeInt(rng)
    val mod = i % n
    if(i + (n-1) - mod >= 0) (mod, rng1)
    else nonNegativeLessThan(n)(rng)
  }

  // exercise 6.8: instead of the above we can use a flatMap which would not require us to pass the RNG along.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  // exercise 6.9: reimplement map and map2 in terms of flatMap
  def mapAlt[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2Alt[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL+ 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
