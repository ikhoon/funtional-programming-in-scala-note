import scala.annotation.tailrec

/**
 * Created by Liam.M(엄익훈) on 9/14/16.
 */

object ch06 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt: (Int, RNG) = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }
  }
  // ex.1 Write a function to generate a random positive integer
  def positiveInteger(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    if(int == Int.MinValue) (Int.MaxValue, rng2)
    else (math.abs(int), rng2)
  }

  // ex2. Write a function to generate a double between 0 and 1 not including 1
  def double(rng: RNG): (Double, RNG) = {
    val (int, rng2) = positiveInteger(rng)
    (int.toDouble / Int.MaxValue, rng2)
  }

  // ex3. Write a function to generate an (Int, Double) pair, a (Double, Int) pair
  // and a (Double, Double, Double) 3-tuple.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (dbl, rng3)  = double(rng2)
    ((int, dbl), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (dbl, rng2) = double(rng)
    val (int, rng3) = rng2.nextInt
    ((dbl, int), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // ex.4 Write a function to generate a list of random integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count > 0) {
      val (int, rng2) = rng.nextInt
      val (list, rng3) = ints(count - 1)(rng2)
      (int :: list, rng3)
    }
    else {
      (Nil, rng)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // ex.5 Use map to generate an Int between 0 to n, inclusive
  def positiveMax(n: Int): Rand[Int] =
    map(int)(i => i % (n + 1))

  // ex.6 Use map to reimplement RNG.double in a more elegant way.
  def doubleWithMap: Rand[Double] =
    map(int)(_.toDouble)

  // ex.7 Write map2 and reimplement intDouble and doubleInt
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDoubleWithMap2: Rand[(Int, Double)] =
    map2(int, doubleWithMap)((_, _))

  def doubleIntWithMap2: Rand[(Double, Int)] =
    map2(doubleWithMap, int)((_, _))

  

}
