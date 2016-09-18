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
      val (list: List[Int], rng3) = ints(count - 1)(rng2)
      (list ::: List(int), rng3)
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

  // ex.8 Implement sequence, for combining a List of transition into single transition
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs.foldLeft((Nil: List[A], rng)) {
      case ((xs, rng2), rand) =>
        val (x, rng3) = rand(rng2)
        (x :: xs, rng3)
    }

  // Use it to reimplement the ints function you write before
  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // ex.9 Implement flatMap, then use it to reimplement positiveInt
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def positiveIntegerWithFlatMap: Rand[Int] =
    flatMap(int) {
      i => if(i == Int.MinValue) unit(Int.MaxValue) else unit(math.abs(i))
    }

  // ex.10 Reimplement map and map2 in terms of flatMap
  def `map'`[A, B](fa: Rand[A])(f: A => B): Rand[B] =
    flatMap(fa)(a => unit(f(a)))

  def `'map2'`[A, B, C](fa: Rand[A], fb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(fa) { a =>
      flatMap(fb) { b =>
        unit(f(a, b))
      }
    }

  // They are general-purpose functions for working with state actions.
//  def map[S, A, B](fa: S => (A, S))(f: A => B): S => (B, S)


  // state action
//  type State[S, +A] = S => (A, S)

  // We might even want to write it as its own class, wrapping the underlying function like this:
//  case class `State'`[S, A](run: S => (A, S))

//  type `Rand'`[A] = State[RNG, A]



}

// ex.11 Generalize the functions unit, map, map2, flatMap and sequence
case class State[S, A](run: S => (A, S)) {
  import State.unit

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](s1 => {
      val (a, s2) = run(s1)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))


}

object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def map2[S, A, B, C]
    (sa: State[S, A], sb: State[S, B])
    (f: (A, B) => C): State[S, C] =
    sa.flatMap{ a =>
      sb.flatMap { b =>
        unit(f(a, b))
      }
    }

  def sequence[S, A](xsa: List[State[S, A]]): State[S, List[A]] =
    xsa.foldLeft(unit[S, List[A]](Nil)) {
      case (sxs, sa) =>
        sa.flatMap { a =>
          sxs.flatMap { xs =>
            unit(a :: xs)
          }
        }
    }

  // ex.12 Come up with the signatures for the get and set, then write their implementations
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

// ex.13 To gain experience with the use of State, implement a simulation of a simple candy dispenser
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  import State._
  /**
  * Rules
  * 1. Insert a coin into a locked machine will cause it to unlock if there is any candy left.
  * 2. Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  * 3. Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  * 4. A machine that is out of candy ignores all inputs.
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def dispense(i: Input) = modify { (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s                 // rule 4
          case (Coin, Machine(false, _, _)) => s          // rule 3
          case (Turn, Machine(true, _, _)) => s           // rule 3
          case (Turn, Machine(false, candies, coins)) =>
            Machine(true, candies - 1, coins)             // rule 2
          case (Coin, Machine(true, candies, coins)) =>   // rule 1
            Machine(false, candies, coins + 1)
        }
      }

    val list: List[State[Machine, Unit]] = inputs reverseMap dispense
    val state: State[Machine, List[Unit]] = sequence(list)

    for {
      _ <- state
      s <- get
    } yield (s.candies, s.coins)
  }
}
