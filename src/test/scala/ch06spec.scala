import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop._

/**
 * Created by Liam.M(엄익훈) on 9/14/16.
 */
class ch06spec extends Properties("functional state") {

  import ch06._

  def isDouble(d: Double): Boolean = d >= 0 && d < 1

  property("testPositiveInteger") = forAll { (i: Int) =>
    val (positive, _)= positiveInteger(RNG.simple(i))
    positive >= 0
  }

  property("testDouble") = forAll { (i: Int) =>
    val (dbl, _) = double(RNG.simple(i))
    dbl >= 0 && dbl < 1
  }

  property("testIntDouble") = forAll { (i: Int) =>
    val ((int, dbl), _) = intDouble(RNG.simple(i))
    isDouble(dbl) && int.isInstanceOf[Int]
  }

  property("testDoubleInt") = forAll { (i: Int) =>
    val ((dbl, int), _) = doubleInt(RNG.simple(i))
    isDouble(dbl) && int.isInstanceOf[Int]
  }

  property("testDouble3") = forAll { (i: Int) =>
    val ((d1, d2, d3), _) = double3(RNG.simple(i))
    isDouble(d1) && isDouble(d2) && isDouble(d3)
  }

  property("testInts") = forAll(Gen.posNum[Int]) { (i: Int) =>
    val (list, _) = ints(i)(RNG.simple(i))
    list.size == i
  }

  property("testIntsWithSequence") = forAll(Gen.posNum[Int]) { (i: Int) =>
   val (list, _) = intsWithSequence(i)(RNG.simple(i))
   val (list2, _) = ints(i)(RNG.simple(i))
   list == list2
  }

  property("positiveIntegerWithFlatMap") = forAll { (i: Int) =>
    val (pos, _) = positiveIntegerWithFlatMap(RNG.simple(i))
    val (pos1, _) = positiveInteger(RNG.simple(i))
    pos == pos1
  }

  property("state") = forAll { (i: Int) =>
    val state: State[RNG, Int] = State(int)
    val (a, rng) = state.run(RNG.simple(i))
    val (b, _) = state.run(rng)
    val state1: State[RNG, (Int, Int)] = for {
      s1 <- state
      s2 <- state
    } yield (s1, s2)

    val ((c, d), _) = state1.run(RNG.simple(i))
    a == c && b == d
  }
  property("modify") = forAll { (i: Int) =>
    true
  }

  property("candy dispenser") = forAll { (i: Int) =>
    import Machine._
    val inputs = List(Coin, Turn)
    val s: State[Machine, (Int, Int)] = simulateMachine(inputs)
    val machine = Machine(true, 10, i)
    val ((candies, coin), m) = s.run(machine)
    candies == 9 && coin == i + 1
  }

}
