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

}
