package funsets

import org.scalatest.funsuite.AnyFunSuite

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends AnyFunSuite {  
  import FunSets._

  test("Contains Test") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    def mul4(x: Int): Boolean = x % 4 == 0
    def mul6(x: Int): Boolean = x % 6 == 0
  }

  test("Singleton Test") {
    new TestSets {
      assert(contains(s1, 1), "singleton s1 contains 1")
      assert(!contains(s2, 1), "singleton s2 not contains 2")
    }
  }

  test("Union Test") {
    new TestSets {
      val unionS1S2 = union(s1, s2)
      val unionMul4Mul6 = union(mul4, mul6)
      assert(contains(unionS1S2, 1), "s1 union s2 contains 1")
      assert(!contains(unionS1S2, 3), "s1 union s2 not contains 3")
      assert(contains(unionMul4Mul6, 16), "mul4 union mul6 contains 16")
      assert(!contains(unionMul4Mul6, 13), "mul4 union mul6 not contains 13")
    }
  }

  test("Intersect Test") {
    new TestSets {
      val intMul4Mul6 = intersect(mul4, mul6)
      assert(contains(intMul4Mul6, -12), "s1 intersect s2 contains -12")
      assert(!contains(intMul4Mul6, 16), "s1 intersect s2 not contains 16")
    }
  }

  test("Diff Test") {
    new TestSets {
      val diffMult4Mul6 = diff(mul4, mul6)
      assert(contains(diffMult4Mul6, 4), "s1 diff s2 contains 4")
      assert(!contains(diffMult4Mul6, 6), "s1 diff s2 not contains 6")
    }
  }

  test("Filter Test") {
    new TestSets {
      val bigMul4 = filter(mul4, x => x > 100)
      assert(contains(bigMul4, 104), "mul4 and x > 100 contains 104")
      assert(!contains(bigMul4, 102), "mul4 and x > 100 not contains 102")
      assert(!contains(bigMul4, 96), "mul4 and x > 100 not contains 96")
    }
  }

  test("Forall Test") {
    new TestSets {
      val intMul4Mul6 = intersect(mul4, mul6)
      def isMul12(x: Int) = x % 12 == 0
      def isMul24(x: Int) = x % 24 == 0
      assert(forall(intMul4Mul6, isMul12), "mul4 intersect mul6 all satisfy isMul12")
      assert(!forall(intMul4Mul6, isMul24), "mul4 intersect mul6 not all satisfy isMul24")
    }
  }

  test("Exists Test") {
    new TestSets {
      val intMul4Mul6 = intersect(mul4, mul6)
      def isMul3(x: Int) = x % 3 == 0
      def isMul12Plus1(x: Int) = x % 12 == 1
      assert(exists(intMul4Mul6, isMul3), "mul4 intersect mul6 exists isMul3")
      assert(!exists(intMul4Mul6, isMul12Plus1), "mul4 intersect mul6 not exists isMul12Plus1")
    }
  }

  test("Map Test") {
    new TestSets {
      def doubleInt(x: Int) = 2 * x
      val mul8 = map(mul4, doubleInt)
      assert(forall(mul8, x => x % 8 == 0), "mul4 map doubleInt is mul8")
      assert(!contains(mul8, 4), "mul4 map doubleInt not contains 4")
    }
  }
}
