package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val s7 = singletonSet(7)
    val s8 = singletonSet(8)
    val s9 = singletonSet(9)
    val s10 = singletonSet(10)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      Assert.assertEquals(false, contains(s1, 2))
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `return all elements that are both in s or t`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val t = union(union(union(s2, s4), s5), s6)
      Assert.assertEquals( true ,contains(intersect(s,t),2))
    }
  }

  @Test def `return all elements of s that are not in t`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)
      val t = union(union(union(s2, s4), s5), s6)
      Assert.assertEquals( true ,contains(diff(s,t),1))
    }
  }

  @Test def `return the subset of s for which p => p/2 holds`: Unit = {
    new TestSets {
      val t = union(union(union(union(union(union(s2, s4), s5), s6), s7), s8), s9)
      Assert.assertEquals( true ,contains(filter(t,p => p/2 == 1),2))
    }
  }

  @Test def `Returns whether all bounded integers within s satisfy p`: Unit = {
    new TestSets {
      val t = union(union(union(union(union(union(s2, s4), s5), s6), s7), s8), s9)
      Assert.assertEquals(true, forall(t,x => x >= 0))
    }
  }

  @Test def `Returns whether there exists a bounded integer`: Unit = {
    new TestSets {
      val t = union(union(union(union(union(union(s2, s4), s5), s6), s7), s8), s9)
      Assert.assertEquals( true, exists(t,x=> x%2 == 2))
    }
  }
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
