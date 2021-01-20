package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }

  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `times(List('a', 'b', 'a')) =  List(('a', 2), ('b', 1))`: Unit = {
      assertEquals(List(('a', 2), ('b', 1)), times(List('a', 'b', 'a')))
  }

  @Test def `makeOrderedLeafList(List(('a', 2), ('b', 1))) = List(Leaf(b,1), Leaf(a,2))`: Unit = {
      assertEquals(List(Leaf('b', 1), Leaf('a', 2)), makeOrderedLeafList(List(('a', 2), ('b', 1))))
  }

  @Test def `singleton(List('a')) =  true`: Unit = {
      assertEquals(true, singleton(List(Leaf('a',1))))
  }

  @Test def `singleton(List(Leaf('a',1), Leaf('b',1))) =  false`: Unit = {
      assertEquals(false, singleton(List(Leaf('a',1), Leaf('b',1))))
  }

  @Test def `singleton(List()) =  false`: Unit = {
      assertEquals(false, singleton(List()))
  }

  @Test def `until(singleton,combine)(List(t1)) = List(Fork(Leaf('a', 2),Leaf('b', 3),List('a', 'b'), 5))`: Unit =
    new TestTrees {
      assertEquals(List(Fork(Leaf('a', 2),Leaf('b', 3),List('a', 'b'), 5)), until(singleton,combine)(List(t1)) )
    }

  @Test def `createCodeTree `: Unit =
    new TestTrees {
      assertEquals(Fork(Fork(Leaf('q', 1), Leaf('v', 1), List('q', 'v'), 2), Leaf('c', 2), List('q', 'v', 'c'),4), createCodeTree(List('q','v','c','c')))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))

  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals(List(0,1),  encode(t1)("ac".toList))
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
