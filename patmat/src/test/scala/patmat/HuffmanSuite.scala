package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger tree t2" ) {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("count charts number") {
    new TestTrees {
      assert(count('z',List('z','a','b','c','z','z','z','x','z')) === 5)
    }
  }

  test("count charts list") {
    new TestTrees {
      assert(makeInner('z',List('z','a','b','c','z','z','z','x','z')) === List(('z',5)))
    }
  }

  test("count charts times") {
    new TestTrees {
      assert(times(List('z','a','b','c','z','z','z','x','z')) === List(('z',5), ('a',1), ('b',1), ('c',1), ('x',1)))
    }
  }



  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("create a tree") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }



  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(createCodeTree(string2Chars("hello, world")) === Fork(Fork(Leaf('o',2),Fork(Leaf('d',1),Fork(Leaf('w',1),Leaf('r',1),List('w', 'r'),2),List('d', 'w', 'r'),3),List('o', 'd', 'w', 'r'),5),Fork(Leaf('l',3),Fork(Fork(Leaf(',',1),Leaf(' ',1),List(',',' '),2),Fork(Leaf('h',1),Leaf('e',1),List('h', 'e'),2),List(',',' ', 'h', 'e'),4),List('l', ',',' ', 'h', 'e'),7),List('o', 'd', 'w', 'r', 'l', ',',' ', 'h', 'e'),12))
    }
  }


  test("decode and french code") {
    new TestTrees {
      assert(decode(frenchCode,secret)==List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l') )
    }
  }
}
