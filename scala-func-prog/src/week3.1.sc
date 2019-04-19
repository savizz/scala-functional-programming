abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(that: IntSet): IntSet
}

class NoneEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NoneEmpty(elem, left.incl(x), right)
    else if (x > elem) new NoneEmpty(elem, left, right.incl(x))
    else this

  override def union(that: IntSet): IntSet =
    ((left.union(right)).union(that)).incl(elem)


  override def toString: String = s"{$left $elem $right}"

}


class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NoneEmpty(x, new Empty, new Empty)

  override def union(that: IntSet): IntSet = that

  override def toString: String = "."
}


val t1 = new NoneEmpty(4, new Empty, new Empty)
val t2 = t1.incl(10)

val a: Array[NoneEmpty] = Array(new NoneEmpty(1, new Empty, new Empty))
val b: Array[IntSet] = a
b(0) = new Empty
val s: NoneEmpty = a(0)