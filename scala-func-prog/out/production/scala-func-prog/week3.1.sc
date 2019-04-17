abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean
}

class NoneEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true

  override def incl(x: Int): IntSet =
    if (x < elem) new NoneEmpty(elem, left.incl(x), right)
    else if (x > elem) new NoneEmpty(elem, left, right.incl(x))
    else this

  override def toString: String = s"{$left $elem $right}"

}


class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NoneEmpty(x, new Empty, new Empty)

  override def toString: String = "."
}


val t1 = new NoneEmpty(4, new Empty, new Empty)
val t2 = t1.incl(10)