import java.util.NoSuchElementException

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}


class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.Head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](index: Int, list: List[T]): T =
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (index == 0) list.head
  else nth(index - 1, list.tail)


val list1 = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil[Int])))
val res = nth(2, list1)

object List {

  def apply[T](x: T, y: T): List[T] = new Cons[Int](x, new Cons[Int](y, new Nil))
}

val x = List(1, 2)
println(x)


