trait Expr
case class SumClass(e1:Expr,e2: Expr)extends Expr{
  override def toString: String = s"$e1+$e2"
}
case class NumberClass(n:Int) extends Expr{
  override def toString: String = s"$n"

}


object Number {
  def apply(n: Int): NumberClass = new NumberClass(n)
}

object Sum {
  def apply(e1: Expr, e2: Expr): SumClass = new SumClass(e1, e2)
}


def show(e: Expr): String = e match {
  case NumberClass(n) => s"$n"
  case SumClass (l,r) => show(l) + " + " + show(r)
}



show(Sum(Number(1),Number(2)))