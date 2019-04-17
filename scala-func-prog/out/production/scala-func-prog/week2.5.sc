class Rational(x: Int, y: Int){
  def numer = x
  def denum = y

  def add (that:Rational):Rational =

    new Rational((this.denum *  that.numer)+(this.numer * that.denum), this.denum * that.denum)

  def neg = new Rational(-numer,denum)

  def sub(that:Rational):Rational =
    add(that.neg)



  override def  toString() = s"$x/$y"

}


val x= new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)


x.sub(y).sub(z)





