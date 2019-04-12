def sum(f: Int => Int)(a:Int, b:Int):Int =

  if(a>b) 0
  else f(a)+sum(f)(a+1,b)



def product(f:Int => Int)(a:Int, b:Int):Int =
  if (a>b) 1
  else f(a) * product(f)(a+1,b)

product(x=>x)(3,4)

def mapReduce(f:Int => Int,g:(Int,Int) => Int, zero:Int)(a:Int, b:Int):Int =
  if (a>b) zero
  else g(f(a),mapReduce(f,g,zero)(a+1,b))


def productG(f:Int => Int)(a:Int, b:Int):Int =
  mapReduce(f,(x:Int, y:Int) =>x*y,1)(a,b)

def factorial(n:Int) =
  product(x =>x)(1,n)

println("the result is: "+product(x => x)(3,5))