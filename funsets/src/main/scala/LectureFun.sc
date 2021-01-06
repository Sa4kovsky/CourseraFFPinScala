

//Lecture 2.1 - Lecture 2.4
def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if(x == 0) 1 else fact(x - 1)



//функции высшего порядка

def sumInts(a: Int, b: Int): Int = {
  if(a > b) 0 else a + sumInts(a + 1, b)
}

def sumCubes(a: Int, b: Int): Int =
  if(a > b) 0 else cube(a) + sumCubes(a + 1, b)

def sumFactory(a: Int, b: Int): Int =
  if(a > b) 0 else fact(a) + sumFactory(a + 1, b)



// сокращаем повторяющийся код методы

def sum(f: Int => Int, a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum(f, a + 1, b)
}

def sumInts1(a: Int, b: Int): Int =    sum(id, a, b)
def sumCubes(a: Int, b: Int): Int =    sum(cube, a, b)
def sumFactory(a: Int, b: Int): Int =  sum(fact, a, b)



// anonymous functions

def sumInts1(a: Int, b: Int): Int =    sum(x => x, a, b)
def sumCubes(a: Int, b: Int): Int =    sum(x => x * x * x, a, b)

def sumTailRecursion(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}



// currying

def sum2(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum2(f)(a + 1, b)
}

def sumInts2(a: Int, b: Int): Int =  sum2(id)(a, b)
def sumCubes2(a: Int, b: Int): Int = sum2(cube)(a, b)
def sumFactory(a: Int, b: Int): Int =  sum2(fact)(a, b)

//Task
def product(f: Int => Int)(a: Int, b: Int): Int ={
  if(a > b) 1
  else f(a) * product(f)(a + 1,b)
}

def fact(n: Int): Int = product(x => x)(1, n)

//combine sum and product
def  mapReduce(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def product1(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x * y, 1)(a, b)
def sum1(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x + y, 0)(a, b)

product1(x=>x * x)(3,4)
sum1(x=>x)(3,4)


//Lecture 2.5 - Functions and Data
class Rational(x: Int, y: Int){
  def numer = x
  def denom = y
}

val x = new Rational(1, 2)
x.numer
x.denom

def addRational(r: Rational, s: Rational): Rational = {
  new Rational(
    r.numer * s.denom + s.numer * r.denom,
    r.denom * s.denom
  )
}

def  makeString(r: Rational) = r.numer + "/" + r.denom

makeString(addRational(new Rational(1, 2), new Rational(2, 3)))

class Rational1(x: Int, y: Int){
  require(y > 0, "нельзя > 0") //ограничение

  def this(x: Int) = this(x, 1) //спец. конструктор

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(x, a % b)

  val numer = x / gcd(x,y)
  val denom = y / gcd(x,y)

  def less(that: Rational1) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational1) = if (this.less(that)) that else this

  def add(that: Rational1) = {
    new Rational1(
    numer * that.denom + that.numer * denom,
      denom * that.denom)
  }

  def neg: Rational1 = new Rational1(-numer, denom)

  def sub(that: Rational1) = add(that.neg)

  override def toString: String = numer + "/" + denom
}

val x1 = new Rational1(1, 2)
val y1 = new Rational1(2, 3)
x1.add(y1)

val x2 = new Rational1(1, 3)
val y2 = new Rational1(5, 7)
val z2 = new Rational1(3, 2)

println(x2.max(y2) )


