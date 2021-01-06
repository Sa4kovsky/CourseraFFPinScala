package example

trait FunSets {
  type FunSet = Int => Boolean

  def singletonSet(elem: Int): FunSet = (_:Int) == elem

  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  def union(s: FunSet, t: FunSet): FunSet = (x:Int) => s(x) || t(x)

  def intersect(s: FunSet, t: FunSet): FunSet = x => s(x) && t(x)

  def diff(s: FunSet, t: FunSet): FunSet = x => s(x) || !t(x)

  def filter(s: FunSet, p: Int => Boolean): FunSet = x => s(x) || p(x)

  val bound = 1000

  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < bound) true
      else if (contains(intersect(s,p),a)) false
      else iter(a+1)
    }
    iter(0)
  }

}
object FunSets extends FunSets

/*
    def union() I'm defining a method that I'll call union.
    (s: Set, t: Set) This method will take 2 parameters that I'll call s and t, both of type Set.
    : Set This method will return a value of typeSet. Hold on...what's a Set?
    type Set = Int => Boolean Ah, OK, Set is a function that takes an Int as
a parameter and returns a Boolean as a result. Got it. Back to the union() method.
    (e: Int) => s(e) || t(e) This is a function that takes a single parameter of type Int.
I'm going to call that parameter e. When this function receives an Int it will be fed to both s and t. s
and t are both type Set, which means that when fed an Int they return a Boolean. So then we'll have 2 Boolean values
and they'll be OR'd together to produce a single Boolean, which matches the definition of a Set (Int in, Boolean out), so we're done.
 */

