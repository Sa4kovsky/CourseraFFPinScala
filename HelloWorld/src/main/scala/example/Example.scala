package example

object Example extends App{
  import FunSets._

  println("Hello World")

  val setA:FunSet = x => x == 0   //this Set is true only for 5
  val setB:FunSet = y => y == 1000   //this Set is true only for 2
  val setU = union(setA, setB) //setA is parameter s, setB is parameter t

  setU(2)  //'e' is now 2, this returns true
  setU(5)  //'e' is now 5, this returns true
  setU(25) //'e' is now 25, this returns false

  println(forall(setA,setB))
}





