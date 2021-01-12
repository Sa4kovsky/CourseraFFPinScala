package Week_3.Task1

object Main extends App {
  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1 incl 4 incl 5 incl 10 incl 1 incl 9 incl 3
  print(t2.toString())
}
