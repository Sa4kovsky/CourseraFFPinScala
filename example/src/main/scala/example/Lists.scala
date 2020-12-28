package example

/**
  *
  */
object Lists {

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    def summary(xs: List[Int], acc: Int): Int = xs match {
      case Nil => throw new NoSuchElementException
      case h :: Nil => acc + h
      case h :: t => summary(t, acc + h)
    }
    summary(xs, 0)
  }


  def merge(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => l2
    case (_, Nil) => l1
    case (h1 :: t1, h2 :: t2) =>
      if (h1 > h2) h1 :: merge(l2, t1)
      else h2 :: merge(l1, t2)
  }

  def mergeSort(list: List[Int]): List[Int] = list match {
    case Nil => list
    case h :: Nil => list
    case _ =>
      val (l1, l2) = list.splitAt(list.length / 2)
      merge(mergeSort(l1), mergeSort(l2))
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    def maximum(xs: List[Int], max: Int): Int = xs match {
      case Nil => throw new NoSuchElementException
      case h :: Nil =>
        if(h > max) h
        else max
      case h :: t =>
        if(h > max) maximum(t, h)
        else maximum(t, max)
    }
    maximum(xs, 0)
  }
}
