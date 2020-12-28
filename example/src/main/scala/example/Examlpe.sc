def sum(xs: List[Int]): Int = {
  def summary(xs: List[Int], acc: Int): Int = xs match {
    case Nil => throw new NoSuchElementException
    case h :: Nil => acc + h
    case h :: t => summary(t, acc + h)
  }
  summary(xs, 0)
}

sum(List(1,2,3,4,5,6,7,8,9))


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

max(List())