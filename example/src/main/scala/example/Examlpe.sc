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
  maximum(xs, Int.MinValue)
}

max(List(1,2,2,250))