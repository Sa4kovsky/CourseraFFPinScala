def times(chars: List[Char]): List[(Char, Int)] = chars match {
  case Nil => Nil
  case e :: t => (e, chars.count(_ == e)):: times(chars.filterNot(e == _))
}

times(List('a' , 'b' , 'c' , 'b' , 'a' , 'a'))