package functional.part2

object list {

  // simple, just make into a list of one value (from the non determinstic perspective on list, this makes sense)
  def pure[A](v:A):List[A] = List(v)
 
  // simple, just apply the function to all values of the list
  def map[A, B](ma: List[A], f: A => B): List[B] = ma match {
    case Nil => Nil
    // in the buildin list Cons is simply named `::` allowing head::tail instead of Cons(head, tail)
    case h :: t => f(h) :: map(t, f)
  }

  // if we sequence two non deterministic computations the end results is taking all possible 
  // results from the first computation and apply the non deterministic function on all of them
  // and the results is all possible results over all
  def flatMap[A, B](ma: List[A], f: A => List[B]): List[B] = flatten(map(ma, f))
     

  // flat map here is easier to implement in terms of flatten so lets implemnet it
  def flatten[A](ma: List[List[A]]): List[A] = ma match {
    case Nil => Nil
    case h::t => h ++ flatten(t)  // ++ is concatentation of 2 lists
  }

  // all possible values cross product with all possible operations  
  def ap[A, B](ma: List[A], ff: List[A => B]): List[B] = {
    for (
      f <- ff;       
      a <- ma
    ) yield f(a) // implement ap using map and flatMap
  }

  // exactly the same implementation we had for Maybe (again using ap and map)
  def map2[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = ap(b, map(a, f.curried))

  def map3[A, B, C, D](a: List[A], b: List[B], c: List[C], f: (A, B, C) => D): List[D] = ap(c, ap(b, map(a, f.curried)))
  
  // Another useful function that is worth knowing
  def product[A, B](a: List[A], b: List[B]): List[(A, B)] = map2(a, b, (_, _))
}
