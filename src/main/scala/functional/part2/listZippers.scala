package functional.part2

object listZippers {

  case class ZipperState[A](
      before: List[A], // the values before the current in reversed order
      after: List[A]
  )

  case class ListZipper[A, B](f: ZipperState[A] => Option[(B, ZipperState[A])]) {
    
    def apply(s: ZipperState[A]): Option[(B, ZipperState[A])] = f(s)

    def map[C](f: B => C): ListZipper[A, C] = ListZipper.map(this, f)

    def flatMap[C](f: B => ListZipper[A, C]): ListZipper[A, C] = ListZipper.flatMap(this, f)
  }

  object ListZipper {
    
    def pure[A, B](v: B): ListZipper[A, B] = ListZipper(s => {
      Some((v, s))
    })
    
    def map[A, B, C](zipper: ListZipper[A, B], f: B => C): ListZipper[A, C] = ListZipper(s => {
      zipper(s).map{case (r, s1) => (f(r), s1)}
    })
   
    def flatMap[A, B, C](zipper: ListZipper[A, B], f: B => ListZipper[A, C]): ListZipper[A, C] = ListZipper(s => {
        zipper(s).flatMap{case (r, s1) => f(r)(s1)}
    })
    
  }
    
  def moveRight[A]: ListZipper[A, Unit] = ListZipper(s => s.after.tail match {
    case Nil => None
    case h :: t =>  // pattern matching can destruct on custom operators using a special function calle unapply
      Some(((), ZipperState(h :: s.before, t)))  // the empty tuple is the only value in the Unit type
  })
 
  def insert[A](v: A): ListZipper[A, Unit] = ListZipper(s => {
    Some((), s.copy(after = v :: s.after))
  })
 
  def moveToStart[A]: ListZipper[A, Unit] = ListZipper(s => {
    Some((), ZipperState(List.empty, s.before.reverse ::: s.after)) // can implement it more efficiently here
  })
 
 
  def insertAfterSecond[A](v: A): ListZipper[A, Unit] = 
    for(
      _ <- moveRight;
      _ <- moveRight;
      _ <- insert(v)
    ) yield ()
  

  // scala 3 syntax to add methods to existing classes (in scala 2 this can be done with implicit classes conversions)
  extension [A](lst: List[A]) {
    def runZipper(zipper: ListZipper[A, Unit]): Option[List[A]] = 
      zipper.flatMap(_ => moveToStart)(ZipperState(List.empty, lst)).map{case (r, s) => s.after}
  }
  
}
