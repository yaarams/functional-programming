package functional.part3


object compare {

  // We use a strange name to not collide with anything in the standard lib
  trait Comp[A] {
    def compare(x: A, y: A): Int
  } 

  val intOrdering = new Comp[Int] {
    override def compare(x: Int, y: Int): Int = x - y
  }

  val stringOrdering = new Comp[String] {
    override def compare(x: String, y: String): Int = {
      // view will convert the string into a lazy sequence of chars, so if the first character
      // is different we wont evaluate all the other caracters in the string
      x.view.zip(y).map{case (xc, yc) => xc - yc}.find(_ != 0).getOrElse(0)
    }
  }

  // we can implemnet a builder of more complicated constructs using simpler constructs  
  def tupleOrdering[A, B](firstOrdering: Comp[A], secondOrdering: Comp[B]): Comp[(A, B)] = new Comp[(A, B)] {
   override def compare(x: (A, B), y: (A, B)): Int = {
     val cmp1 = firstOrdering.compare(x._1, y._1)
     if(cmp1 == 0) {
       secondOrdering.compare(x._2, y._2)
     } else {
       cmp1
     }
   }
 }
  
}
