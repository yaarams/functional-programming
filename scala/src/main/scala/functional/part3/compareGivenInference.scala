package functional.part3

import functional.part3.compareGiven.Comp

object compareGivenInference {

  // We use a strange name to not collide with anything in the standard lib
  trait Comp[A] {
    def compare(x: A, y: A): Int
  }

  //val intOrdering = new Comp[Int] {
  given Comp[Int] with {
    def compare(x: Int, y: Int): Int = x - y
  }

  //val stringOrdering = new Comp[String] {
  given Comp[String] with {
    def compare(x: String, y: String): Int =
      // view will convert the string into a lazy sequence of chars, so if the first character
      // is different we wont evaluate all the other caracters in the string
      x.view.zip(y).map{case (xc, yc) => xc - yc}.find(_ != 0).getOrElse(0)
  }

  given[A, B](using ca : Comp[A], cb: Comp[B]): Comp[(A, B)] with {
    def compare(x: (A, B), y: (A, B)): Int = {
      val ord1 = ca.compare(x._1, y._1)
      if(ord1 == 0) {
        cb.compare(x._2, y._2)
      } else {
        ord1
      }
    }
  }

  given[A, B, C](using ca : Comp[A], cb: Comp[B], cc: Comp[C]): Comp[(A, B, C)] with {
    def compare(x: (A, B, C), y: (A, B, C)): Int = {
      val ord1 = ca.compare(x._1, y._1)
      if(ord1 == 0) {
        val ord2 = cb.compare(x._2, y._2)
        if (ord2 == 0) {
          cc.compare(x._3, y._3)
        } else {
          ord2
        }
      } else {
        ord1
      }
    }
  }
}
