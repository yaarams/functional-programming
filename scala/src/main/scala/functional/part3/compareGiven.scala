package functional.part3

object compareGiven {


  // We use a strange name to not collide with anything in the standard lib
  trait Comp[A] {
    def compare(x: A, y: A): Int
  }

  //val intOrdering = new Comp[Int] {
  given Comp[Int] {
    override def compare(x: Int, y: Int): Int = x - y
  }

  //val stringOrdering = new Comp[String] {
  given Comp[String] {
    override def compare(x: String, y: String): Int =
      // view will convert the string into a lazy sequence of chars, so if the first character
      // is different we wont evaluate all the other caracters in the string
      x.view.zip(y).map{case (xc, yc) => xc - yc}.find(_ != 0).getOrElse(0)
  }


}
