// scala builtin Either type
import functional.part1.maybePart4._
import functional.part4.maybeIfM._

type UserId = Int
case class User(id: UserId, name: String, age: Int)

val bobby = User(0, "Bobby", 24)
val sam = User(1, "Sam", 15)

def getUserByName(name: String): Maybe[User] = name.toLowerCase match {
  case "bobby" => Maybe.pure(bobby)
  case "sam" => Maybe.pure(sam)
  case _ => Maybe.nothing()
}

type MovieId = Int
case class Movie(id: MovieId, name: String, minAge: Int)

val spongeBob = Movie(0, "Spong Bob", 5)
val saw = Movie(1, "Saw", 18)

def getMovieByName(name: String): Maybe[Movie] = name.toLowerCase match {
  case "spong bob" => Maybe.pure(spongeBob)
  case "saw" => Maybe.pure(saw)
  case _ => Maybe.nothing()
}

def rentMovie(userName: String, movieName: String): Maybe[(UserId, MovieId)] = {
  val mUser = getUserByName(userName)
  val mMovie = getMovieByName(movieName)
  val ageIsFine = Maybe.map2(mUser, mMovie, (u, m) => u.age >= m.minAge)
  ifM(ageIsFine, Maybe.map2(mUser, mMovie,(u, m) => (u.id, m.id)), Maybe.nothing())
}

rentMovie("bobby", "saw")
rentMovie("sam", "saw")

// A shorter variation, this just a different style of the same thing
def rentMovie2(userName: String, movieName: String): Maybe[(UserId, MovieId)] = {
  val participants = Maybe.map2(getUserByName(userName), getMovieByName(movieName), (_, _))
  val ageIsFine = participants.map{ case (u, m) => u.age >= m.minAge }
  ifM(ageIsFine, participants.map{ case (u, m) => (u.id, m.id) }, Maybe.nothing())
}

rentMovie2("bobby", "saw")
rentMovie2("sam", "saw")
rentMovie2("john", "baby shark")
