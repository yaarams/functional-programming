// scala builtin Either type
import functional.part1.maybePart4._
import functional.part3.maybeIfM._

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


import functional.part3.maybeWithFilter._ // import extension to maybe (adds ability to use if as filter in for-yield )

// This is very close to the syntactic sugar version
def rentMovie(userName: String, movieName: String): Maybe[(UserId, MovieId)] =
  getUserByName(userName).flatMap(user => getMovieByName(movieName).withFilter(m => user.age >= m.minAge).map(movie =>
      (user.id, movie.id)
  ))

rentMovie("bobby", "saw")
rentMovie("sam", "saw")
rentMovie("john", "baby shark")


// the syntactic sugar with an if guard, which translates into a call to withFilter
def rentMovie2(userName: String, movieName: String): Maybe[(UserId, MovieId)] =
  for (
    user <- getUserByName(userName);
    movie <- getMovieByName(movieName) if user.age >= movie.minAge
  ) yield (user.id, movie.id)

rentMovie2("bobby", "saw")
rentMovie2("sam", "saw")
rentMovie2("john", "baby shark")

