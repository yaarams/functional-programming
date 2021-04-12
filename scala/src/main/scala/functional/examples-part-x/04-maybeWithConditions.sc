// scala builtin Either type
import functional.part1.maybePart4.*

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


def rentMovie(userName: String, movieName: String): Maybe[(UserId, MovieId)] =
  for (
    user <- getUserByName(userName);
    movie <- getMovieByName(movieName)
  ) yield {
    if (user.age >= movie.minAge) {
      (user.id, movie.id)
    } else {
      ??? // We can't really do that, this part will beinside a map without the syntactic sugar
    }
  }


rentMovie("bobby", "saw")
rentMovie("sam", "saw")

// asme as rentMovie but without the syntactic sugar
def rentMovie2(userName: String, movieName: String): Maybe[(UserId, MovieId)] =
  getUserByName(userName).flatMap(user => getMovieByName(movieName).map(movie =>
    // map can not change the structure, so it can't really suddently take Just values and turn the into None
    if (user.age >= movie.minAge) {
      (user.id, movie.id)
    } else {
      ??? // We can't really do that, this part will be inside a map without the syntactic sugar
    }
  ))

