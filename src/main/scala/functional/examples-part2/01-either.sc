// scala builtin Either type

type UserId = Int
type Address = String

case class Fail(msg: String)
type CanFail[+A] = Either[Fail, A]

def getUserIDFromName(name: String): CanFail[UserId] = name match {
  // imagine we actually call the DB here
  case "david" => Right(0)
  case "marry" => Right(1)
  case "sam" => Right(2)
  case _ => Left(Fail(s"User with name:$name not found"))
}

def getAddressById(id: UserId): CanFail[Address] = id match {
  case 0 => Right("Haifa")
  // case 1 is missing (user has no address in the system)
  case 2 => Right("Tel-Aviv")
  case _ => Left(Fail(s"Could not locate address for id: $id"))
}

getUserIDFromName("jim").flatMap(id => getAddressById(id))

// easy to combine into more complex operations (sequencing) with for-yield
def getAddressByName(name: String): CanFail[Address] =
  for (
    userId <- getUserIDFromName(name);
    address <- getAddressById(userId)
  ) yield address


getAddressByName("david")
getAddressByName("jim")
getAddressByName("marry")



