import scala.collection.mutable

object DegreesOfSeparation {
  class Member() {
    def getId: Int = 0
    def getPetitionsSigned: List[Petition] = List[Petition]()
  }

  class Petition() {
    def getId: Int = 0
    def getSigningMembers: List[Member] = List[Member]()
  }

  // essentially its a graph we have to traverse. members are the nodes, petitions are the edges
  // eg:
  //    one degree of separation: member <-petition-> member
  //    second degree of separation: member <-petition-> member <-petition-> member
  // breath first search to print all members at nth degree of separation

  def listNthDegreeOfSeparation(member: Member): Unit = {
    val membersSeen = scala.collection.mutable.HashSet[Member](member)
    val petitionsSeen = scala.collection.mutable.HashSet[Petition]()

    var nthDegreePetitions = member.getPetitionsSigned // essentially the edges
    var degree = 1

    while (nthDegreePetitions.nonEmpty) {
      val nthDegreeMembers = nthDegreePetitions
        .flatMap { p =>
        petitionsSeen.add(p)
        p.getSigningMembers
      }
        .filter(m => !membersSeen.contains(m))

      println("Members separated by ${degree} degrees from ${member.getId}")

      nthDegreeMembers.foreach { m =>
        membersSeen.add(m)
        print(m.getId)
      }

      print("\n")

      nthDegreePetitions = nthDegreeMembers
        .flatMap(m => m.getPetitionsSigned)
        .filter(p => !petitionsSeen.contains(p))

      degree += 1
    }
  }
}
