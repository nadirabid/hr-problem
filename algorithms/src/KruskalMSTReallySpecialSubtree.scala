object KruskalMSTReallySpecialSubtree {
  case class Edge(u: Int, v: Int, w: Int)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n :: m :: _ = inputIter.next().split(" ").map(_.toInt).toList

    val edges = inputIter
        .take(m)
        .map(_.split(" ").map(_.toInt))
        .foldLeft(List[Edge]()) {
          case (edges, Array(u, v, w)) =>
            Edge(u - 1, v -1, w) :: edges
        }
        .sortWith { (e1, e2) =>
          e1.w < e2.w || (e1.w == e2.w && (e1.u + e1.v < e2.u + e2.v))
        }

    val sets = (0 to n - 1).foldLeft(Vector[Set[Int]]()) { (sets, n) =>
      sets ++ Vector(Set(n))
    }

    println(findWeightOfKruskalMST(edges, sets))
  }

  def findWeightOfKruskalMST(edges: List[Edge],
                             sets: Vector[Set[Int]],
                             w: Int = 0): Int = {
    if (sets.length > 1) {
      val (nextEdgeToAdd, remainingEdges) = nextEdgeToAddToMST(edges, sets)

      val joinedSets = joinSetsByEdge(nextEdgeToAdd, sets)

      findWeightOfKruskalMST(remainingEdges, joinedSets, w + nextEdgeToAdd.w)
    }
    else {
      w
    }
  }

  def nextEdgeToAddToMST(edges: List[Edge], sets: Vector[Set[Int]], prevEdges: List[Edge] = Nil): (Edge, List[Edge]) = {
    edges match {
      case head :: Nil =>
        (head, prevEdges)
      case head :: rest if sets.forall(s => !s.contains(head.u) || !s.contains(head.v)) =>
        (head, prevEdges ::: rest)
      case head :: rest =>
        nextEdgeToAddToMST(rest, sets, prevEdges ::: List(head))
    }
  }

  def joinSetsByEdge(edge: Edge, sets: Vector[Set[Int]]): Vector[Set[Int]] = {
    val setContainingU = sets.find(_.contains(edge.u)).get
    val setContainingV = sets.find(_.contains(edge.v)).get

    sets.filterNot(s => s.contains(edge.u) || s.contains(edge.v)) ++ Vector(setContainingU.union(setContainingV))
  }
}
