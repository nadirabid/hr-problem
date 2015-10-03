object ConvexHull {
  case class Point(x:Int, y:Int)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("fp/input.txt").getLines()
    val n = inputIter.next().trim.toInt
    val points = inputIter
      .take(n)
      .map(_.split(" ").map(_.toInt) match { case Array(x, y) => Point(x, y) })
      .toVector

    val minX = points.minBy(_.x)
    val maxX = points.maxBy(_.x)

    val minY = points.minBy(_.y)
    val maxY = points.maxBy(_.y)

    val poly = new java.awt.Polygon()
    poly.addPoint(minX.x, minX.y)
    poly.addPoint(maxX.x, maxX.y)
    poly.addPoint(minY.x, minY.y)
    poly.addPoint(maxY.x, maxY.y)

    val partitions = (points.filterNot(p => poly.contains(p.x, p.y)) ++ Vector(minY, maxY))
      .groupBy(locationOfPointRelativeToLine(minX, maxX, _))

    val convexHullRight = findConvexHull(partitions.lift(-1), minX, maxX)
    val convexHullLeft = findConvexHull(partitions.lift(1), minX, maxX)

    val convexHull = Vector(minX) ++ convexHullLeft ++ Vector(maxX) ++ convexHullRight.reverse

    println(convexHull.foldLeft((distanceBetweenPoints(convexHull.last, convexHull.head), convexHull.head)) { case ((perimeter, p), pNext) =>
      (perimeter + distanceBetweenPoints(p, pNext), pNext)
    }._1)
  }

  def findConvexHull(allPoints: Option[Vector[Point]], l1: Point, l2: Point): Vector[Point] = {
    allPoints match {
      case Some(points) if points.nonEmpty =>
        val pointFurthestFromLine = points.maxBy { p =>
          java.awt.geom.Line2D.ptLineDistSq(l1.x, l1.y, l2.x, l2.y, p.x, p.y)
        }

        val poly = new java.awt.Polygon()
        poly.addPoint(l1.x, l1.y)
        poly.addPoint(pointFurthestFromLine.x, pointFurthestFromLine.y)
        poly.addPoint(l2.x, l2.y)

        val s1Location = -1*locationOfPointRelativeToLine(l1, pointFurthestFromLine, l2)
        val s2Location = -1*s1Location

        val remainingPoints = points.filterNot(p => poly.contains(p.x, p.y))

        val left = remainingPoints.filter(s1Location == locationOfPointRelativeToLine(l1, pointFurthestFromLine, _))
        val right = remainingPoints.filter(s2Location == locationOfPointRelativeToLine(l1, pointFurthestFromLine, _))

        findConvexHull(Some(left), l1, pointFurthestFromLine) ++
          Vector(pointFurthestFromLine) ++
          findConvexHull(Some(right), pointFurthestFromLine, l2)
      case _ => Vector()
    }

  }

  def distanceBetweenPoints(p1: Point, p2: Point): Double = {
    math.sqrt(math.pow(p2.x-p1.x,2) + math.pow(p2.y-p1.y, 2))
  }

  def locationOfPointRelativeToLine(p1: Point, p2: Point, v: Point): Int = {
    math.signum((v.x - p1.x)*(p2.y - p1.y) - (v.y - p1.y)*(p2.x - p1.x))
  }
}
