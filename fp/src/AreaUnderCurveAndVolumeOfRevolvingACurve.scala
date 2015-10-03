object AreaUnderCurveAndVolumeOfRevolvingACurve {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("fp/input.txt").getLines()

    val coefficients = inputIter.next().split(" ").map(_.toInt).toList
    val powers = inputIter.next().split(" ").map(_.toInt).toList
    val l :: r :: Nil = inputIter.next().split(" ").map(_.toInt).toList

    //println(f(coefficients, powers, 2))
    println(summation(f, r, l, coefficients, powers))
    println(summation(area, r, l, coefficients, powers))
  }

  // This function will be used while invoking "Summation" to compute
  // The area under the curve.
  def f(coefficients:List[Int],powers:List[Int],x:Double):Double = {
    0.001 * coefficients.zip(powers).foldLeft(0.0){ case (sum, (a, b)) =>
      sum + a*scala.math.pow(x, b)
    }
  }

  // This function will be used while invoking "Summation" to compute
  // The Volume of revolution of the curve around the X-Axis
  // The 'Area' referred to here is the area of the circle obtained
  // By rotating the point on the curve (x,f(x)) around the X-Axis
  def area(coefficients:List[Int],powers:List[Int],x:Double):Double = {
    //Fill Up this function body
    // To compute the area of the circle on revolving the point
    // (x,f(x)) around the X-Axis
    // For the given coefficients, powers and value of x

    val r = coefficients.zip(powers).foldLeft(0.0){ case (sum, (a, b)) =>
      sum + a*scala.math.pow(x, b)
    }

    scala.math.Pi * scala.math.pow(r, 2) * 0.001
  }

  // This is the part where the series is summed up
  // This function is invoked once with func = f to compute the area         // under the curve
  // Then it is invoked again with func = area to compute the volume
  // of revolution of the curve
  def summation(
                 func:(List[Int],List[Int],Double)=>Double,
                 upperLimit:Int,
                 lowerLimit:Int,
                 coefficients:List[Int],
                 powers:List[Int]):Double = {
    (lowerLimit.toDouble to upperLimit.toDouble by 0.001)
      .foldLeft(0.0) (_ + func(coefficients, powers, _))
  }
}
