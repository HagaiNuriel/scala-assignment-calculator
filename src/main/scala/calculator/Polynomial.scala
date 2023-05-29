package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // first did it this way, and this doesn't work. decided to leave it
    // here in case maybe you will have some insight as to
    // why this prevents the new signal to be properly updated (according to the tests)
//    Signal {
//      val aVal = a()
//      val bVal = b()
//      val deltaVal = delta()
//
//      if (deltaVal < 0) Set()
//      else {
//        Set(
//          (-1 * bVal + math.sqrt(deltaVal)) / 2 * aVal,
//          (-1 * bVal - math.sqrt(deltaVal)) / 2 * aVal)
//      }
//    }

    val negB = Signal(-1 * b())
    val doubleA = Signal(2 * a())
    val sqrtDelta = Signal(math.sqrt(delta()))

    Signal {
      if (delta() < 0) Set()
      else {
        Set(
          (negB() + sqrtDelta()) / doubleA(),
          (negB() - sqrtDelta()) / doubleA())
      }
    }
  }
}
