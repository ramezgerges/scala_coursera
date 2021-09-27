package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal(ca ?=> Math.pow(b(), 2) -  4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      if delta() < 0 then Set()
      else
        val root1 = (-b() + Math.sqrt(delta())) / (2 * a())
        val root2 = (-b() - Math.sqrt(delta())) / (2 * a())
        if root1.compare(root2) == 0 then Set(root1) else Set(root1, root2)
    }
