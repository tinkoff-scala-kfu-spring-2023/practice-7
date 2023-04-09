object Main3 extends App {

  val rng = SimpleRNG(42)

  //region: wrong randomPair

  def randomPair(rng: RNG): (Int, Int) = {
    val (i1, newRng) = rng.nextInt
    val (i2, _) = newRng.nextInt
    (i1, i2)
  }

  randomPair(rng)

  //endregion

  //region: correct randomPair

  def randomPairCorrect(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  //endregion

  //region: implement

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, next) = rng.nextInt
    (a.abs, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int, next) = nonNegativeInt(rng)
    (int / Int.MaxValue, next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, next) = rng.nextInt
    val (dbl, newNext) = double(next)
    ((int, dbl), newNext)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    ???
  }

  //endregion
}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}