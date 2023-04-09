object Main2 extends App {
  def rollDie(rng: scala.util.Random): Int = rng.nextInt(6)

  val random = scala.util.Random

  def test(rnd: scala.util.Random): Unit = {
    val x = rollDie(rnd)
    println(s"$x")
    assert(x >= 1 && x <= 6, s"x=$x")
  }

  for (_ <- 1 to 100) test(random)
}
