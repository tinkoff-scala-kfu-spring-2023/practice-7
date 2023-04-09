object Main1 extends App {
  def rollDie(random: scala.util.Random): Int = {
    random.nextInt(6)
  }

  def test(random: scala.util.Random): Unit = {
    val x = rollDie(random)
    assert(x >= 1 && x <= 6, s"x=$x")
  }

  for (_ <- 1 to 100) test(new scala.util.Random)
}