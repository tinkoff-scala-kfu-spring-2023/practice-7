object Main4 extends App {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[ Int] = _.nextInt

  val nonNegativeInt: Rand[ Int] = map(int)(_.abs)

  val double: Rand[ Double] = rng => {
    val (int, next) = rng.nextInt
    (int.abs / Int.MaxValue, next)
  }

  def unit[S, A](a: A): Rand[ A] = s => (a, s)

  def nonNegativeEven: Rand[ Int] = map(nonNegativeInt)(i => i - i % 2)

  //region: map2

  def map2[S,A,B,C](ra: Rand[ A], rb: Rand[ B])(f: (A, B) => C): Rand[ C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def both[S,A,B](ra: Rand[ A], rb: Rand[ B]): Rand[ (A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[ (Int, Double)] = both(int, double)
  val randDoubleInt: Rand[ (Double, Int)] = both(double, int)

  //endregion

  //region: nesting

//  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
//    map(nonNegativeInt) { i =>
//      val mod = i % n
//      if (i + (n - 1) - mod >= 0) mod else nonNegativeLessThan(n)(rng)
//    }
//  }

  def nonNegativeLessThan(n: Int): Rand[ Int] = flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def flatMap[A,B](f: Rand[ A])(g: A => Rand[ B]): Rand[ B] = rng => {
    val (a, next) = f(rng)
    g(a)(next)
  }

  def map[A, B](s: Rand[ A])(f: A => B): Rand[ B] =
    flatMap(s)(a => unit(f(a)))

  //endregion
}
