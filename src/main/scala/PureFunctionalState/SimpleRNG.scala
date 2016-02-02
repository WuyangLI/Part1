package PureFunctionalState

/**
 * Created by lwy on 02/02/16.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long)  extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed*0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  //ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, r) if (i < 0) => (-i-1, r)
    case t => t
  }

  //ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n.toDouble/(Int.MaxValue.toDouble+1), r)
  }

  //ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case _ => {
      val (n, r1) = rng.nextInt
      val (l, r2) = ints(count-1)(r1)
      (n::l, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)( i => i - i % 2)
  //ex 6.5
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)( i => i.toDouble/(Int.MaxValue+1))
  //ex 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (va, r1) = ra(rng)
      val (vb, r2) = rb(r1)
      (f(va, vb), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, doubleViaMap)
  val randDoubleInt: Rand[(Double, Int)] = both(doubleViaMap, int)

  //ex 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A]))((ra, rl) => map2(ra, rl)((a, l) => a::l))


}
