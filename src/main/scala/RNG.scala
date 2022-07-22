import scala.util.Random
trait RNG {
  def nextInt:(Int,RNG)
}

case class SimpleRNG(seed: Long) extends RNG{
  def nextInt: (Int,RNG) ={
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,nextRNG)
  }
}

object RNG{

  type Rand[+A] = RNG => (A,RNG)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match{
  case (i,rng2) => (i % 2 == 0,rng2)}

  def nonNegativeInt(rng: RNG): (Int,RNG) = {
    val(value,cur_rng) = rng.nextInt
    (math.abs(value % Int.MaxValue),cur_rng)
  }

  def double(rng: RNG): (Double,RNG) ={
    val(value,cur_rng) = rng.nextInt
    (math.abs((value.toDouble/Int.MaxValue.toDouble)),cur_rng)
  }

  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val(valueInt,cur_rng1) = nonNegativeInt(rng)
    val(valueDouble,cur_rng2) = double(cur_rng1)
    ((valueInt,valueDouble),cur_rng2)

  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val(value,cur_rng) = intDouble(rng)
    ((value._2,value._1),cur_rng)
  }
  def double3(rng: RNG): ((Double,Double,Double),RNG) = {
    val(valueDouble1,cur_rng1) = double(rng)
    val(valueDouble2,cur_rng2) = double(cur_rng1)
    val(valueDouble3,cur_rng3) = double(cur_rng2)
    ((valueDouble1,valueDouble2,valueDouble3),cur_rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int],RNG) ={
    sequence(List.fill(count)(nonNegativeInt(_)))(rng)
  }

  def mapDouble: Rand[Double] = { map[Int,Double](nonNegativeInt)(a => (a.toDouble/Int.MaxValue.toDouble))}

  def map[A,B](s: Rand[A])(f:A=>B):Rand[B] = rng => {flatMap(s)(a=>b=>(f(a),b))(rng)}

  def map2[A,B,C](ra: Rand[A],rb: Rand[B])(f:(A,B) => C): Rand[C] =
    rng => { flatMap[(A,B),C](s => ((ra(s)._1,rb(ra(s)._2)._1),rb(ra(s)._2)._2))(a => s2 => (f(a._1,a._2),s2))(rng)}

  def transfer[A](fs:List[Rand[A]])(list:List[A]):Rand[List[A]] = rng => {
    fs match{case Nil => (list,rng) case x::y => transfer(y)(x(rng)._1::list)(x(rng)._2)}
  }
  def sequence[A](fs:List[Rand[A]]):Rand[List[A]] = rng => transfer(fs)(Nil)(rng)

  def flatMap[A,B](f: Rand[A])(g: A=> Rand[B]): Rand[B] = rng => {
    val(value,rng2) = f(rng)
    g(value)(rng2)
  }



  def main(a:Array[String]):Unit = {
    val test1 = nonNegativeInt(SimpleRNG(42))
    val test2 = nonNegativeInt(test1._2)
    val test3 = nonNegativeInt(test2._2)

    val test4 = double(SimpleRNG(42))
    val test5 = double(test4._2)
    val test6 = double(test5._2)
    val int :Rand[Int] = _.nextInt
    val int_list = ints(2)(SimpleRNG(42))
    println(int(SimpleRNG(33)))
    int

    test1
    test2
    test3

    test4
    test5
    test6

    int_list

  }
}
