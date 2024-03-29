import prob.{FailedCase, SuccessCount}
import scala.::

object prob{
  type FailedCase = String
  type SuccessCount = Int
}


trait Prob {
  def check: Either[(FailedCase,SuccessCount),SuccessCount]
}
/*
type TestCases = Int
//type Result = Option[(FailedCase,SuccessCount)]
case class Prop(run: (TestCases, RNG) => Result){
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop{
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{
      case (a,i) => try{
        if(f(a)) Passed else Falsified(a.toString,i)
      } catch{ case e: Exception => Falsified(buildMsg(a,e),i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A ,e: Exception): String = s"test case $s\n"+
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
*/

sealed trait Result{
  def isFalsified: Boolean
}
case object Passed extends Result{
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result{
  def isFalsified = true;
}


case class Gen[A]( sample : State[RNG,A]) {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {

    Gen(State(RNG.map(RNG.nonNegativeInt)(n => start + n % (stopExclusive - start))))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State(rng => (a, rng)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State(RNG.sequence(List.fill(n)(g.sample.run))))

  def flatMap[B](f:A => Gen[B]):Gen[B] = Gen(State(RNG.flatMap(this.sample.run)(a => f(a).sample.run)))
  def new_listofN(size: Gen[Int]): Gen[List[A]]
  = Gen(State(RNG.flatMap(size.sample.run)(n => listOfN(n,this).sample.run)))

  def Union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    this.boolean.flatMap(b => if(b) g1 else g2)
  }

}