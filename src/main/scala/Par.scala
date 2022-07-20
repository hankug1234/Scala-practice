  import scala.concurrent.duration.TimeUnit
  import java.util.concurrent.{Callable, ExecutorService, Future}
  import scala.{::, List}

object Par {
  /*
  * def unit[A](a:A): Par[A] = 상수값을 병렬 계산으로 승격 한다
  * def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 두 병렬 계산의 결과를 이항 항수로 결합한다
  * def fork[A](a:=> Par[A]): Par[A] = 주어진 인수가 동시적으로 평가될 계산임을 표시 한다 그평가는 run 에 의해 강제되야 실행 된다
  * def lazyUnit[A](a:=>A): Par[A] = fork(unit(a))
  * def run[A](a: Par[A]): A
  * */
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A]{
    def isDone = true
    def get(timeout:Long, units: TimeUnit):A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  }

  def unit[A](a:A): Par[A] = (es: ExecutorService) => UnitFuture(a) // a를 병렬 처리 가능한 값으로 승격 시킨다
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def map2[A,B,C](a: Par[A], b: Par[B])(f:(A,B) => C):Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get,bf.get))
  }

  def map[A,B](a: Par[A])(f:A=>B):Par[B] = {
  map2(a,unit(()))((a,_) => f(a))
  }

  // def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]]

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A]{ def call = a(es).get}) // 분기가 시작 된다는 서명 이자 excutorService 를 받아 a를 적제 시키는 고리 역할

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = { //f를 병렬 처리 할 수 있도록 만들어줌
    a => lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]]):Par[List[A]] = ps match{
    case Nil => unit(Nil)
    case x::y => map2(x,fork(sequence(y)))((a,b) => a::b)
  }

  def parMap[A,B](ps: List[A])(f: A=>B): Par[List[B]] = fork{
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] =
      l.map(asyncF(a => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def equel[A](e: ExecutorService)(p: Par[A], p2: Par[A]) : Boolean = p(e).get == p2(e).get

}
/*
trait Callable[A]{def call: A}
trait Future[A]{
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancle(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}


class ExecutorService{
  def submit[A](a: Callable[A]): Future[A] = {}
}
*/

