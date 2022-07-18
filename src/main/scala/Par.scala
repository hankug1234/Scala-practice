  import scala.concurrent.duration.TimeUnit
import java.util.concurrent.{ExecutorService,Future,Callable}

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

  def unit[A](a:A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def map2[A,B,C](a: Par[A], b: Par[B])(f:(A,B) => C):Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get,bf.get))
  }
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A]{ def call = a(es).get})

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

