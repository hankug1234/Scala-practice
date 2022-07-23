import java.util.concurrent.{CountDownLatch, ExecutorService, Callable}
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[A]{
  private[parallelism] def apply(k: A => Unit): Unit

  def run[A](es: ExecutorService)(p: Par[A]): A ={
    val ref = new AtomicReference[A] // cas 방식을 이용항 원자성을 제공하는 wrapper class
    val latch = new CountDownLatch(1)// count == 0이 될때까지 thread를 대기
    p(es){a => ref.set(a); latch.countDown()}
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] = es => new Future[A]{
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit]{def call = r})

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }
  def map2[A,B,C](a: Par[A], b: Par[B])  (f: (A,B) => C):Par[C] = {
    
  }
}
type Par[+A] = ExecutorService => Future[A]