import scala.::
object Empty extends Stream[Nothing]
case class Cons[+A](h:()=>A,t:()=>Stream[A]) extends Stream[A]
sealed trait Stream[+A]
{
  def empty[A] : Stream[A] = Empty

  def cons[A](hd: =>A,tl: =>Stream[A]):Stream[A] =
  {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=>head,()=>tail)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h()::(t().toList)
  }

  def take(n:Int): Stream[A] = this match{
    case Empty => empty
    case Cons(h,t) => if(n==0) empty else cons(h(),t().take(n-1))
  }

  def drop(n:Int): Stream[A] = this match{
    case Empty => empty
    case Cons(h,t) => if(n > 0) drop(n-1) else cons(h(),t().drop(n))
  }

  def forAll(p: A=>Boolean): Boolean = this match {
    case Empty => true
    case Cons(h,t) => p(h()) && t().forAll(p)
  }

  def foldRight[B](z: =>B)(f:(A,=>B)=>B): B = this match{
    case Cons(h,t) => f(h(),t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile(f:A=>Boolean):Stream[A] = {

     foldRight[Stream[A]](empty)((x, y) => if(f(x)) cons(x,y) else y)
  }

  def headOption:Option[A] =
    {
      foldRight[Option[A]](None)((a,b) => if(a==None) None else Some(a))
    }
}

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = {
    stream.foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))
  }

  def flatMap[A, B](stream: Stream[A])(f: A => Stream[B]): Stream[B] = {
    stream.foldRight[Stream[B]](empty)((a, b) => append(f(a), b))
  }

  def append[A](stream1: => Stream[A], stream2: => Stream[A]): Stream[A] = {
    stream1.foldRight[Stream[A]](stream2)((a, b) => cons(a, b))
  }

  def filter[A](stream: Stream[A])(f: A => Boolean): Stream[A] = {
    stream.foldRight[Stream[A]](empty)((a, b) => if (f(a)) cons(a, b) else b)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a + b, go(a + b, b))

    cons(0, cons(1, go(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some(a) => cons(a._1, unfold(a._2)(f))
    }
  }

  def unfoldOnes: Stream[Int] = unfold[Int, Stream[Int]](empty)(s => Some((1, s)))

  def unfoldConstant[A](a: A): Stream[A] = unfold[A, Stream[A]](empty)(s => Some((a, s)))

  def unfoldFrom(n: Int): Stream[Int] =
    unfold[Int, Stream[Int]](cons(0, empty))(s => s match {
      case Cons(x, _) => Some((n + x(), cons(x() + 1, empty)))
    })

  def unfoldFibs: Stream[Int] =
    unfold[Int, Stream[Int]](cons(0, cons(1, empty)))(s =>
      s match {
        case Cons(x, y) => Some((x(), cons((y() match {
          case Cons(x, _) => x()
        }), cons(x() + (y() match {
          case Cons(x, _) => x()
        }), empty))))
      })

  def unfoldMap [A, B](stream: Stream[A])(f: A => B): Stream[B] ={
    unfold(stream)(s => s match{case Empty => None case Cons(x,y) => Some((f(x())),y())})
  }

  def unfoldTake[A](stream :Stream[A])(n:Int):Stream[A] = {
    unfold[A,(Int,Stream[A])]((n,stream))(s => if(s._1 <= 0) None else s._2 match{case Empty => None case Cons(x,y) => Some(x(),(s._1-1,y()))})
  }

  def unfoldTakeWhile[A](stream :Stream[A])(f:A => Boolean):Stream[A] =
    {
      def go(s:Stream[A]):Option[(A,Stream[A])] = s match{case Empty => None case Cons(x,y) => if(f(x())) Some((x(),y())) else go(y())}
      unfold[A,Stream[A]](stream)(s => s match{case Empty => None case Cons(x,y) => if(f(x())) Some((x(),y())) else go(y()) } )
    }
  def unfoldZipWith[A](stream1 :Stream[A],stream2 :Stream[A])(add:(A,A)=>A):Stream[A] =
    {
      unfold[A,(Stream[A],Stream[A])]((stream1,stream2))(s =>
        s._1 match{case Empty =>
          None case Cons(x,y) =>
          s._2 match{case Empty =>
            None case Cons(a,b) =>
            Some((add(x(),a()),(y(),b())))}})
    }
}
object run{

  def main(a:Array[String]): Unit =
    {
      val test2 = Stream(1,2,3,4,5).takeWhile(a=> a>3).toList
      val test3 = Stream(5,6,7).headOption
      val test4 = Stream.unfoldMap(Stream(1,2,3,4,5,6))(a => "i am %d".format(a)).toList
      val test5 = Stream.flatMap(Stream(1,2,3,4,5,6))(a => Stream.cons("i am %d".format(a),Stream.empty))
      val test6 = Stream.filter(Stream(1,7,4,2,7,6))(a => a>2)
      test2
      test3
      test4
      test5
      test6
      val j = println("i am not lazy value")
      lazy val i = println("evaluation")
      println("not yet evaluation")
      i
      val one = Stream.unfoldTake(Stream.unfoldOnes)(10).toList
      val two = Stream.unfoldFrom(10).take(10).toList
      val tree = Stream.unfoldFibs.take(10).toList
      val four = Stream.unfoldTakeWhile(Stream(1,3,5,2,4,7))(a=>a>3).toList
      val five = Stream.unfoldZipWith[Int](Stream(1,2),Stream(2,3,4))((a : Int,b : Int) => (a+b)).toList
      one
      two
      tree
      four
      five
    }
}
