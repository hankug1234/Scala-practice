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

object Stream
{
  def empty[A] : Stream[A] = Empty

  def cons[A](hd: =>A,tl: =>Stream[A]):Stream[A] =
  {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=>head,()=>tail)
  }

  def map[A,B](stream:Stream[A])(f:A=>B):B =
    {
      Empty.foldRight(Empty)()
    }

  def apply[A](as:A*):Stream[A] = {
    {println("hi")}
    if(as.isEmpty)  empty
    else cons(as.head,apply(as.tail:_*))
  }
}

object run{
  def main(a:Array[String]): Unit =
    {
      val test2 = Stream(1,2,3,4,5).takeWhile(a=> a>3).toList
      val test3 = Stream(5,6,7).headOption
      test2
      test3
    }
}
