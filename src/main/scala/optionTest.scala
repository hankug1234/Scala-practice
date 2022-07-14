import scala.::
import scala.collection.immutable

object optionTest {
  def variance(xs: Seq[Double]): Option[Double] =
  {
    def go(s: Seq[Double],min:Double,len: Int):Option[Double] =
    {
      val b = Some(math.pow(s.head -min,2)/len)
      if(s.length == 1) b
      else go(s.tail,min,len).flatMap(a=> Some(a+(math.pow(s.head -min,2)/len)))
    }
    val len = xs.length
    val min = xs.sum / len
    go(xs,min,len)
  }
  def map2[A,B,C](a: Option[A], b: Option[B])(f:(A,B) => C): Option[C] =
    {
      if((a==None) || (b==None)) None
      else Some(f(a match{case Some(a) => a},b match{case Some(b) => b}))
    }

  def sequence[A](a:List[Option[A]]):Option[List[A]] = a match
  {
    case Nil => Some(Nil)
    case x::y => map2(x.filter((a)=>a==None),sequence(y))((a,b) => a::b)
  }

  def traverse[A,B](list: List[A])(f:A =>Option[B]): Option[List[B]] = list match {
    case Nil => Some(Nil)
    case x::y => map2(f(x).filter((a)=>(a==None)),traverse(y)(f))((a,b) => a::b )
  }

  def main(a:Array[String]):Unit =
  {
    val list = List(Some(1),Some(2),Some(3),Some(None))
    val v = variance(Seq(1.0,2.0,3.0,4.0,5.0))
    val test_sequence = sequence(list)
    v
    test_sequence
  }

}
