package fpinscala.datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List{
  def rtail[A](list: List[A]):List[A]= list match
    {
    case Nil => list
    case Cons(_,t) => t
    }
  def setHead[A](a: A, list: List[A]):List[A] =
    {
     Cons(a,list)
    }
  def drop[A](list: List[A],num:Int):List[A]= {
    if(num == 0) list
    else if(list == Nil) list
    else drop(rtail[A](list),num-1)

  }
  def apply[A](as:A*):List[A]=
  {
    if(as.isEmpty)Nil
    else Cons(as.head,apply(as.tail:_*))
  }
  def show[A](list: List[A]):String = list match
    {
    case Nil => "is Nil"
    case Cons(x,_) => "is %s".formatted(x.toString)
    }
  def main(A:Array[String]): Unit =
  {
    var list0 = List()
    var list1 =rtail[Int](list0)
    println(show[Int](list0))
    println(show[Int](list1))
    var list2 = setHead[Int](9,list1)
    println(show[Int](list2))
    var list3 = drop[Int](list0,3)
    println(show[Int](list3))
  }
}
