package fpinscala.datastructure

import scala.annotation.tailrec

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
  def dropWhile[A](list: List[A], f:A=>Boolean): List[A] = list match
  {
    case Nil => list
    case Cons(x,y) => if(f(x)) y else dropWhile(y,f)
  }
  def init[A](list: List[A]):List[A] = list match
  {
    case Nil => list
    case Cons(x,y) => if(y == Nil) Nil else Cons(x,init(y))
  }

  def foldRight[A,B](list:List[A],b:B)(f:(A,B)=>B):B = list match
    {
    case Nil => b
    case Cons(x,y) => f(x,foldRight(y,b)(f))
    }

@tailrec  def foldLeft[A,B](list: List[A], b:B)(f:(B,A)=> B):B = list match {
  case Nil => b
  case Cons(x, y) => foldLeft(y, f(b, x))(f)
}
  @tailrec  def fold[A,B](list: List[A], b:B)(f:(A,B)=> B):B = list match
  {
    case Nil => b
    case Cons(x,y) => fold(y,f(x,b))(f)

  }

 def append[A](list1:List[A],list2:List[A]): List[A] =
  {
    var rlist = foldReverse(list1)
   def go(list1: List[A], list2:List[A]): List[A] = list1 match
      {
      case Nil => foldReverse(list1)
      case Cons(x,y) => fold(y,Cons(x,list2))(Cons(_,_))
      }
    go(rlist,list2)
  }

  def listMerge[A](list_set: List[List[A]]): List[A] =
  {
    def index(list: List[List[A]],i: Int): List[A] = list match {
      case Cons(x,y) => if(i==0) x else index(y,i-1)
    }
    def loop(list: List[List[A]],plist: List[A], len: Int): List[A] =
      {
        if(len == 0) plist
        else loop(list,append(index(list,len-1),plist),len-1)
      }
    var len = length(list_set)
    var plist = index(list_set,len-1)
    loop(list_set,plist,len-1)

  }

  def foldReverse[A](list: List[A]): List[A] = list match
    {
    case Nil => list
    case Cons(x,y) => fold(y,Cons(x,Nil))(Cons(_,_))
    }
  def length[A](list: List[A]): Int =
    {
      foldLeft(list,0)((b,a) => b+1)
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
    var list_set = List(List(1,2,3,4),List(5,6,7),List(8,9))
    var list0 = List(1,2,3,4,5)
    var list1 =rtail[Int](list0)
    println(show[Int](list0))
    println(show[Int](list1))
    var list2 = setHead[Int](9,list1)
    println(show[Int](list2))
    var list3 = drop[Int](list0,3)
    println(show[Int](list3))
    var list4 = dropWhile[Int](list0,(i:Int)=> i==4)
    println(show[Int](list4))
    var list5 = init(list0)
    list5
    var b = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    b
    var l = length(list0)
    l
    var rl = foldReverse(list0)
    rl
    var al = append(list0,list1)
    al
    val mlist = listMerge(list_set)
    mlist
  }
}
