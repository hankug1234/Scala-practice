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
    val rlist = foldReverse(list1)
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
    val len = length(list_set)
    val plist = index(list_set,len-1)
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
  def addOne(list: List[Int]):List[Int] = list match
  {
    case Nil => Nil
    case Cons(x,y) => Cons(x+1,addOne(y))
  }
  def doubleToString(list: List[Double]): List[String] = list match {
    case Nil => Nil
    case Cons(x,y) => Cons(x.toString,doubleToString(y))
  }

  def map[A,B](list: List[A])(f: A=>B): List[B] = list match {
    case Nil => Nil
    case Cons(x,y) => Cons(f(x),map(y)(f))
  }

  def filter[A](list:List[A])(f:A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x,y) => if(f(x)) filter(y)(f) else Cons(x,filter(y)(f))
  }

  def flatMap[A,B](list:List[A])(f:A=>List[B]): List[B] = list match {
    case Nil => Nil
    case Cons(x,y) => append(f(x),flatMap(y)(f)) }

  def main(A:Array[String]): Unit =
  {
    def odd(i :Int): Boolean={if(i%2==0)false else true }
    val filter_list = List(0,1,2,3,4,5)
    val double_list = List(1.0,2.0,3.0)
    val list_set = List(List(1,2,3,4),List(5,6,7),List(8,9))
    val list0 = List(1,2,3,4,5)
    val list1 =rtail[Int](list0)
    println(show[Int](list0))
    println(show[Int](list1))
    val list2 = setHead[Int](9,list1)
    println(show[Int](list2))
    val list3 = drop[Int](list0,3)
    println(show[Int](list3))
    val list4 = dropWhile[Int](list0,(i:Int)=> i==4)
    println(show[Int](list4))
    val list5 = init(list0)
    list5
    val b = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    b
    val l = length(list0)
    l
    val rl = foldReverse(list0)
    rl
    val al = append(list0,list1)
    al
    val mlist = listMerge(list_set)
    mlist
    val add_one_list = addOne(list0)
    add_one_list
    val double_list_test = doubleToString(double_list)
    double_list_test
    val filter_list_test = filter(filter_list)(odd)
    filter_list_test
    val flatmap_test = flatMap(list0)(i=>List(i,i))
    flatmap_test
  }
}
