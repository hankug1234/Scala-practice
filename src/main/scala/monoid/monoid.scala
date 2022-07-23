trait Monoid[A]{
  def zero: A // 항등원
  def op( a :A, b: A): A // 결합법칙이 성립하는 이항 연산
}

class monoid_set{
  def optionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]]{def zero: Option[A] = None; def op(a: Option[A], b:Option[A]):Option[A]={a.orElse(b)}}

  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[A => A]{def zero : A=>A = (a:A) => a; def op(a: A=>A , b : A=>A):A=>A = {(c:A) => a(b(c))} }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B):B = {
    as.foldRight(m.zero)((a,b) => m.op(f(a),b))
  }

  def mFoldRight[A,B](list :List[B],z:B)(m:Monoid[B]):B ={
    foldMap(list,m)((b)=>b)
  }

  def foldMapV[A,B](v: IndexedSeq[A], m : Monoid[B])(f: A=>B):B ={
    if(v.length == 0) m.zero
    else if(v.length <= 2){v.foldLeft(m.zero)((a,b) => m.op(f(b),a))}
    else {
      val (l, r) = v.splitAt(Math.round(v.length / 2))
      m.op(l.foldLeft(m.zero)((a,b) => m.op(f(b),a)),r.foldLeft(m.zero)((a,b) => m.op(f(b),a)))
    }
  }

}

object monoid_test{
  def main(a:Array[String]):Unit ={
    /*
    val intAddition: Monoid[Int] = new Monoid[Int]{ def zero: Int = 0 ; def op(a: Int, b:Int): Int = {a+b}}
    val intMultiplication : Monoid[Int] = new Monoid[Int]{def zero: Int = 1; def op(a:Int,b:Int): Int = {a*b}}
    val booleanOr: Monoid[Boolean] = new Monoid[Boolean]{def zero: Boolean = false; def op(a: Boolean, b: Boolean) = {a||b}}
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]{def zero: Boolean = true; def op(a: Boolean, b:Boolean) = {a && b}}
     */
    val bool = new monoid_set;
    val re = bool.checkSort(IndexedSeq(6,5,4,3,2,1))
    re
  }
}