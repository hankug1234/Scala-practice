object Compose {
  def start[A,B,C](f:A=>B,g:B=>C): A=>C =
    {
      (a:A) => g(f(a))
    }
  def func1(b:Boolean):Int =
  {
    if(b)1
    else 0
  }
  def func2(i:Int):String =
  {
    if(i == 1)"i am one"
    else "i am not one"
  }
  def main(A:Array[String]):Unit =
  {
    var t0 = Compose.start[Boolean,Int,String](func1,func2)
    var t1 = t0(false)
    println(t1)
  }

}
