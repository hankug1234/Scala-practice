object Curring {
  def curry[A,B,C](f: (A, B) => C): A=>(B=>C) =
    {
      (a:A) => ((b:B)=>f(a,b))
    }
  def test(a:Boolean, b:Int): String={
    if(a == true) "true %d".formatted(b)
    else "false %d".formatted(b*12)
  }
  def main(A:Array[String]): Unit={
    var c0 = Curring.curry[Boolean,Int,String](test)
    var c1 = c0(false)
    var c2 = c1(10)
    println(c2)
  }

}
