object Curring {
  def curry[A,B,C](f: (A, B) => C): A=>(B=>C) =
    {
      (a:A) => ((b:B)=>f(a,b))
    }
  def uncurry[A,B,C](f: A=>B=>C):(A,B)=>C =
    {
      (a:A,b:B) => f(a)(b)
    }

  def test(a:Boolean, b:Int): String={
    if(a == true) "true %d".formatted(b)
    else "false %d".formatted(b*12)
  }

  def test2(a:Boolean): Int => String =
    {
      if(a) (b:Int) => "I am true %d".formatted(b)
      else (b:Int) => "I am false %d".formatted(b)
    }

  def main(A:Array[String]): Unit={
    var c0 = Curring.curry[Boolean,Int,String](test)
    var c1 = c0(false)
    var c2 = c1(10)
    println(c2)
    println("-------------")
    var d0 = Curring.uncurry[Boolean, Int, String](test2)
    var d1 = d0(true,123)
    println(d1)

  }

}
