object fibonacci
{
  def go(v1: Int, v2: Int, n: Int): Int =
    { val curr = v1 + v2
      if(n==2) curr
      else go(v2,curr,n-1)
    }

  def start(n: Int): Int =
  {
    if(n == 0) 0
    else if(n == 1) 1
    else go(0, 1,n)
  }
  def main(a:Array[String]): Unit =
  {
    println(fibonacci.start(6))
  }
}

