sealed trait Tree[+A]
case class Leaf[A](value : A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object Tree
{
  def fold[A,B](tree:Tree[A],b:B)(f:(A,B) => B):B = tree match
    {
    case Leaf(a) => f(a,b)
    case Branch(l,r) => fold(l,fold(r,b)(f))(f)
    }

  def size[A](tree : Tree[A]): Int = {
    def count[A](root: Tree[A], i: Int): Int = root match {
      case Leaf(_) => i+1
      case Branch(l, r) => count(l, i + 1) + count(r, 0)
    }
    count(tree,0)
  }

  def foldSize[A](tree : Tree[A]): Int = {
    def count[A](a:A,b:Int):Int =

      {
        b+1
      }
    fold(tree,0)((a,b) => count(a,b))
  }

  def depth[A](tree :Tree[A]): String = {
    def stringMax(s1:String,s2:String): String =
      {
        if(s1.length >= s2.length)
          s1
        else
          s2
      }
    def deepPath[A](root: Tree[A], path:String):String  = root match {
      case Leaf(_) => path
      case Branch(l, r) => stringMax(deepPath(l,path+"l"),deepPath(r,path+"r"))
    }
    deepPath(tree,"")
  }

  def maximum(tree :Tree[Int]): Int = tree match{
    case Leaf(a) => a
    case Branch(l,r) => maximum(l).max(maximum(r))
}
  def map[A,B](tree:Tree[A])(f:A=>B):Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))

  }

  def main(a :Array[String]): Unit =
    {
      val tree = Branch[Int](Branch(Leaf(15),Branch(Leaf(9),Branch(Leaf(11),Leaf(12)))),Branch(Leaf(3),Leaf(4)))
      val s = size(tree)
      val m = maximum(tree)
      val path = depth(tree)
      val mapAtoB = map(tree)(a => "i am %d".formatted(a))
      s
      m
      path
      mapAtoB
    }
}




