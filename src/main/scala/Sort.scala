import java.security.KeyStore.TrustedCertificateEntry

object Sort {

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean={
 def loop(n: Int): Boolean ={
   if(n == as.length-1) true
   else if(ordered(as(n),as(n+1)) == false) false
   else loop(n+1)
 }
  loop(0)
}

 def main(A:Array[String]): Unit={
   println(Sort.isSorted[Int](Array(5,4,3,2,1),(a:Int, b:Int)=> a>=b))
 }
}


