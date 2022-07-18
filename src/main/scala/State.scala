case class State[S,+A](run: S => (A,S)){

  type Rand[A] = State[RNG,A]

  def unit[A](a:A):Rand[A] = State(rng => (a,rng))
3
  def map[A,B](s: Rand[A])(f:A=>B):Rand[B] = State(rng => {flatMap(s)(a=>State(b=>(f(a),b)))match{case State(r) => r(rng)}})

  def map2[A,B,C](ra: Rand[A],rb: Rand[B])(f:(A,B) => C): Rand[C] =
    State(rng =>
    { flatMap[(A,B),C](State(s =>
       (ra match{case State(r) =>
        rb match{case State(r2) =>
          ((r(s)._1, r2(r(s)._2)._1),r2(r(s)._2)._2)}}))
    )(a => State(s2 =>
      (f(a._1,a._2),s2))) match{case State(r) => r(rng)}})

  def transfer[A](fs:List[Rand[A]])(list:List[A]):Rand[List[A]] = State(rng => {
    fs match{case Nil => (list,rng) case x::y => (transfer(y)(x match{case State(r) => r(rng)._1::list}))match{case State(r) => r(rng)}}
  })
  def sequence[A](fs:List[Rand[A]]):Rand[List[A]] = State(rng => transfer(fs)(Nil) match{case State(r) => r(rng)})

  def flatMap[A,B](f: Rand[A])(g: A=> Rand[B]): Rand[B] = State(rng => {
    val(value,rng2) = f match{case State(r) => r(rng)}
    g(value) match{case State(r) => r(rng2) }
  })

  def get[S]:State[S,S] = State(s => (s,s))
  def set[S](s:S):State[S,Unit] = State(_ => ((),s))

  /*
  def modify[S](f:S=>S):State[S,Unit] = for{
    s <- get[S]
    _ <- set[S](f(s))
  }yield()
*/
}
