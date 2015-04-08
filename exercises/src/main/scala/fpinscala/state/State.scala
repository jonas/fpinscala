package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rng2) = rng.nextInt

    if (a == Int.MinValue)
      (math.abs(a + 1), rng2)
    else
      (math.abs(a), rng2)
  }

  def nonNegativeInt1(rng: RNG): (Int, RNG) =
    RNG.map(RNG.int)(x => if (x < 1) math.abs(x + 1) else math.abs(x))(rng)

  def double1(rng: RNG): (Double, RNG) = {
    val (a, rng2) = nonNegativeInt(rng)
    (a.toDouble / Int.MaxValue, rng2)
  }

  def double(rng: RNG): (Double, RNG) =
    RNG.map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (Nil, rng)
    else {
      val (a, rng2) = rng.nextInt
      val (l, rng3) = ints(count - 1)(rng2)
      (a :: l, rng3)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((Nil:List[A], rng))((rand, data) => {
        val (a, rng) = rand(data._2)
	(a :: data._1, rng)
      })
    }

  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldLeft((Nil:List[A], rng))((data, rand) => {
        val (a, rng) = rand(data._2)
	(a :: data._1, rng)
      })
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((Nil:List[A], rng))((rand, data) => {
        val (a, rng) = rand(data._2)
        (a :: data._1, rng)
      })
    }

  def ints1(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })
  }

  def mapUsingFlatmap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2UsingFlatmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(x => State.unit(f(x)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
    fs.foldLeft((Nil:List[A], s))((acc, state) => {
      val (a, s) = state.run(acc._2)
      (a :: acc._1, s)
    })
  })

  private[state] def get[S]: State[S, S] = State(s => (s, s))
  private[state] def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Simulation {
  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    @scala.annotation.tailrec
    def go(inputs: List[Input])(s: Machine): ((Int, Int), Machine) = 
      if (s.candies == 0)
	((s.coins, s.candies), s)
      else
        inputs match {
          case Coin :: t =>
	    if (s.locked)
	      go(t)(s.copy(locked = false, coins = s.coins + 1))
	    else
	      go(t)(s.copy(coins = s.coins + 1))
          case Turn :: t =>
	    if (s.locked)
	      go(t)(s)
	    else
	      go(t)(s.copy(locked = true, candies = s.candies - 1))
          case Nil => 
	    ((s.coins, s.candies), s)
        }

    State(go(inputs))
  }

  def simulateMachineWithModify(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def processInput(input: Input)(s: Machine): Machine = (input, s) match {
      case (_,    Machine(_, 0, _))           => println(s"No candies"); s
      case (Coin, Machine(true, _, coins))    => println(s"Coin in: $coins"); s.copy(locked = false, coins = coins + 1)
      case (Turn, Machine(false, candies, _)) => println(s"Candy out: $candies"); s.copy(locked = true, candies = candies - 1)
      case _                                  => println(s"Do nothing"); s
    }

    for {
      _ <- sequence(inputs.map(input => modify(processInput(input))))
      s <- get
    } yield (s.coins, s.candies)
  }
}

class RNGState {
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def ints(count: Int): Rand[List[Int]] = {
    @scala.annotation.tailrec
    def go(count: Int, l: List[Int])(rng: RNG): (List[Int], RNG) = 
      if (count <= 0)
        (l, rng)
      else {
        val (a, rng2) = rng.nextInt
        go(count - 1, a :: l)(rng2)
      }

    State(go(count, Nil))
  }
}
