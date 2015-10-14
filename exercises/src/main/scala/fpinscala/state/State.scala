package fpinscala.state

//load into REPL with :load /Users/brenteritou/dev/fpinscala/exercises/src/main/scala/fpinscala/state/State.scala

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
    val (randomNumber, theGenerator) = rng.nextInt
    (math.abs(randomNumber), theGenerator)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nonNeg, gen) = nonNegativeInt(rng)
    (nonNeg.toDouble / Int.MaxValue, gen)
  }

  def elegantDouble(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (theInt, gen) = rng.nextInt
    val (theDouble, gen2) = double(gen)
    ((theInt,theDouble), gen2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (tuple,gen) = intDouble(rng)
    //this is cool
    ((tuple.swap), gen)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, gen) = double(rng)
    val (double2, gen2) = double(gen)
    val (double3, gen3) = double(gen2)
    ((double1,double2,double3),gen3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(cnt: Int, gen: RNG, theList:List[Int]): (List[Int],RNG) = {
      if (cnt == 0)
        (theList, gen)
      else {
        val (anotherInt, nextGen) = gen.nextInt
        loop(cnt + 1, nextGen, anotherInt :: theList)
      }
    }
    loop(count,rng,List[Int]())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    //for crazy compiler error, repalce rng1 with _
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a,b),rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
   val randOfListOfA: Rand[List[A]] = unit(List[A]())
    fs.foldRight(randOfListOfA)((f, acc) => map2(f, acc)(_ :: _))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
