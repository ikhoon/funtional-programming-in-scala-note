import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.collection.immutable.Nil


/**
  * Created by ikhoon on 2016. 9. 18..
  */

object Parallel {
  type Par[A] = ExecutorService => Future[A]

  // ex.3 Let's begin by implementing the functions of the API we've developed so far.
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def run[A](s: ExecutorService)(fa: Par[A]): Future[A] = fa(s)

  // Should be run in the difference thread.
  def fork[A](fa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = fa(es).get()
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def async[A](a: => A): Par[A] = lazyUnit(a)

  def asyncF[A, B](f: A => B): A => Par[B] =
    f andThen lazyUnit

  // ex.1 Par.map2 is a new high-order function for combining the result of two compuation.
  // Give most general signature possible.
  def map2[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    es => Map2Future(fa(es), fb(es))(f)

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)

  // ex.5 (Optional) Implement product and map as primitives, then define map2 in terms of them.
  def map[A, B](fa: Par[A])(f: A => B): Par[B] =
    es => MapFuture(run(es)(fa))(f)

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    es => ProductFuture(run(es)(fa), run(es)(fb))

  def mapTwo[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    map(product(fa, fb))(f.tupled)

  def parMap[A, B](fa: List[A])(f: A => B): Par[List[B]] =
    sequenceBalance(fa.map(asyncF(f)))

  // Write the function, called sequence. No additional primitives are required. Do not call run.
  def sequence[A](fpa: List[Par[A]]): Par[List[A]] =
    fpa.foldRight[Par[List[A]]](unit(Nil)) {
      case (x, acc) => map2(x, acc)(_ :: _)
    }

  def sequenceRight[A](fpa: List[Par[A]]): Par[List[A]] =
    fpa match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  def sequenceBalance[A](fpa: List[Par[A]]): Par[List[A]] = fork {
    fpa match {
      case Nil => unit(Nil)
      case x :: Nil => map(x)(List(_))
      case xs =>
        val (l, r) = xs.splitAt(xs.size / 2)
        map2(sequenceBalance(l), sequenceBalance(r))(_ ++ _)
    }
  }

  def parFilter[A](fa: List[A])(f: A => Boolean): Par[List[A]] =
    fa match {
      case Nil => unit(Nil)
      case h :: t =>
        val filtered = unit(if(f(h)) List(h) else Nil)
        map2(filtered, fork(parFilter(t)(f)))(_ ++ _)
    }

  // Given map(y)(id) == y, it's a free theorem that map(map(y)(g))(f)  == map(y)(f compose g)
  // This is sometimes called map fusion. Prove it!
  


  // primitive
  private case class UnitFuture[A](a: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = get()

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  private case class ProductFuture[A, B](fa: Future[A], fb: Future[B]) extends Future[(A, B)] {
    @volatile var cache: Option[(A, B)] = None

    override def isCancelled: Boolean = fa.isCancelled || fb.isCancelled

    override def get(): (A, B) = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): (A, B) = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      fa.cancel(mayInterruptIfRunning) && fb.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = cache.isDefined

    private def compute(timeoutNs: Long): (A, B) = {
      val start = System.nanoTime()
      val a = fa.get(timeoutNs, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime()
      val lap = stop - start
      val b = fb.get(timeoutNs - lap, TimeUnit.NANOSECONDS)
      cache = Some((a, b))
      (a, b)
    }
  }
  private case class MapFuture[A, B](fa: Future[A])(f: A => B) extends Future[B] {
    @volatile var cache: Option[B] = None

    override def isCancelled: Boolean = fa.isCancelled

    override def get(): B = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): B = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = fa.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = cache.isDefined

    private def compute(timeoutNs: Long): B = {
      cache match {
        case Some(c) => c
        case None =>
          f(fa.get(timeoutNs, TimeUnit.NANOSECONDS))
      }
    }
  }

  private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B])(f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def isCancelled: Boolean = fa.isCancelled || fb.isCancelled

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      fa.cancel(mayInterruptIfRunning) && fb.cancel(mayInterruptIfRunning)

    override def isDone: Boolean = cache.isDefined

    private def compute(timeoutNs: Long): C = {
      val start = System.nanoTime()
      val ar = fa.get(timeoutNs, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime()
      val lap = stop - start
      val br = fb.get(timeoutNs - lap, TimeUnit.NANOSECONDS)
      val cr = f(ar, br)
      cache = Some(cr)
      cr
    }
  }


  // ex.2 Before continuing, try to come up with representation for Par and Strategy
  // that make it possible to implement the function of our API.





}
