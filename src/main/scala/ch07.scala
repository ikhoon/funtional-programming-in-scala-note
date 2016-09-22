import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


/**
  * Created by ikhoon on 2016. 9. 18..
  */




object Par {
  type Par[A] = ExecutorService => Future[A]

  // ex.3 Let's begin by implementing the functions of the API we've developed so far.
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](a: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = get()

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)

  // Should be run in the difference thread.
  def fork[A](pa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = pa(es).get()
    })

  def async[A](a: => A): Par[A] = fork(unit(a))

  // ex.1 Par.map2 is a new high-order function for combining the result of two compuation.
  // Give most general signature possible.
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => Map2Future(pa(es), pb(es))(f)

  case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

    override def isDone: Boolean = cache.isDefined

    private def compute(timeoutNs: Long): C = {
      val start = System.nanoTime()
      val ar = a.get(timeoutNs, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime()
      val diff = stop - start
      val br = b.get(timeoutNs - diff, TimeUnit.NANOSECONDS)
      val cr = f(ar, br)
      cache = Some(cr)
      cr
    }
  }


  // ex.2 Before continuing, try to come up with representation for Par and Strategy
  // that make it possible to implement the function of our API.





}
