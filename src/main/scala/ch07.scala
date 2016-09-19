import java.util.concurrent.ExecutorService

import scala.concurrent.Future

/**
  * Created by ikhoon on 2016. 9. 18..
  */




object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = ???

  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)

  // Should be run in the difference thread.
  def fork[A](pa: => Par[A]): Par[A] = pa

  def async[A](a: => A): Par[A] = fork(unit(a))

  // ex.1 Par.map2 is a new high-order function for combining the result of two compuation.
  // Give most general signature possible.
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???


  // ex.2 Before continuing, try to come up with representation for Par and Strategy
  // that make it possible to implement the function of our API.


  


}
