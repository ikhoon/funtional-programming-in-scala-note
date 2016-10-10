package parallel

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }


// emacs + ensime 모드로 코딩하기.
// 조금 불편하 점이 있긴 하지만 나쁘지 않다.
// auto import
// company-mode를 이용한 어느정도의 자동완성
// 실시간 에러에 대한 highlighting 정도 만족한다.
// 계속 사용해봐야 겠다.
// refactoing에 대한 기능은 intellij를 따라갈순 없지만 작은 코드를 만지기엔 충분한것 같다.
object NonBlocking {

  // apply method는 외부에 노출하지 않는다.
  sealed trait Future[A] {
    private[parallel] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown }

    // 위의 latch.countDown 이 될때 까지 기다린다.
    latch.await
    ref.get

  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit):Unit = cb(a)
    }


  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }



}
