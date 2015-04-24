package fpinscala.parallelism

import java.util.concurrent._
import scala.concurrent.duration._
import org.specs2.specification.Scope

trait ThreadPoolContext extends Scope {
  private val startTime = System.currentTimeMillis
  private val asyncThreadCount = new atomic.AtomicInteger
  def elapsedTime = System.currentTimeMillis millis
  val pool: ExecutorService = Executors.newCachedThreadPool(new ThreadFactory {
    override def newThread(r: Runnable) = {
      asyncThreadCount.incrementAndGet
      Executors.defaultThreadFactory.newThread(r)
    }
  })
  def threadCount = asyncThreadCount.get
}
