package fpinscala.parallelism

import java.util.concurrent._
import scala.concurrent.duration._
import fpinscala.parallelism._

import org.specs2.mutable.Specification
import org.specs2.matcher.Matchers
import org.specs2.specification.Scope

class ParSpecification extends Specification with Matchers {

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

  class StringParOps(val s: String) {
    def after(delay: Duration) = {
      Thread.sleep(delay toMillis)
      Par.unit(s)
    }
  }
  implicit def string2StringParOps(s: String) = new StringParOps(s)

  def concat(a: String, b: String) = s"$a$b"

  def sleepyToUpper(a: String) = {
    Thread.sleep(500)
    a.toUpperCase
  }

  "Exercise 7.3".p

  "Par.map2" should {
    "wait for both computations when get is called" in new ThreadPoolContext {
      val par = Par.map2(Par.fork("a1" after 500.millis), Par.fork("b1" after 1000.millis))(concat)

      Par.run(pool)(par).get === "a1b1"
      threadCount must be_>(1)
      elapsedTime must be_>=(1000 millis)
    }
  
    "not fail if computation takes longer than requested" in new ThreadPoolContext {
      val par = Par.map2(Par.fork("a2" after 500.millis), Par.fork("b2" after 1000.millis))(concat)

      Par.run(pool)(par).get(700, TimeUnit.MILLISECONDS) === "a2b2"
      threadCount must be_>(1)
      elapsedTime must be_>=(1000 millis)
    }
  }
  
  "Par.map2Timed" should {
    "wait for both computations when get is called" in new ThreadPoolContext {
      val par = Par.map2Timed(Par.fork("a3" after 500.millis), Par.fork("b3" after 1000.millis))(concat)

      Par.run(pool)(par).get === "a3b3"
      threadCount must be_>(1)
      elapsedTime must be_>=(1000 millis)
    }
  
    "fail if computation takes too long" in new ThreadPoolContext {
      val par = Par.map2Timed(Par.fork("a4" after 500.millis), Par.fork("b4" after 1000.millis))(concat)
      def exec = Par.run(pool)(par).get(700, TimeUnit.MILLISECONDS)

      exec must throwA[TimeoutException]
    }
  }

  "Exercise 7.4".p

  "Par.asyncF" should {
    "compute its result asynchronously" in new ThreadPoolContext {
      val asyncF = Par.asyncF(sleepyToUpper)("future")

      Par.run(pool)(asyncF).get === "FUTURE"
      threadCount must be_==(1)
    }
  }

  "Exercise 7.5".p

  "Par.sequence" should {
    "combine a list of Par" in new ThreadPoolContext {
      val range = 1 to 10
      val asyncF = Par.asyncF(sleepyToUpper)

      val parList = range.map(i => asyncF("future" + i)).toList
      val result = Par.run(pool)(Par.sequence(parList)).get
  
      range.zip(result) foreach {
        case (i, r) => r must_== s"FUTURE$i"
      }

      threadCount must be_>=(10)
      elapsedTime must be_>=(500 millis)
    }

    "work as part of parMap" in new ThreadPoolContext {
      val range = 1 to 10 toList

      val par = Par.parMap(range)(i => sleepyToUpper(s"future$i"))
      val result = Par.run(pool)(par).get

      range.zip(result) foreach {
        case (i, r) => r must_== s"FUTURE$i"
      }

      threadCount must be_>=(10)
      elapsedTime must be_>=(500 millis)
    }
  }
  
}
