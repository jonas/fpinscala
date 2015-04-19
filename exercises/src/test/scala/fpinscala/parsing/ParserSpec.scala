package fpinscala.parsing

import java.util.concurrent._

import org.specs2.mutable.Specification
import org.specs2.matcher.{Matcher,Matchers,TerminationMatchers}
import org.specs2.matcher.Expectable
import org.specs2.matcher.MatchResult

import fpinscala.parallelism._
import fpinscala.state._
import fpinscala.testing._

class ParserSpec extends Specification with Matchers { //with ScalaCheck {

  val P = fpinscala.parsing.Reference
  import fpinscala.parsing.ReferenceTypes.Parser

  val simpleStringGen = Gen.string('a', 'd')

  "Executable laws:" >> {
    val Laws = fpinscala.parsing.Reference.Laws
    type ParserLaw = Gen[String] => Prop
 
    case class ParserLawMatcher(gens: Seq[Gen[String]]) extends Matcher[ParserLaw] {
      val testCases = 100
      val maxSize = 100
      val rng = RNG.Simple(System.currentTimeMillis)
 
      def apply[S <: ParserLaw](e: Expectable[S]) = {
        val errorMessage = gens.map(gen => e.value(gen).run(maxSize, testCases, rng))
                               .collect { case Falsified(msg, n) =>
                                      s"Falsified after $n passed tests:\n ``$msg''" }
                               .mkString("\n")
 
        result(errorMessage.isEmpty,
               s"Passed $testCases tests for ${gens.size} generators",
               errorMessage, e)
      }
    }
 
    implicit class AnyBePassable(result: MatchResult[ParserLaw]) extends AnyRef {
      def pass = passWith(simpleStringGen)
      def passWith(g: Gen[String]*) = ParserLawMatcher(g)
    }
 
    def pass = passWith(simpleStringGen)
    def passWith(g: Gen[String]*) = ParserLawMatcher(g)

    val p1 = P.string("a")
    val p2 = P.string("b")
    val p3 = P.string("c")

    import Laws._

    "map(p)(a => a) === p" in {
      mapIdentity(p1) must pass
    }

    "succeed(a) === a" in {
      succeedLaw("a") must pass
    }

    "((a | b) | c) === (a | (b | c))" in {
      orIsAssociative(p1, p2, p3) must passWith(simpleStringGen, Gen.string())
    }

    "(a | b) =!= (b | a)" in {
      orIsCommutative(p1, p2) must passWith(simpleStringGen, Gen.string())
    }

    "((a ~ b) ~ c) === (a ~ (b ~ c))" in {
      productIsAssociative(p1, p2, p3) must passWith(simpleStringGen, Gen.string())
    }

    "(a ~ (b | c)) === ((a ~ b) | (a ~ c))" in {
      productDistributesOverOr(p1, p2, p3) must pass
    }

    "(a +) ==> (a *)" in {
      many1MeansManyMatches(p1) must pass
    }

    "(a ?) ==> (a *)" in {
      optionalMeansManyMatches(p1) must pass
    }

    "many map size" in {
      manyMapSize(P.string("a")) must passWith(Gen.string('a', 'a'))
      manyMapSize2(P.string("a")) must passWith(Gen.string('a', 'a'))
    }

    "slice law" in {
      sliceLaw(P.string("a"), P.string("b")) must passWith(Gen.string('a', 'b'))
    }

    "support context-sensitive parsing" in {
      val gen = Gen.string().map(s => s"${s.length}$s")

      lengthEncodedStringParser must passWith(gen)
    }
  }
}
