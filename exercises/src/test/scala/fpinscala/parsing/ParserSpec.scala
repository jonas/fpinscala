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
  import P._
  implicit val stringImplicit = P.string _
  implicit val parserOps = P.operators _

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
      val numA: Parser[Int] = char('a').many.map(_.size)
      run(numA)("aaa") mustEqual Right(3)
      run(numA)("b") mustEqual Right(0)

      manyMapSize(P.string("a")) must passWith(Gen.string('a', 'a'))
      manyMapSize2(P.string("a")) must passWith(Gen.string('a', 'a'))
    }

    "slice law" in {
      run(slice((char('a') | char('b')).many))("aaba") mustEqual Right("aaba")
      sliceLaw(P.string("a"), P.string("b")) must passWith(Gen.string('a', 'b'))
    }

    "support context-sensitive parsing" in {
      val gen = Gen.string().map(s => s"${s.length}$s")

      lengthEncodedStringParser must passWith(gen)
    }

    "char" in {
      val c = 'z'
      run(char(c))(c.toString) == Right(c)
    }

    "succeed" in {
      run(succeed(7357))("scala") == Right(7357)
    }

    "string" in {
      val s = "habla"
      run(string(s))(s) == Right(s)
    }

    "or" in {
      run(or(string("abra"),string("cadabra")))("abra") mustEqual Right("abra")
      run(or(string("abra"),string("cadabra")))("cadabra") mustEqual Right("cadabra")
    }

    "listOfN" in {
      run(listOfN(3, "ab" | "cad").slice)("ababcad") mustEqual Right("ababcad")
      run(listOfN(3, "ab" | "cad").slice)("cadabab") mustEqual Right("cadabab")
      run(listOfN(3, "ab" | "cad").slice)("ababab") mustEqual Right("ababab")
    }
  }

  "JSON parser" should {
    import fpinscala.parsing.JSON
    import fpinscala.parsing.JSON._

    def parse(json: String) = run(JSON.parser(P))(json)

    "parse empty object" in {
      parse("{}") === Right(JObject(Map()))
      parse(" {} ") === Right(JObject(Map()))
      parse(" { 	} 	") === Right(JObject(Map()))
      parse("{	 }") === Right(JObject(Map()))
    }

    "parse simple object" in {
      parse("""{"null":null}""") === Right(JObject(Map("null" -> JNull)))
      parse("""{ "null" : null }""") === Right(JObject(Map("null" -> JNull)))
      parse("""{ "int" : 42 }""") === Right(JObject(Map("int" -> JNumber(42))))
      parse("""{ "pi" : 3.14 }""") === Right(JObject(Map("pi" -> JNumber(3.14))))
      parse("""{ "float" : 1.23e-3 }""") === Right(JObject(Map("float" -> JNumber(0.00123))))
      parse("""{ "name" : "Alice" }""") === Right(JObject(Map("name" -> JString("Alice"))))
      parse("""{ "name" : "Mc\"Allen" }""") === Right(JObject(Map("name" -> JString("Mc\\\"Allen"))))
      parse("""{ "truthy" : true }""") === Right(JObject(Map("truthy" -> JBool(true))))
      parse("""{ "no" : false }""") === Right(JObject(Map("no" -> JBool(false))))
      parse("""{ "a": 1 , "b": "B", "c": true }""") ===
      	Right(JObject(Map("a" -> JNumber(1), "b" -> JString("B"), "c" -> JBool(true))))
    }

    "parse empty array" in {
      parse("[]") === Right(JArray(IndexedSeq()))
      parse(" [ ] ") === Right(JArray(IndexedSeq()))
      parse("[	] ") === Right(JArray(IndexedSeq()))
      parse("[ 	]") === Right(JArray(IndexedSeq()))
    }

    "parse simple array" in {
      parse("""[null]""")              === Right(JArray(Vector(JNull)))
      parse("""[ null ]""")            === Right(JArray(Vector(JNull)))
      parse("""[ 42 ]""")              === Right(JArray(Vector(JNumber(42))))
      parse("""[ 3.14 ]""")            === Right(JArray(Vector(JNumber(3.14))))
      parse("""[ 1.23e-3 ]""")         === Right(JArray(Vector(JNumber(0.00123))))
      parse("""[ "Alice" ]""")         === Right(JArray(Vector(JString("Alice"))))
      parse("""[ "Mc\"Allen" ]""")     === Right(JArray(Vector(JString("Mc\\\"Allen"))))
      parse("""[ true ]""")            === Right(JArray(Vector(JBool(true))))
      parse("""[ false ]""")           === Right(JArray(Vector(JBool(false))))
      parse("""[ "a", true, 3.14 ]""") === Right(JArray(Vector(JString("a"), JBool(true), JNumber(3.14))))
    }

    "parse valid JSON" in {
      val json = """
        {
          "Company name" : "Microsoft Corporation",
          "Ticker"  : "MSFT",
          "Active"  : true,
          "Price"   : 30.66,
          "Shares outstanding" : 8.38e9,
          "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
        }
      """
  
      parse(json) === Right(JObject(Map(
	"Company name"       -> JString("Microsoft Corporation"),
	"Ticker"             -> JString("MSFT"),
	"Active"             -> JBool(true),
	"Price"              -> JNumber(30.66),
        "Shares outstanding" -> JNumber(8.38E9),
	"Related companies"  -> JArray(Vector(
	  JString("HPQ"), JString("IBM"), JString("YHOO"), JString("DELL"), JString("GOOG"))
	)
      )))
    }

    "fail to parse malformed object property separator" in {
      val json = """
        {
          "Company name" ; "Microsoft Corporation"
        }
      """
 
      parse(json) === Left(ParseError(List((Location(json, 36), "':'")), Nil))
    }

    "fail to parse malformed array" in {
      val json = """
        [
          [ "HPQ", "IBM",
            "YHOO", "DELL" ++
            "GOOG"
          ]
        ]
      """

      parse(json) === Left(ParseError(List((Location(json, 64), "']'")), Nil))
    }
  }
}
