package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import calculator.TweetLength._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

	/** ****************
	 * * TWEET LENGTH **
	 * *****************/

	def tweetLength(text: String): Int =
		text.codePointCount(0, text.length)

	test("tweetRemainingCharsCount with a constant signal") {
		val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
		assert(result() == MaxTweetLength - tweetLength("hello world"))

		val tooLong = "foo" * 200
		val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
		assert(result2() == MaxTweetLength - tweetLength(tooLong))
	}

	test("tweetRemainingCharsCount with a supplementary char") {
		val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
		assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
	}

	test("colorForRemainingCharsCount with a constant signal") {
		val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
		assert(resultGreen1() == "green")
		val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
		assert(resultGreen2() == "green")

		val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
		assert(resultOrange1() == "orange")
		val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
		assert(resultOrange2() == "orange")

		val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
		assert(resultRed1() == "red")
		val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
		assert(resultRed2() == "red")
	}

	test("compute solutions 1") {
		val a = Signal(1d)
		val b = Signal(2d)
		val c = Signal(-15d)
		val delta = Polynomial.computeDelta(a, b, c)

		val roots = Polynomial.computeSolutions(a, b, c, delta)
		assert(roots() == Set(-5d, 3d))
	}

	test("compute solutions 2") {
		val a = Signal(1d)
		val b = Signal(-14d)
		val c = Signal(49d)
		val delta = Polynomial.computeDelta(a, b, c)

		val roots = Polynomial.computeSolutions(a, b, c, delta)
		assert(roots() == Set(7d))
	}

	test("compute solutions 3") {
		val a = Signal(1d)
		val b = Signal(4d)
		val c = Signal(3d)
		val delta = Polynomial.computeDelta(a, b, c)

		val roots = Polynomial.computeSolutions(a, b, c, delta)
		assert(roots() == Set(-3d, -1d))
	}

	test("calculator compute values 1") {

		val expr1: Expr = Literal(1d)
		val expr2: Expr = Literal(2d)
		val references: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(expr1, expr2)))
		val results = Calculator.computeValues(references)

		assert(results.get("a").get() == 3d)

	}

	test("calculator compute values 2") {

		val expr1: Expr = Literal(1d)
		val expr2: Expr = Ref("b")
		val expr3: Expr = Literal(2d)
		val expr4: Expr = Ref("a")
		//e.g., a = b + 1 and b = 2 * a. Such cyclic dependencies are considered as errors
		val references: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(expr1, expr2)), "b" -> Signal(Times(expr4, expr3)))
		val results = Calculator.computeValues(references)

		assert(results.get("a").get().equals(Double.NaN))
	}

	test("calculator compute values 3") {

		val expr1: Expr = Literal(1d)
		val expr4: Expr = Ref("a")
		//e.g., a = b + 1 and b = 2 * a. Such cyclic dependencies are considered as errors
		val references: Map[String, Signal[Expr]] = Map("a" -> Signal(Plus(expr4, expr1)))
		val results = Calculator.computeValues(references)

		assert(results.get("a").get().equals(Double.NaN))
	}

}
