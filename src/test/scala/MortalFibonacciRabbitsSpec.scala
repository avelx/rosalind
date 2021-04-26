import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class MortalFibonacciRabbitsSpec extends AnyFunSuite {
  import MortalFibonacciRabbits._

  test("rabbitCount(6)(2)") {
    val in = State(1, 0, List.empty)
    val res = rabbitIterate(in, 6)(1)(3)
    rabbitCount(res) shouldBe BigInt("4")
  }

  test("rabbitCount ?") {
    val in = State(1, 0, List.empty)
    val res = rabbitIterate(in, 83)(1)(16)
    rabbitCount(res) shouldBe BigInt("97817789207983710")
  }

}
