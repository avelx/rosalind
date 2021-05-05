import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class RosalindSpec extends AnyFunSuite {
  import Rosalind._
  test("counte nuceotides in a string") {
    val in = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    val actual = countNucleotides(in)( Seq('A', 'C', 'G', 'T') )
    actual should be eq  ("A" -> 20, "C" -> 12, "G" -> 17, "T" -> 21)
  }

  test("Transcribing DNA into RNA") {
    val in = "GATGGAACTTGACTACGTAAATT"
    val actual = transcribingToRna(in)
    val expected = "GAUGGAACUUGACUACGUAAAUU"
    actual shouldBe expected
  }

  test("Reverse complement") {
    val in = "AAAACCCGGT"
    val actual = reverseComplement(in)
    val expected = "ACCGGGTTTT"
    actual shouldBe expected
  }

  test("hammingDistance") {
    val a = "GAGCCTACTAACGGGAT"
    val b = "CATCGTAATGACGGCCT"
    hammingDistance(a, b) shouldBe 7
  }

  test("mendelFirstLaw") {
    assert( mendelFirstLaw(2, 2, 2) == 0.7833333333333333 )
  }

//  test("transalte RNA") {
//    val in = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
//    val res = translateRna( in )
//      .filter(_ != "Stop")
//      .mkString("")
//    assert(res == "MAMAPRTEINSTRING")
//  }

  test("string occurances") {
    val a = "GATATATGCATATACTT"
    val b = "ATAT"
    assert( occurancesIndex(a, b) == List(2, 4, 10) )
  }

  test("consensus string") {
    val it = scala.io.Source.fromFile("src/test/resources/data/consensus.txt").getLines().toList
    val result = consensus(it)
    val expected =
      List(
        Map("A" -> 5, "C" -> 0, "G" -> 1, "T" -> 1),
        Map("A" -> 1, "C" -> 0, "G" -> 1, "T" -> 5),
        Map("A" -> 0, "C" -> 1, "G" -> 6, "T" -> 0),
        Map("A" -> 0, "C" -> 4, "G" -> 3, "T" -> 0),
        Map("A" -> 5, "C" -> 2, "G" -> 0, "T" -> 0),
        Map("A" -> 5, "C" -> 0, "G" -> 1, "T" -> 1),
        Map("A" -> 0, "C" -> 6, "G" -> 0, "T" -> 1),
        Map("A" -> 0, "C" -> 1, "G" -> 0, "T" -> 6)
      )
    for {
      i <- 0 to result.length - 1
    } yield assert(result(i) == expected(i))
  }

  test("expectedOffspring") {
    val in = List(1, 0, 0, 1, 0, 1)
    assert(expectedOffspring(in) == 3.5D)
  }

}
