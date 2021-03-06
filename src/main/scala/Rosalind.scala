import scala.collection.mutable.ListBuffer

object Rosalind {
  def countNucleotides(in: String)
                      (alphabet: Seq[Char]): Map[String, Int] = {
    val counter: Map[String, Int] = alphabet
      .toList
      .map(c => (c.toString, 0))
      .toMap[String, Int]
    in.foldLeft(counter) { (acc, c) =>
      if (acc.contains(c.toString)) {
        acc + (c.toString -> (acc(c.toString) + 1))
      } else {
        acc + (c.toString -> 1)
      }
    }
  }

  def transcribingToRna(dna: String): String =
    dna.foldLeft(List[Char]()) { (acc, c) =>
      if (c == 'T') {
        acc :+ 'U'
      } else {
        acc :+ c
      }
    }.mkString("")

  def reverseComplement(dna: String): String = {
    val mapping: Map[Char, Char] = Map(
      'T' -> 'A',
      'A' -> 'T',
      'C' -> 'G',
      'G' -> 'C'
    )
    dna
      .reverse
      .foldLeft(List[Char]()) { (acc, c) =>
        if (mapping.contains(c)) {
          acc :+ mapping(c)
        } else {
          acc :+ c
        }
      }.mkString("")
  }

  // Problem: http://rosalind.info/problems/hamm/
  def hammingDistance(a: String, b: String): Int = {
    var count = 0
    for {
      i <- 0 to a.length - 1
      if a(i) != b(i)
    } yield count += 1
    count
  }

  // http://rosalind.info/problems/iprb/
  def mendelFirstLaw(k: Int, m: Int, n: Int): Double = {
    val s = k.toDouble + m.toDouble + n.toDouble

    val a: Double = n.toDouble * (n.toDouble - 1) / (s * (s - 1))
    val b: Double = 0.5 * m * n / (s * (s - 1))
    val c: Double = 0.25 * m * (m - 1) / (s * (s - 1))

    1 - a - 2 * b - c
  }

  //
  def translateRna(rna: String): List[String] = {
    val fileName = "/src/main/resources/rnaCodonsToaacids.txt"
    val lines = scala.io.Source.fromFile(fileName).getLines()
    val mapping = lines.toList.map(s => {
      val p = s.trim.split(" ")
      p(0) -> p(1)
    }).toMap

    val res = for {
      triple <- rna.sliding(3, 3)
      x = mapping(triple)
    } yield x
    res.toList
  }

  def occurancesIndex(a: String, b: String): List[Int] = {
    val res = ListBuffer[Int]()
    a.foldLeft((0, List[Char]())) { (acc, c) =>
      if (acc._2.mkString("") == b) {
        res.append(acc._1 + 1)
      }
      if (acc._2.length == b.length) {
        (acc._1 + 1, acc._2.tail :+ c)
      } else {
        (acc._1, acc._2 :+ c)
      }
    }
    res.toList
  }

  def consensus(in: List[String]): List[Map[String, Int]] = {
    val result = ListBuffer[collection.mutable.Map[String, Int]]()

    def compute(ds: String): Unit = {
      for {
        i <- 0 to ds.length - 1
        c = ds(i).toString
      } yield {
        val count = result(i)(c)
        result(i) = result(i) + (c -> (count + 1))
      }
    }

    val data = ListBuffer[String]()
    in.foreach(s => {
      if (!s.contains("Rosalind")) {
        data.append(s)
      }
      if (s.contains("Rosalind") && data.length > 0 && result.length == 0) {
        val dataString = data.mkString("")
        List
          .fill(dataString.length)(
            collection.mutable.Map[String, Int]("A" -> 0, "C" -> 0, "G" -> 0, "T" -> 0)
          )
          .map(e => result.append(e))
      }
      if (s.contains("Rosalind") && data.length > 0) {
        compute(data.mkString(""))
        data.clear()
      }
    })
    compute(data.mkString(""))
    // convert to list and immutable map
    result.map(e => collection.immutable.Map() ++ e).toList
  }

  def consensusString(in: List[Map[String, Int]]): String =
    in.map(x => x.maxBy(_._2)._1).mkString("")

  def consensusMatrix(in: List[Map[String, Int]]): List[String] = {
    List("A", "C", "G", "T")
      .map(alpha => {
        val res = in.map(row => row(alpha)).mkString(" ")
        s"$alpha: $res"
      })
  }

  def graphOverlap(in: Map[String, String])(implicit k: Int): List[(String, String)] = {
    {
      for {
        sk <- in.keys
        tk <- in.keys
        if sk != tk
        if stringMatch(in(sk), in(tk))
      } yield (sk, tk)
    }.toList
  }

  def stringMatch(s: String, t: String)(implicit k: Int) : Boolean =
    s.takeRight(k) == t.take(k)

  def readFile(fileName: String): Map[String, String] = {
    val it = scala.io.Source.fromFile(fileName).getLines()
    var (key, value) = ("", "")
    val res = ListBuffer[(String, String)]()
    while (it.hasNext){
      val s = it.next()
      if (s.contains("Rosalind")){
        if (key.nonEmpty && value.nonEmpty) {
          res.append((key, value))
        }
        value = ""
        key = s.replace(">", "")
      } else {
        value += s
      }
    }
    res.append( (key, value) )
    res.toList.toMap
  }

  // (a[0] * 1 + a[1] * 1 + a[2] * 1 + a[3] *0.75 + a[4] *0.5 + a[5] * 0) * 2
  // http://rosalind.info/problems/iev/
  def expectedOffspring(in: List[Int]): Double = in.map(_.toDouble) match {
    case List(a, b, c, d, e, f) =>
      (a * 1 + b * 1 + c * 1 + d * 0.75 + e * 0.5 + f * 0) * 2
    case _ =>
      throw new Error("Not enough parameters")
  }

  def longestCommonSubString(fileName:String): Option[String] = {
    val in = readFile( fileName )

    def commonSubString(k: Int) : Option[String] = {
      val xs = in.keys.map(key => {
        val s = in(key)
        s.sliding(k, 1).toSet
      }).toList

      val firstSet = xs.head
      val res = xs.tail.foldLeft(firstSet) { (acc, curr) => acc intersect curr }

      res.headOption
    }

    var res : Option[String] = None
    (2 to in.head._2.length - 1)
      .takeWhile(k => {
        commonSubString(k) match {
          case Some(xs) =>
            res = Some(xs)
            true
          case None =>
            false
        }
      })
    res
  }

}