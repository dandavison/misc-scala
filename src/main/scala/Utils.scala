object Utils {
  import java.io.BufferedWriter
  import java.io.File
  import java.io.FileWriter

  def doc(term: String): Unit = {
    val url = s"https://www.scala-lang.org/api/2.12.8/scala/collection/index.html?search=${term}"
    s"open -a '/Applications/Google Chrome.app' '${url}'".!
  }

  def median(xs: Seq[Double]): Double = {
    val n = xs.length
    assert(n > 0)
    val xss = xs.sorted
    if (n % 2 == 1) {
      xss((n - 1) / 2)
    } else {
      (xss(n / 2) + xss(n / 2 - 1)) / 2
    }
  }

  def fetch(url: String): String = Source.fromURL(url).mkString

  def writeFile(path: String, text: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(path)))
    writer.write(text)
    writer.close()
  }

  def readCsv(path: String, sep: String = ","): Seq[Map[String, String]] = {
    val lines = Source.fromFile(path).getLines.toSeq
    val (colNames, rowLines) = (lines.head.split(sep), lines.tail)
    rowLines.map { line => colNames.zip(line.split(sep)).toMap }
  }

  def writeCsv(
    path: String,
    colNames: Seq[String],
    rows: Seq[Map[String, String]],
    sep: String = ",",
  ): Unit = {
    def mkLine(row: Map[String, String]): String = colNames.map(row(_).toString()).mkString(",")
    val head = colNames.mkString(",")
    val body = rows.map(mkLine).mkString("\n")
    writeFile(path, s"${head}\n${body}")
  }
}
