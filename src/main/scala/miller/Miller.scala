package miller

import scala.io.Source.{stdin, fromFile}

object Miller {
  def main(args: Array[String]): Unit = {
    val code = args.headOption
                    .map(fromFile(_).getLines())
                    .getOrElse(stdin.getLines())
                    .toSeq

    val parsed = Parsing.parse(code.mkString("\n"))
    val checked = Inference.check(code.mkString("\n"))

    println(parsed.annotateSource(code))
    println(checked)
  }
}
