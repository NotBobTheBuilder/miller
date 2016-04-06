package miller

import scala.io.Source.{stdin, fromFile}

object Miller {
  def main(args: Array[String]): Unit = {
    print("> ")
    val code = args.filterNot(_.startsWith("-"))
                    .headOption
                    .map(fromFile(_).getLines())
                    .getOrElse(stdin.getLines())
                    .toSeq

    val checked = Inference.check(code.mkString("\n"))

    if (args.contains("-p")) {
      println(Parsing.parseAST(code.mkString("\n")))
    } else if (args.contains("-v")) {
      println(Parsing.parse(code.mkString("\n")).annotateSource(code))
    } else {
      println(checked.getOrElse("No type errors found"))
      println("run with -v for full parse tree")
    }

    if (checked.isDefined) {
      System.exit(1)
    }
  }
}
