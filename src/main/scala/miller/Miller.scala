package miller

import scala.io.Source.{stdin, fromFile}

object Miller {
  def main(args: Array[String]): Unit = {
    implicit val st: ScopeStack = new ScopeStack
    println(SetT(Set(TUndefined)) canSatisfy SetT(Set(TUndefined)))
    return


    if (args.forall(_.startsWith("-"))) {
      // No file specified
      println("Entering CLI input mode, once you've input the source leave a newline then press ctrl-d to start type checking")
    }
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
      checked match {
        case Some(errs) => System.err.println(errs)
        case None => println("No type errors found")
      }
      println("run with -v for full parse tree")
    }

    if (checked.isDefined) {
      System.exit(1)
    }
  }
}
