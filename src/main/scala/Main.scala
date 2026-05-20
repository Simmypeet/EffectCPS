import expr.{CuryCpsLoweringStrategy, ExprCompiler, LoweringStrategy}

import java.nio.file.Path

@main
def exprCli(args: String*): Unit =
  given LoweringStrategy = CuryCpsLoweringStrategy

  args.toList.dropWhile(_ == "--") match
    case "run" :: inputPath :: Nil =>
      println(ExprCompiler.runFile(Path.of(inputPath)))

    case "emit-js" :: inputPath :: Nil =>
      println(ExprCompiler.emitJavaScriptFile(Path.of(inputPath)))

    case "emit-js" :: inputPath :: outputPath :: Nil =>
      val writtenPath =
        ExprCompiler.writeJavaScript(Path.of(inputPath), Path.of(outputPath))
      println(s"Wrote $writtenPath")

    case "example-js" :: Nil =>
      val outputPath = Examples.writeDrunkTossJavaScript()
      println(s"Wrote $outputPath")

    case _ =>
      val usage =
        """Usage:
          |  sbt "run -- run <source.expr>"
          |  sbt "run -- emit-js <source.expr> [output.js]"
          |  sbt "run -- example-js"
          |""".stripMargin

      Console.err.println(usage)
      sys.exit(1)
