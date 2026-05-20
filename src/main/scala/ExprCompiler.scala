package expr

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.Predef.{String as ScalaString}
import scala.sys.process._

object ExprCompiler {
  def parseFile(path: Path): Expr =
    ExprParser.parse(Files.readString(path, StandardCharsets.UTF_8))

  def emitJavaScript(expr: Expr)(using
      strategy: LoweringStrategy = CuryCpsLoweringStrategy
  ): ScalaString = {
    val loweredJs = strategy.lowerTopLevel(expr).toJavaScript
    s"process.stdout.write(JSON.stringify($loweredJs));"
  }

  def emitJavaScriptFromSource(source: ScalaString)(using
      strategy: LoweringStrategy = CuryCpsLoweringStrategy
  ): ScalaString =
    emitJavaScript(ExprParser.parse(source))

  def emitJavaScriptFile(path: Path)(using
      strategy: LoweringStrategy = CuryCpsLoweringStrategy
  ): ScalaString =
    emitJavaScript(parseFile(path))

  def writeJavaScript(inputPath: Path, outputPath: Path)(using
      strategy: LoweringStrategy = CuryCpsLoweringStrategy
  ): Path = {
    Files.writeString(
      outputPath,
      emitJavaScriptFile(inputPath),
      StandardCharsets.UTF_8
    )
    outputPath
  }

  def run(expr: Expr)(using
      strategy: LoweringStrategy = CuryCpsLoweringStrategy
  ): ScalaString =
    Process(Seq("node", "-e", emitJavaScript(expr))).!!.trim

  def runFile(path: Path)(using
      strategy: LoweringStrategy = CuryCpsLoweringStrategy
  ): ScalaString =
    run(parseFile(path))
}
