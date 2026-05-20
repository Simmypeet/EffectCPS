import expr.{CuryCpsLoweringStrategy, Expr, LoweringStrategy}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.sys.process._

trait ExprSuiteSupport extends munit.FunSuite {
  given LoweringStrategy = CuryCpsLoweringStrategy

  protected val drunkTossSource =
    """handle let firstChoice = do Choose([]) in
      |  if firstChoice then
      |    let secondChoice = do Choose([]) in
      |      if secondChoice then
      |        return "Heads"
      |      else
      |        return "Tails"
      |  else
      |    let _ = do Fail([]) in
      |      return "Dropped"
      |with {
      |  return value -> return [value];
      |  case Choose(_ignored, resume) ->
      |    let left = resume(1) in
      |      let right = resume(0) in
      |        return left ++ right;
      |  case Fail(_ignored, _resume) ->
      |    return []
      |}
      |""".stripMargin

  protected def assertLoweredJs(expr: Expr, expected: String): Unit =
    assertEquals(expr.cps().toJavaScript, expected)

  protected def runNode(script: String): String =
    Process(Seq("node", "-e", script)).!!.trim

  protected def withTempExprFile[A](
      source: String
  )(body: java.nio.file.Path => A): A = {
    val path = Files.createTempFile("effectcps-", ".expr")
    Files.writeString(path, source, StandardCharsets.UTF_8)

    try body(path)
    finally Files.deleteIfExists(path)
  }
}
