import expr.{Expr, Handler, OperationClause, ReturnClause, Value}
import lowerExpr.LowerExpr
import scala.sys.process._

class MySuite extends munit.FunSuite {
  private def assertLoweredJs(expr: Expr, expected: String): Unit =
    assertEquals(expr.cps().toJavaScript, expected)

  private def runNode(script: String): String =
    Process(Seq("node", "-e", script)).!!.trim

  test("emits JavaScript for lambda application") {
    val expr = LowerExpr.App(
      LowerExpr.Lambda(
        "x",
        LowerExpr.Add(LowerExpr.Var("x"), LowerExpr.Num(1))
      ),
      LowerExpr.Num(2)
    )

    assertEquals(expr.toJavaScript, "((x) => x + 1)(2)")
  }

  test("emits JavaScript for let bindings and indexing") {
    val expr = LowerExpr.Let(
      Some("xs"),
      LowerExpr.Array(List(LowerExpr.String("a\nb"), LowerExpr.Num(3))),
      LowerExpr.Index(LowerExpr.Var("xs"), LowerExpr.Num(0))
    )

    assertEquals(
      expr.toJavaScript,
      """(() => { const xs = ["a\nb", 3]; return xs[0]; })()"""
    )
  }

  test("emits JavaScript for equality and if-else") {
    val expr = LowerExpr.IfElse(
      LowerExpr.Equality(LowerExpr.Var("x"), LowerExpr.Num(0)),
      LowerExpr.String("zero"),
      LowerExpr.Add(LowerExpr.Var("x"), LowerExpr.Num(1))
    )

    assertEquals(expr.toJavaScript, """x === 0 ? "zero" : x + 1""")
  }

  test("lowers return expressions to continuation-passing JavaScript") {
    assertLoweredJs(
      Expr.Return(Value.Add(Value.Num(1), Value.Num(2))),
      "(__k) => __k(1 + 2)"
    )
  }

  test("lowers do expressions to handler-passing JavaScript") {
    assertLoweredJs(
      Expr.Do("log", Value.String("hello")),
      """(__k) => (__h) => __h(["log", "hello", (x) => __k(x)(__h)])"""
    )
  }

  test("lowers handled effectful expressions all the way to JavaScript") {
    val expr = Expr.Handle(
      Expr.Do("log", Value.String("hello")),
      Handler(
        ReturnClause("result", Expr.Return(Value.Var("result"))),
        List(
          OperationClause(
            "log",
            "message",
            "resume",
            Expr.Return(Value.Var("message"))
          )
        )
      )
    )

    assertLoweredJs(
      expr,
      """((__k) => (__h) => __h(["log", "hello", (x) => __k(x)(__h)]))((result) => (_) => (__k) => __k(result))((__triplet) => "log" === __triplet[0] ? (__k) => ((__k) => __k(__triplet[1]))((message) => ((__k) => ((__k) => __k(__triplet[2]))((resume) => ((__k) => __k(message))(__k)))(__k)) : ((__triplet) => (__k) => (__h) => __h([__triplet[0], __triplet[1], (__x) => __triplet[2](__x)(__k)(__h)]))(__triplet))"""
    )
  }

  test("lowers drunkToss with handlers written in Expr syntax and executes it with node") {
    val loweredJs = Examples.allChoicesDrunkToss.cps().toJavaScript
    val script = Examples.renderDrunkTossJavaScript

    assert(loweredJs.contains("Choose"))
    assert(loweredJs.contains("Fail"))
    assertEquals(runNode(script), """["Heads","Tails"]""")
  }
}
