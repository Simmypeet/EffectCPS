import expr.BinaryOp
import lowerExpr.LowerExpr

class LowerExprSuite extends ExprSuiteSupport {
  test("emits JavaScript for lambda application") {
    val expr = LowerExpr.App(
      LowerExpr.Lambda(
        "x",
        LowerExpr.Binary(LowerExpr.Var("x"), BinaryOp.Add, LowerExpr.Num(1))
      ),
      LowerExpr.Num(2)
    )

    assertEquals(expr.toJavaScript, "((x) => x + 1)(2)")
  }

  test("emits JavaScript for let bindings and indexing") {
    val expr = LowerExpr.Let(
      Some("xs"),
      LowerExpr.Array(List(LowerExpr.String("a\nb"), LowerExpr.Num(3))),
      LowerExpr.Binary(LowerExpr.Var("xs"), BinaryOp.Index, LowerExpr.Num(0))
    )

    assertEquals(
      expr.toJavaScript,
      """(() => { const xs = ["a\nb", 3]; return xs[0]; })()"""
    )
  }

  test("emits JavaScript for equality and if-else") {
    val expr = LowerExpr.IfElse(
      LowerExpr.Binary(LowerExpr.Var("x"), BinaryOp.Equal, LowerExpr.Num(0)),
      LowerExpr.String("zero"),
      LowerExpr.Binary(LowerExpr.Var("x"), BinaryOp.Add, LowerExpr.Num(1))
    )

    assertEquals(expr.toJavaScript, """x === 0 ? "zero" : x + 1""")
  }

  test("emits JavaScript for binary operator precedence") {
    val expr = LowerExpr.Binary(
      LowerExpr.Var("x"),
      BinaryOp.Subtract,
      LowerExpr.Binary(LowerExpr.Var("y"), BinaryOp.Subtract, LowerExpr.Num(1))
    )

    assertEquals(expr.toJavaScript, "x - (y - 1)")
  }

  test("emits JavaScript for recursive functions") {
    val expr = LowerExpr.Rec(
      "countDown",
      "n",
      LowerExpr.IfElse(
        LowerExpr.Binary(LowerExpr.Var("n"), BinaryOp.LessThanOrEqual, LowerExpr.Num(0)),
        LowerExpr.Num(0),
        LowerExpr.App(
          LowerExpr.Var("countDown"),
          LowerExpr.Binary(LowerExpr.Var("n"), BinaryOp.Subtract, LowerExpr.Num(1))
        )
      )
    )

    assertEquals(
      expr.toJavaScript,
      "function countDown(n) { return n <= 0 ? 0 : countDown(n - 1); }"
    )
  }

  test("emits JavaScript for absurd") {
    assertEquals(
      LowerExpr.Absurd.toJavaScript,
      """(() => { throw new Error("absurd"); })()"""
    )
  }
}
