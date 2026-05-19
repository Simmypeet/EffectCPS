import lowerExpr.LowerExpr

class MySuite extends munit.FunSuite {
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
}
