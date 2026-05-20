import expr.{Expr, Value}

class ExprLoweringSuite extends ExprSuiteSupport {
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

  test(
    "lowers drunkToss with handlers written in Expr syntax and executes it with node"
  ) {
    val loweredJs = Examples.allChoicesDrunkToss.cps().toJavaScript
    val script = Examples.renderDrunkTossJavaScript()

    assert(loweredJs.contains("Choose"))
    assert(loweredJs.contains("Fail"))
    assertEquals(runNode(script), """["Heads","Tails"]""")
  }
}
