import expr.{BinaryOp, Expr, ExprCompiler, ExprParser, Value}

class ExprParserRuntimeSuite extends ExprSuiteSupport {
  test("parses Expr source into the existing AST") {
    assertEquals(
      ExprParser.parse(drunkTossSource),
      Examples.allChoicesDrunkToss
    )
  }

  test("parses a source file and runs it through node") {
    withTempExprFile(drunkTossSource) { path =>
      assertEquals(ExprCompiler.runFile(path), """["Heads","Tails"]""")
    }
  }

  test("parses binary operators into the new binary AST form") {
    assertEquals(
      ExprParser.parse("return xs[1 + 1] != ys[0]"),
      Expr.Return(
        Value.Binary(
          Value.Binary(
            Value.Var("xs"),
            BinaryOp.Index,
            Value.Binary(Value.Num(1), BinaryOp.Add, Value.Num(1))
          ),
          BinaryOp.NotEqual,
          Value.Binary(Value.Var("ys"), BinaryOp.Index, Value.Num(0))
        )
      )
    )
  }

  test("parses recursive functions with rec syntax") {
    assertEquals(
      ExprParser.parse("return rec loop x => return x"),
      Expr.Return(
        Value.Rec("loop", "x", Expr.Return(Value.Var("x")))
      )
    )
  }

  test("reports unsupported nested function application with context and caret") {
    val error =
      intercept[ExprParser.ParseError](ExprParser.parse("return f(1)")).getMessage

    assert(error.contains("Parse error at 1:9"))
    assert(
      error.contains(
        "Function application is not allowed directly inside return"
      )
    )
    assert(error.contains("return f(1)"))
    assert(error.contains("        ^"))
  }
}
