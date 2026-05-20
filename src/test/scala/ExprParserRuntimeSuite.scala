import expr.{ExprCompiler, ExprParser}

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
}
