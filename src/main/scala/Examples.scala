import expr.{Expr, Handler, OperationClause, ReturnClause, Value}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import expr.LoweringStrategy

object Examples {
  val drunkToss: Expr = Expr.Let(
    Some("firstChoice"),
    Expr.Do("Choose", Value.Array(Nil)),
    Expr.IfElse(
      Value.Var("firstChoice"),
      Expr.Let(
        Some("secondChoice"),
        Expr.Do("Choose", Value.Array(Nil)),
        Expr.IfElse(
          Value.Var("secondChoice"),
          Expr.Return(Value.String("Heads")),
          Expr.Return(Value.String("Tails"))
        )
      ),
      Expr.Let(
        None,
        Expr.Do("Fail", Value.Array(Nil)),
        Expr.Return(Value.String("Dropped"))
      )
    )
  )

  val allChoicesHandler: Handler = Handler(
    ReturnClause("value", Expr.Return(Value.Array(Value.Var("value") :: Nil))),
    List(
      OperationClause(
        "Choose",
        "_ignored",
        "resume",
        Expr.Let(
          Some("left"),
          Expr.App(Value.Var("resume"), Value.Num(1)),
          Expr.Let(
            Some("right"),
            Expr.App(Value.Var("resume"), Value.Num(0)),
            Expr.Return(Value.Concat(Value.Var("left"), Value.Var("right")))
          )
        )
      ),
      OperationClause(
        "Fail",
        "_ignored",
        "_resume",
        Expr.Return(Value.Array(Nil))
      )
    )
  )

  val allChoicesDrunkToss: Expr = Expr.Handle(drunkToss, allChoicesHandler)

  def renderDrunkTossJavaScript()(implicit
      strategy: LoweringStrategy
  ): String = {
    val loweredJs = strategy.lowerTopLevel(allChoicesDrunkToss).toJavaScript

    s"process.stdout.write(JSON.stringify($loweredJs));"
  }

  def writeDrunkTossJavaScript(
      outputPath: Path = Path.of("drunkToss.generated.js")
  )(implicit
      strategy: LoweringStrategy
  ): Path = {
    Files.writeString(
      outputPath,
      renderDrunkTossJavaScript()(using strategy),
      StandardCharsets.UTF_8
    )
    outputPath
  }
}
