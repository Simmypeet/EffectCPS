import expr.LoweringStrategy
import expr.CuryCpsLoweringStrategy
@main
def emitDrunkTossJs(): Unit =
  given LoweringStrategy = CuryCpsLoweringStrategy

  val outputPath = Examples.writeDrunkTossJavaScript()
  println(s"Wrote $outputPath")
