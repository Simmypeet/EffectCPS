@main
def emitDrunkTossJs(): Unit =
  val outputPath = Examples.writeDrunkTossJavaScript()
  println(s"Wrote $outputPath")
