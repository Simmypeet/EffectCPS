package expr

import lowerExpr.LowerExpr

import scala.Predef.{String as ScalaString}

trait LoweringStrategy {
  def lowerReturn(value: Value): LowerExpr
  def lowerDo(label: ScalaString, arg: Value): LowerExpr
  def lowerHandle(expr: LowerExpr, handler: Handler): LowerExpr
  def lowerLet(
      name: Option[ScalaString],
      expr: LowerExpr,
      body: LowerExpr
  ): LowerExpr
}

object LoweringStrategy {
  given default: LoweringStrategy = CuryCpsLoweringStrategy
}

object CuryCpsLoweringStrategy extends LoweringStrategy {
  override def lowerReturn(value: Value): LowerExpr =
    LowerExpr.Lambda(
      "__k",
      LowerExpr.App(LowerExpr.Var("__k"), value.cps()(using this))
    )

  override def lowerDo(label: ScalaString, arg: Value): LowerExpr = {
    val loweredArg = arg.cps()(using this)
    val cont = LowerExpr.Lambda(
      "x",
      LowerExpr.App(
        LowerExpr.App(
          LowerExpr.Var("__k"),
          LowerExpr.Var("x")
        ),
        LowerExpr.Var("__h")
      )
    )
    val argTriplet = LowerExpr.Array(
      LowerExpr.String(label) :: loweredArg :: cont :: Nil
    )

    LowerExpr.Lambda(
      "__k",
      LowerExpr.Lambda(
        "__h",
        LowerExpr.App(LowerExpr.Var("__h"), argTriplet)
      )
    )
  }

  override def lowerHandle(expr: LowerExpr, handler: Handler): LowerExpr = {
    val loweredReturn = handler.returnClause.cps()(using this)
    val loweredOperation = handler.cpsOperation()(using this)

    LowerExpr.App(
      LowerExpr.App(
        expr,
        loweredReturn
      ),
      loweredOperation
    )
  }

  override def lowerLet(
      name: Option[ScalaString],
      expr: LowerExpr,
      body: LowerExpr
  ): LowerExpr = {
    val cont = LowerExpr.Lambda(
      name.getOrElse("_"),
      LowerExpr.App(body, LowerExpr.Var("__k"))
    )

    LowerExpr.Lambda("__k", LowerExpr.App(expr, cont))
  }

}

enum Value {
  case Num(n: Int)
  case String(s: ScalaString)
  case Var(name: ScalaString)
  case Lambda(params: ScalaString, body: Expr)
  case Array(elements: List[Value])
  case Add(v1: Value, v2: Value)
  case Concat(v1: Value, v2: Value)
  case Index(array: Value, index: Value)
  case Equality(v1: Value, v2: Value)

  def cps()(implicit strategy: LoweringStrategy): LowerExpr = {
    this match {
      case Value.Num(n)              => LowerExpr.Num(n)
      case Value.Var(name)           => LowerExpr.Var(name)
      case Value.String(s)           => LowerExpr.String(s)
      case Value.Lambda(p, b)        => LowerExpr.Lambda(p, b.cps())
      case Value.Array(elements)     => LowerExpr.Array(elements.map(_.cps()))
      case Value.Add(v1, v2)         => LowerExpr.Add(v1.cps(), v2.cps())
      case Value.Concat(v1, v2)      => LowerExpr.Concat(v1.cps(), v2.cps())
      case Value.Equality(v1, v2)    => LowerExpr.Equality(v1.cps(), v2.cps())
      case Value.Index(array, index) =>
        LowerExpr.Index(array.cps(), index.cps())
    }
  }
}

enum Expr {
  case App(func: Value, arg: Value)
  case Let(name: Option[ScalaString], expr: Expr, body: Expr)
  case Return(value: Value)
  case Do(label: ScalaString, arg: Value)
  case Handle(expr: Expr, handler: Handler)
  case IfElse(cond: Value, thenBranch: Expr, elseBranch: Expr)

  def cps()(implicit strategy: LoweringStrategy): LowerExpr = {
    this match {
      case Expr.App(func, arg)        => LowerExpr.App(func.cps(), arg.cps())
      case Expr.Let(name, expr, body) =>
        strategy.lowerLet(name, expr.cps(), body.cps())
      case Expr.Return(value) =>
        strategy.lowerReturn(value)
      case Expr.IfElse(cond, thenBranch, elseBranch) => {
        val loweredCond = cond.cps()
        val loweredThen = thenBranch.cps()
        val loweredElse = elseBranch.cps()

        LowerExpr.IfElse(loweredCond, loweredThen, loweredElse)
      }
      case Expr.Handle(expr, handler) =>
        strategy.lowerHandle(expr.cps(), handler)
      case Expr.Do(label, arg) =>
        strategy.lowerDo(label, arg)
    }
  }
}

/// <returnClause> ::= 'return' <ident> '->' <expr>
case class ReturnClause(param: ScalaString, body: Expr) {
  def cps()(implicit strategy: LoweringStrategy): LowerExpr =
    LowerExpr.Lambda(this.param, LowerExpr.Lambda("_", this.body.cps()))
}

/// <operationClause> ::= 'case' <ident> '(' <ident>, <ident> ')' '->' <expr>
case class OperationClause(
    label: ScalaString,
    param: ScalaString,
    resumption: ScalaString,
    body: Expr
) {
  def cpsWithFallback(
      fallback: LowerExpr
  )(implicit strategy: LoweringStrategy): LowerExpr = {
    val expectedLabel = LowerExpr.String(this.label)
    val labelArgLower =
      LowerExpr.Index(LowerExpr.Var("__triplet"), LowerExpr.Num(0))
    val paramArgLower =
      LowerExpr.Index(LowerExpr.Var("__triplet"), LowerExpr.Num(1))
    val resumptionArgLower =
      LowerExpr.Index(LowerExpr.Var("__triplet"), LowerExpr.Num(2))
    val paramArg =
      Expr.Return(Value.Index(Value.Var("__triplet"), Value.Num(1)))
    val resumptionArg =
      Expr.Return(Value.Index(Value.Var("__triplet"), Value.Num(2)))
    val compareLabels = LowerExpr.Equality(expectedLabel, labelArgLower)

    val operationHandlerLowered = Expr
      .Let(
        Some(param),
        paramArg,
        Expr.Let(
          Some(resumption),
          resumptionArg,
          body
        )
      )
      .cps()

    LowerExpr.Lambda(
      "__triplet",
      LowerExpr.IfElse(
        compareLabels,
        operationHandlerLowered,
        LowerExpr.App(fallback, LowerExpr.Var("__triplet"))
      )
    )
  }
}

/// <handler> ::= '{' <returnClause> <operationClause>* '}'
case class Handler(
    returnClause: ReturnClause,
    operationClauses: List[OperationClause]
) {
  def cpsOperation()(implicit strategy: LoweringStrategy): LowerExpr =
    operationClauses.foldRight(Handler.forwardUnhandledOperation) {
      (operationClause, fallback) =>
        operationClause.cpsWithFallback(fallback)
    }
}

object Handler {
  private val tripletVar = "__triplet"

  private def forwardUnhandledOperation: LowerExpr = {
    val labelArgLower =
      LowerExpr.Index(LowerExpr.Var(tripletVar), LowerExpr.Num(0))
    val paramArgLower =
      LowerExpr.Index(LowerExpr.Var(tripletVar), LowerExpr.Num(1))
    val resumptionArgLower =
      LowerExpr.Index(LowerExpr.Var(tripletVar), LowerExpr.Num(2))
    val newResumption =
      LowerExpr.Lambda(
        "__x",
        LowerExpr.App(
          LowerExpr.App(
            LowerExpr.App(
              resumptionArgLower,
              LowerExpr.Var("__x")
            ),
            LowerExpr.Var("__k")
          ),
          LowerExpr.Var("__h")
        )
      )
    val newTriplet = LowerExpr.Array(
      labelArgLower :: paramArgLower :: newResumption :: Nil
    )

    LowerExpr.Lambda(
      tripletVar,
      LowerExpr.Lambda(
        "__k",
        LowerExpr.Lambda(
          "__h",
          LowerExpr.App(LowerExpr.Var("__h"), newTriplet)
        )
      )
    )
  }
}
