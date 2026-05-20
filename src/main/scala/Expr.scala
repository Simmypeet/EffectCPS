package expr

import lowerExpr.LowerExpr

import scala.Predef.{String as ScalaString}

trait LoweringStrategy {
  def lowerReturn(value: Value): LowerExpr
  def lowerDo(label: ScalaString, arg: Value): LowerExpr
  def lowerHandle(expr: Expr, handler: Handler): LowerExpr
  def lowerLet(
      name: Option[ScalaString],
      expr: Expr,
      body: Expr
  ): LowerExpr
  def lowerTopLevel(expr: Expr): LowerExpr
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

  override def lowerHandle(expr: Expr, handler: Handler): LowerExpr = {
    given LoweringStrategy = this

    val loweredReturn = handler.returnClause.cps()(using this)
    val loweredOperation = handler.cpsOperation()(using this)

    LowerExpr.App(
      LowerExpr.App(
        expr.cps(),
        loweredReturn
      ),
      loweredOperation
    )
  }

  override def lowerLet(
      name: Option[ScalaString],
      expr: Expr,
      body: Expr
  ): LowerExpr = {
    given LoweringStrategy = this

    val cont = LowerExpr.Lambda(
      name.getOrElse("_"),
      LowerExpr.App(body.cps(), LowerExpr.Var("__k"))
    )

    LowerExpr.Lambda("__k", LowerExpr.App(expr.cps(), cont))
  }

  override def lowerTopLevel(expr: Expr): LowerExpr = {
    given LoweringStrategy = this
    val identReturn =
      LowerExpr.Lambda("__x", LowerExpr.Lambda("_", LowerExpr.Var("__x")));
    val absurdHandler = LowerExpr.Lambda("__triplet", LowerExpr.Absurd);

    LowerExpr.App(
      LowerExpr.App(
        expr.cps(),
        identReturn
      ),
      absurdHandler
    )
  }
}

enum BinaryOp {
  case Add
  case Subtract
  case Multiply
  case Divide
  case Modulo
  case Equal
  case NotEqual
  case GreaterThan
  case LessThan
  case GreaterThanOrEqual
  case LessThanOrEqual
  case Concat
  case Index
}

enum Value {
  case Num(n: Int)
  case String(s: ScalaString)
  case Var(name: ScalaString)
  case Lambda(params: ScalaString, body: Expr)
  case Rec(name: ScalaString, params: ScalaString, body: Expr)
  case Array(elements: List[Value])
  case Binary(left: Value, op: BinaryOp, right: Value)

  def cps()(implicit strategy: LoweringStrategy): LowerExpr = {
    this match {
      case Value.Num(n)           => LowerExpr.Num(n)
      case Value.Var(name)        => LowerExpr.Var(name)
      case Value.String(s)        => LowerExpr.String(s)
      case Value.Lambda(p, b)     => LowerExpr.Lambda(p, b.cps())
      case Value.Rec(n, p, b)     => LowerExpr.Rec(n, p, b.cps())
      case Value.Array(elements)  => LowerExpr.Array(elements.map(_.cps()))
      case Value.Binary(l, op, r) => LowerExpr.Binary(l.cps(), op, r.cps())
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
        strategy.lowerLet(name, expr, body)
      case Expr.Return(value) =>
        strategy.lowerReturn(value)
      case Expr.IfElse(cond, thenBranch, elseBranch) => {
        val loweredCond = cond.cps()
        val loweredThen = thenBranch.cps()
        val loweredElse = elseBranch.cps()

        LowerExpr.IfElse(loweredCond, loweredThen, loweredElse)
      }
      case Expr.Handle(expr, handler) =>
        strategy.lowerHandle(expr, handler)
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
      LowerExpr.Binary(
        LowerExpr.Var("__triplet"),
        BinaryOp.Index,
        LowerExpr.Num(0)
      )
    val paramArg =
      Expr.Return(
        Value.Binary(Value.Var("__triplet"), BinaryOp.Index, Value.Num(1))
      )
    val resumptionArg =
      Expr.Return(
        Value.Binary(Value.Var("__triplet"), BinaryOp.Index, Value.Num(2))
      )
    val compareLabels =
      LowerExpr.Binary(expectedLabel, BinaryOp.Equal, labelArgLower)

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
      LowerExpr.Binary(
        LowerExpr.Var(tripletVar),
        BinaryOp.Index,
        LowerExpr.Num(0)
      )
    val paramArgLower =
      LowerExpr.Binary(
        LowerExpr.Var(tripletVar),
        BinaryOp.Index,
        LowerExpr.Num(1)
      )
    val resumptionArgLower =
      LowerExpr.Binary(
        LowerExpr.Var(tripletVar),
        BinaryOp.Index,
        LowerExpr.Num(2)
      )
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
