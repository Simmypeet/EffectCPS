package expr

import lowerExpr.LowerExpr

import scala.Predef.{String as ScalaString}

enum Value {
  case Num(n: Int)
  case String(s: ScalaString)
  case Var(name: ScalaString)
  case Lambda(params: ScalaString, body: Expr)
  case Array(elements: List[Value])
  case Add(v1: Value, v2: Value)
  case Index(array: Value, index: Value)
  case Equality(v1: Value, v2: Value)

  def cps(): LowerExpr = {
    this match {
      case Value.Num(n)              => LowerExpr.Num(n)
      case Value.Var(name)           => LowerExpr.Var(name)
      case Value.String(s)           => LowerExpr.String(s)
      case Value.Lambda(p, b)        => LowerExpr.Lambda(p, b.cps())
      case Value.Array(elements)     => LowerExpr.Array(elements.map(_.cps()))
      case Value.Add(v1, v2)         => LowerExpr.Add(v1.cps(), v2.cps())
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

  def cps(): LowerExpr = {
    this match {
      case Expr.App(func, arg)        => LowerExpr.App(func.cps(), arg.cps())
      case Expr.Let(name, expr, body) => {
        val loweredExpr = expr.cps()
        val loweredBody = body.cps()
        val cont = LowerExpr.Lambda(
          name.getOrElse("_"),
          LowerExpr.App(loweredBody, LowerExpr.Var("__k"))
        )

        LowerExpr.Lambda("__k", LowerExpr.App(loweredExpr, cont))
      }
      case Expr.Return(value) =>
        LowerExpr.Lambda(
          "__k",
          LowerExpr.App(LowerExpr.Var("__k"), value.cps())
        )
      case Expr.IfElse(cond, thenBranch, elseBranch) => {
        val loweredCond = cond.cps()
        val loweredThen = thenBranch.cps()
        val loweredElse = elseBranch.cps()

        LowerExpr.IfElse(loweredCond, loweredThen, loweredElse)
      }
      case Expr.Handle(expr, handler) => {
        val loweredExpr = expr.cps()
        val loweredReturn = handler.returnClause.cps()
        val loweredOperation = handler.operationClause.cps()

        LowerExpr.App(
          LowerExpr.App(
            loweredExpr,
            loweredReturn
          ),
          loweredOperation
        )
      }
      case Expr.Do(label, arg) => {
        val loweredArg = arg.cps()
        val cont = LowerExpr.Lambda(
          "x",
          LowerExpr.App(
            LowerExpr.App(
              LowerExpr.Var("__k"),
              LowerExpr.Var("x")
            ),
            LowerExpr.Var("__h")
          )
        );
        val argTriplet = LowerExpr.Array(
          LowerExpr.String(label) :: loweredArg :: cont :: Nil
        );

        LowerExpr.Lambda(
          "__k",
          LowerExpr.Lambda(
            "__h",
            LowerExpr.App(LowerExpr.Var("__h"), argTriplet)
          )
        )
      }
    }
  }
}

/// <returnClause> ::= 'return' <ident> '->' <expr>
case class ReturnClause(param: ScalaString, body: Expr) {
  def cps(): LowerExpr =
    LowerExpr.Lambda(this.param, LowerExpr.Lambda("_", this.body.cps()))
}

/// <operationClause> ::= 'case' <ident> '(' <ident>, <ident> ')' '->' <expr>
case class OperationClause(
    label: ScalaString,
    param: ScalaString,
    resumption: ScalaString,
    body: Expr
) {
  def cps(): LowerExpr = {
    val bodyCps = body.cps()
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

    val forward = {
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
        );
      val newTriplet = LowerExpr.Array(
        labelArgLower :: paramArgLower :: newResumption :: Nil
      )

      LowerExpr.Lambda(
        "__k",
        LowerExpr.Lambda(
          "__h",
          LowerExpr.App(LowerExpr.Var("__h"), newTriplet)
        )
      )
    };

    LowerExpr.Lambda(
      "__triplet",
      LowerExpr.IfElse(
        compareLabels,
        operationHandlerLowered,
        forward
      )
    )
  }
}

/// <handler> ::= '{' <returnClause> <operationClause> '}'
case class Handler(
    returnClause: ReturnClause,
    operationClause: OperationClause
)
