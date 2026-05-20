package lowerExpr

import scala.Predef.{String as ScalaString}
import expr.BinaryOp

enum LowerExpr {
  case Num(n: Int)
  case String(s: ScalaString)
  case Lambda(params: ScalaString, body: LowerExpr)
  case Var(name: ScalaString)
  case App(func: LowerExpr, args: LowerExpr)
  case Let(name: Option[ScalaString], value: LowerExpr, body: LowerExpr)
  case Array(elements: List[LowerExpr])
  case IfElse(cond: LowerExpr, thenBranch: LowerExpr, elseBranch: LowerExpr)
  case Binary(left: LowerExpr, op: BinaryOp, right: LowerExpr)
  case Rec(name: ScalaString, param: ScalaString, body: LowerExpr)
  case Absurd

  def toJavaScript: ScalaString =
    LowerExpr.render(this, LowerExpr.Precedence.Lowest)
}

object LowerExpr {
  private object Precedence {
    val Lowest = 0
    val Lambda = 1
    val Conditional = 2
    val Comparison = 3
    val Equality = 4
    val Additive = 5
    val Multiplicative = 6
    val Postfix = 7
    val Atomic = 8
  }

  private case class BinaryRendering(
      code: ScalaString,
      precedence: Int
  )

  private def renderBinary(
      left: LowerExpr,
      op: BinaryOp,
      right: LowerExpr
  ): BinaryRendering =
    def infix(symbol: ScalaString, precedence: Int): BinaryRendering =
      BinaryRendering(
        s"${render(left, precedence)} $symbol ${render(right, precedence + 1)}",
        precedence
      )

    op match
      case BinaryOp.Add                => infix("+", Precedence.Additive)
      case BinaryOp.Subtract           => infix("-", Precedence.Additive)
      case BinaryOp.Multiply           => infix("*", Precedence.Multiplicative)
      case BinaryOp.Divide             => infix("/", Precedence.Multiplicative)
      case BinaryOp.Modulo             => infix("%", Precedence.Multiplicative)
      case BinaryOp.Equal              => infix("===", Precedence.Equality)
      case BinaryOp.NotEqual           => infix("!==", Precedence.Equality)
      case BinaryOp.GreaterThan        => infix(">", Precedence.Comparison)
      case BinaryOp.LessThan           => infix("<", Precedence.Comparison)
      case BinaryOp.GreaterThanOrEqual => infix(">=", Precedence.Comparison)
      case BinaryOp.LessThanOrEqual    => infix("<=", Precedence.Comparison)
      case BinaryOp.Concat             =>
        BinaryRendering(
          s"${render(left, Precedence.Postfix)}.concat(${render(right, Precedence.Lowest)})",
          Precedence.Postfix
        )
      case BinaryOp.Index =>
        BinaryRendering(
          s"${render(left, Precedence.Postfix)}[${render(right, Precedence.Lowest)}]",
          Precedence.Postfix
        )

  private def render(expr: LowerExpr, parentPrecedence: Int): ScalaString = {
    val (code, precedence) = expr match {
      case LowerExpr.Num(n) =>
        (n.toString, Precedence.Atomic)

      case LowerExpr.String(s) =>
        (renderStringLiteral(s), Precedence.Atomic)

      case LowerExpr.Lambda(param, body) =>
        (s"($param) => ${render(body, Precedence.Lambda)}", Precedence.Lambda)

      case LowerExpr.Rec(name, param, body) =>
        (
          s"function $name($param) { return ${render(body, Precedence.Lowest)}; }",
          Precedence.Atomic
        )

      case LowerExpr.Var(name) =>
        (name, Precedence.Atomic)

      case LowerExpr.App(func, arg) =>
        (
          s"${render(func, Precedence.Postfix)}(${render(arg, Precedence.Lowest)})",
          Precedence.Postfix
        )

      case LowerExpr.Let(name, value, body) =>
        val binding = name match {
          case Some(bindingName) =>
            s"const $bindingName = ${render(value, Precedence.Lowest)};"
          case None => s"${render(value, Precedence.Lowest)};"
        }

        (
          s"(() => { $binding return ${render(body, Precedence.Lowest)}; })()",
          Precedence.Atomic
        )

      case LowerExpr.Array(elements) =>
        val renderedElements =
          elements.map(render(_, Precedence.Lowest)).mkString(", ")
        (s"[$renderedElements]", Precedence.Atomic)

      case LowerExpr.IfElse(cond, thenBranch, elseBranch) =>
        (
          s"${render(cond, Precedence.Conditional)} ? ${render(thenBranch, Precedence.Lowest)} : ${render(elseBranch, Precedence.Lowest)}",
          Precedence.Conditional
        )

      case LowerExpr.Binary(left, op, right) =>
        val rendered = renderBinary(left, op, right)
        (rendered.code, rendered.precedence)

      case LowerExpr.Absurd =>
        ("(() => { throw new Error(\"absurd\"); })()", Precedence.Atomic)
    }

    parenthesizeIfNeeded(code, precedence, parentPrecedence)
  }

  private def parenthesizeIfNeeded(
      code: ScalaString,
      precedence: Int,
      parentPrecedence: Int
  ): ScalaString =
    if precedence < parentPrecedence then s"($code)" else code

  private def renderStringLiteral(value: ScalaString): ScalaString =
    val escaped = new StringBuilder
    var index = 0

    while index < value.length do
      value.charAt(index) match
        case '\\' => escaped.append("\\\\")
        case '"'  => escaped.append("\\\"")
        case '\n' => escaped.append("\\n")
        case '\r' => escaped.append("\\r")
        case '\t' => escaped.append("\\t")
        case '\b' => escaped.append("\\b")
        case '\f' => escaped.append("\\f")
        case c    => escaped.append(c)

      index += 1

    s""""${escaped.toString}""""
}
