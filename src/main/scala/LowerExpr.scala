package lowerExpr

import scala.Predef.{String as ScalaString}

enum LowerExpr {
    case Num(n: Int)
    case String(s: ScalaString)
    case Add(e1: LowerExpr, e2: LowerExpr)
    case Lambda(params: ScalaString, body: LowerExpr)
    case Var(name: ScalaString)
    case App(func: LowerExpr, args: LowerExpr)
    case Let(name: Option[ScalaString], value: LowerExpr, body: LowerExpr)
    case Array(elements: List[LowerExpr])
    case Index(array: LowerExpr, index: LowerExpr)
    case Equality(v1: LowerExpr, v2: LowerExpr)
    case IfElse(cond: LowerExpr, thenBranch: LowerExpr, elseBranch: LowerExpr)

    def toJavaScript: ScalaString =
        LowerExpr.render(this, LowerExpr.Precedence.Lowest)
}

object LowerExpr {
    private object Precedence {
        val Lowest = 0
        val Lambda = 1
        val Conditional = 2
        val Equality = 3
        val Add = 4
        val Postfix = 5
        val Atomic = 6
    }

    private def render(expr: LowerExpr, parentPrecedence: Int): ScalaString = {
        val (code, precedence) = expr match {
            case LowerExpr.Num(n) =>
                (n.toString, Precedence.Atomic)

            case LowerExpr.String(s) =>
                (renderStringLiteral(s), Precedence.Atomic)

            case LowerExpr.Add(e1, e2) =>
                (
                    s"${render(e1, Precedence.Add)} + ${render(e2, Precedence.Add)}",
                    Precedence.Add
                )

            case LowerExpr.Equality(v1, v2) =>
                (
                    s"${render(v1, Precedence.Equality)} === ${render(v2, Precedence.Equality)}",
                    Precedence.Equality
                )

            case LowerExpr.Lambda(param, body) =>
                (s"($param) => ${render(body, Precedence.Lambda)}", Precedence.Lambda)

            case LowerExpr.Var(name) =>
                (name, Precedence.Atomic)

            case LowerExpr.App(func, arg) =>
                (
                    s"${render(func, Precedence.Postfix)}(${render(arg, Precedence.Lowest)})",
                    Precedence.Postfix
                )

            case LowerExpr.Let(name, value, body) =>
                val binding = name match {
                    case Some(bindingName) => s"const $bindingName = ${render(value, Precedence.Lowest)};"
                    case None => s"${render(value, Precedence.Lowest)};"
                }

                (
                    s"(() => { $binding return ${render(body, Precedence.Lowest)}; })()",
                    Precedence.Atomic
                )

            case LowerExpr.Array(elements) =>
                val renderedElements = elements.map(render(_, Precedence.Lowest)).mkString(", ")
                (s"[$renderedElements]", Precedence.Atomic)

            case LowerExpr.Index(array, index) =>
                (
                    s"${render(array, Precedence.Postfix)}[${render(index, Precedence.Lowest)}]",
                    Precedence.Postfix
                )

            case LowerExpr.IfElse(cond, thenBranch, elseBranch) =>
                (
                    s"${render(cond, Precedence.Conditional)} ? ${render(thenBranch, Precedence.Lowest)} : ${render(elseBranch, Precedence.Lowest)}",
                    Precedence.Conditional
                )
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
                case '"' => escaped.append("\\\"")
                case '\n' => escaped.append("\\n")
                case '\r' => escaped.append("\\r")
                case '\t' => escaped.append("\\t")
                case '\b' => escaped.append("\\b")
                case '\f' => escaped.append("\\f")
                case c => escaped.append(c)

            index += 1

        s""""${escaped.toString}""""
}
