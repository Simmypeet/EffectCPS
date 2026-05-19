package lowerExpr


enum LowerExpr {
    case Num(n: Int)
    case String(s: String)
    case Add(e1: LowerExpr, e2: LowerExpr)
    case Lambda(params: List[String], body: LowerExpr)
    case Var(name: String)
    case App(func: LowerExpr, args: List[LowerExpr])
    case Let(name: Option[String], value: LowerExpr, body: LowerExpr)
    case Array(elements: List[LowerExpr])
    case Index(array: LowerExpr, index: LowerExpr)
}
