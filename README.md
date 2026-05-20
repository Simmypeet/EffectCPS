## Mini Expr

This project lowers a small effectful Expr language into JavaScript.

### CLI

Run a source file and print its result:

```bash
sbt "run -- run program.expr"
```

Print the generated JavaScript to stdout:

```bash
sbt "run -- emit-js program.expr"
```

Write the generated JavaScript to a file:

```bash
sbt "run -- emit-js program.expr output.js"
```

The old built-in example is still available:

```bash
sbt "run -- example-js"
```

### Syntax

Expressions:

```txt
let name = <expr> in <expr>
let _ = <expr> in <expr>
return <value>
do Label(<value>)
if <value> then <expr> else <expr>
handle <expr> with {
  return x -> <expr>;
  case Label(arg, resume) -> <expr>
}
<value>(<value>)
```

Values:

```txt
123
"text"
name
fn x => <expr>
[v1, v2, v3]
lhs + rhs
lhs ++ rhs
lhs == rhs
value[index]
```

Comments use `//`.

### Example

```txt
handle let firstChoice = do Choose([]) in
  if firstChoice then
    let secondChoice = do Choose([]) in
      if secondChoice then
        return "Heads"
      else
        return "Tails"
  else
    let _ = do Fail([]) in
      return "Dropped"
with {
  return value -> return [value];
  case Choose(_ignored, resume) ->
    let left = resume(1) in
      let right = resume(0) in
        return left ++ right;
  case Fail(_ignored, _resume) ->
    return []
}
```
