// true or false is old
// time for yes or no
type t = Yes | No

//logic gates
let no input = 
    match input with
    | Yes -> No
    | No -> Yes
let (|||) left right =      // or
    match (left, right) with
    | (No, No) -> No
    | (Yes, No) | (Yes, Yes) | (No, Yes) -> Yes
let (&&&) left right =      // and
    match (left, right) with
    | (Yes, Yes) -> Yes
    | (No, Yes) | (Yes, No) | (No, No) -> No
let (-|) =                  // nor
    no >> (|||)
let (-&) =                  // nand
    no >> (&&&)
let (-|-) left right =      // xor
    (left -& (left -& right)) -& (right -& (left -& right))
let (|-|) left right =      // xnor
    (-|-) left right |> no

//print
let print input = 
    match input with
    | Yes -> printfn "yes"
    | No -> printfn "no"
type logger() =
    member this.Bind(m,f) = 
        printfn "x is %A" m
        f m
    member this.Return(x) =
        printfn "x is %A" x
        x
let log = new logger()

// testing
let A = Yes
let B = No
let z = log {
    let! x = A ||| B
    let! y = A |-| B
    let! z = x &&& y
    return z
}
