module Variation0

type Term = 
    | Con of int
    | Div of Term * Term

module Term =
    let rec eval0 = function
        | Con x -> x
        | Div (x, y) -> (eval0 x) / (eval0 y)

type Monad0<'a> = 'a

module Monad0 =
    let lift a = a
    let bind binder m0a =
        binder m0a
    let inline (>>=) m0a binder =
        binder m0a

    let rec monadicEval0 term =
        match term with
        | Con a -> lift a
        | Div (t,u) -> 
            (monadicEval0 t) >>= fun a ->
            (monadicEval0 u) >>= fun b ->
            lift (a / b)

let answer = (Div (Div (Con 1972, Con 2),Con 23)) 
let error = Div (Con 1, Con 0)

let example_0_1 = Term.eval0 answer
//let example_0_2 = Term.eval0 error