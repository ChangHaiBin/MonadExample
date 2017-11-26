module Variation1

open Variation0

type Monad1<'a> =
    | Raise of string
    | ReturnValue of 'a

module Monad1 =
    let rec eval1_original = function
        | Con a -> ReturnValue a
        | Div (t,u) -> 
            match eval1_original t with
            | Raise e -> Raise e
            | ReturnValue a ->
                match eval1_original u with
                | Raise e -> Raise e
                | ReturnValue b ->
                    if b = 0
                        then Raise "divide by zero"
                        else ReturnValue (a/b)

let answer = (Div (Div (Con 1972, Con 2),Con 23)) 
let error = Div (Con 1, Con 0)

let Example_1_1 = Monad1.eval1_original answer
let Example_1_2 = Monad1.eval1_original error