module FSharpPlayground.ExpressionTests

open Expecto
open Swensen.Unquote

type Operation = | Plus | Minus | Multiply | Divide | Modulo

type Expression =
    | Number of int
    | BinaryOperationExpression of BinaryOperationExpression
and BinaryOperationExpression = { Left : Expression; Operation : Operation; Right : Expression }

let printOperation = function
    | Plus -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | Modulo -> "%"

let printExpression expression =
    let rec printInternal wrap = function
        | Number number -> $"%d{number}"
        | BinaryOperationExpression operation ->
            let left = printInternal true operation.Left
            let op = printOperation operation.Operation
            let right = printInternal true operation.Right
            if wrap then
                $"(%s{left} %s{op} %s{right})"
            else
                $"%s{left} %s{op} %s{right}"
    printInternal false expression

let evaluateOperation operation =
    match operation with
    | Plus -> (+)
    | Minus -> (-)
    | Multiply -> (*)
    | Divide -> (/)
    | Modulo -> (%)

let rec evaluate expression =
    match expression with
    | Number number -> number
    | BinaryOperationExpression operation ->
        let left = evaluate operation.Left
        let right = evaluate operation.Right
        (evaluateOperation operation.Operation) left right

[<Tests>]
let tests =
    testList "Expressions" [
        testList "Printing expressions" [
            testCase "NumberExpression prints number" <| fun _ ->
                test <@ printExpression (Number 1) = "1"  @>
                test <@ printExpression (Number 2) = "2"  @>
                test <@ printExpression (Number 10) = "10"  @>

            testCase "BinaryOperationExpression prints operation" <| fun _ ->
                test <@ printExpression (BinaryOperationExpression {Left= Number 1; Operation = Plus; Right = Number 2}) = "1 + 2" @>
                test <@ printExpression (BinaryOperationExpression {Left= Number 1; Operation = Minus; Right = Number 2}) = "1 - 2" @>
                test <@ printExpression (BinaryOperationExpression {Left= Number 1; Operation = Multiply; Right = Number 2}) = "1 * 2" @>
                test <@ printExpression (BinaryOperationExpression {Left= Number 1; Operation = Divide; Right = Number 2}) = "1 / 2" @>
                test <@ printExpression (BinaryOperationExpression {Left= Number 1; Operation = Modulo; Right = Number 2}) = "1 % 2" @>

            testCase "Complex expression prints grouping" <| fun _ ->
                let expression1 = BinaryOperationExpression {
                    Left = Number 6;
                    Operation = Plus;
                    Right = BinaryOperationExpression { Left = Number 2; Operation = Multiply; Right = Number 3 }}

                test <@ printExpression expression1 = "6 + (2 * 3)" @>

                let expression2 = BinaryOperationExpression {
                    Left = BinaryOperationExpression { Left = Number 2; Operation = Plus; Right = Number 3 }
                    Operation = Multiply;
                    Right = BinaryOperationExpression { Left = Number 2; Operation = Plus; Right = Number 3 }}
                test <@ printExpression expression2 = "(2 + 3) * (2 + 3)" @>
        ]

        testList "Evaluating expressions" [
            testCase "Evaluating numbers" <| fun _ ->
                test <@ evaluate (Number 1) = 1 @>
                test <@ evaluate (Number 2) = 2 @>
                test <@ evaluate (Number 10) = 10 @>

            testCase "Evaluating simple BinaryOperationExpressions" <| fun _ ->
                test <@ evaluate (BinaryOperationExpression { Left = Number 1; Operation = Plus; Right = Number 1 }) = 2 @>
                test <@ evaluate (BinaryOperationExpression { Left = Number 1; Operation = Minus; Right = Number 1 }) = 0 @>
                test <@ evaluate (BinaryOperationExpression { Left = Number 2; Operation = Multiply; Right = Number 3 }) = 6 @>
                test <@ evaluate (BinaryOperationExpression { Left = Number 6; Operation = Divide; Right = Number 3 }) = 2 @>
                test <@ evaluate (BinaryOperationExpression { Left = Number 3; Operation = Modulo; Right = Number 2 }) = 1 @>
        ]
    ]