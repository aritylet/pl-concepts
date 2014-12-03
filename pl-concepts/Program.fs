// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

type expr =
    | CstI of int
    | Var of string
    | Prim of string * expr * expr

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let rec eval (e : expr) (env : (string * int) list) : int =
    match e with
    | CstI i  -> i
    | Var x -> lookup env x
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim _ -> failwith "unknown primitive"

let rec evalm (e : expr) env : int =
    match e with
    | CstI i  -> i
    | Var x -> lookup env x
    | Prim("+", e1, e2) -> evalm e1 env + evalm e2 env
    | Prim("*", e1, e2) -> evalm e1 env * evalm e2 env
    | Prim("-", e1, e2) ->
        let res = evalm e1 env - evalm e2 env
        if res < 0 then 0 else res 
    | Prim _ -> failwith "unknown primitive"

let env = [("x", 42); ("y", 3)]
let exp1 = Prim("*", CstI 3, Var "x")

[<EntryPoint>]
let main argv = 
    Console.WriteLine("hello")
    0 // return an integer exit
