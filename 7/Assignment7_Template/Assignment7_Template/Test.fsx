#r "bin/Debug/netcoreapp3.1/Assignment7_Template.dll"
#r "bin/Debug/netcoreapp3.1/FParsec.dll"

open ImpParser
open StateMonad
open Eval
open FParsecLight
open JParsec

run pif "if" |> printfn "%A"