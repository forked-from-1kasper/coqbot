module CoqCompatibility

open Logic.Coq_bot_core
open System
open System.Text

Console.InputEncoding <- Encoding.UTF8
Console.OutputEncoding <- Encoding.UTF8

let rec private convert current acc =
    if current = 0 then acc
    else
        match (current % 2) with
        | 1 -> convert (current / 2) (List.append acc [1])
        | 0 -> convert (current / 2) (List.append acc [0])
        | _ ->
            failwithf
                "Reptiloids are accidentally the remainder of division!"

let private CoqStringToList s =
    let rec support current acc =
        match current with
        | Logic.String (x, st1) -> support st1 (x :: acc)
        | Logic.EmptyString -> acc
    support s []

let private AsciiToNum (Logic.Ascii (a, b, c, d, e, f, g, h)) = // TODO: fix me, please, plea‐a‐ase!
    let BoolPow power = function
        | true -> pown 2 power // 2 ^ n, not 1 ^ n
        | false -> 0
    [a; b; c; d; e; f; g; h]
    |> List.zip [0..7]
    |> List.fold (fun acc (power, num) -> acc + (BoolPow power num)) 0

let private NumToCoqAscii raw =
    let n = raw % 256 // only 8-bit numbers

    let boolList =
        convert n []
        |> List.map
            (fun x -> if x = 1 then true else false)
    match if boolList.Length <> 8 then
              List.append boolList (List.init (8 - boolList.Length) (fun _ -> false))
          else boolList with
    | [a; b; c; d; e; f; g; h] -> Logic.Ascii(a, b, c, d, e, f, g, h) // yo‐ho‐ho!
    | _ -> failwithf "ЕГГОГ"

let public StringToCoqString (s : string) =
    Encoding.UTF8.GetChars ([| for c in s -> byte c |])
    |> List.ofArray
    |> List.map (int >> NumToCoqAscii)
    |> List.rev
    |> List.fold
        (fun acc current ->
            Logic.String (current, acc))
        Logic.EmptyString

let public CoqStringToString =
    CoqStringToList
    >> List.map AsciiToNum
    >> List.rev
    >> Array.ofList
    >> Array.map byte
    >> Encoding.UTF8.GetString

let public CoqZArithToInt N =
    let rec support current acc power =
        match current with
        | Logic.XI xs -> support xs (acc + (pown 2 power)) (power + 1) // XI is digit 1
        | Logic.XO xs -> support xs acc (power + 1) // XO is digit 0
        | Logic.XH -> acc + (pown 2 power) // XH is also digit 1
    match N with
    | Logic.Npos start -> support start 0 0
    | Logic.N0 -> 0

let public IntToCoqZArith n =
    match n with
    | 0 -> Logic.N0
    | n' ->
        convert n' []
        |> List.tail // head is here always 1
        |> List.fold
            (fun acc current ->
                if current = 1 then Logic.XI acc
                else Logic.XO acc)
            Logic.XH
        |> Logic.Npos
