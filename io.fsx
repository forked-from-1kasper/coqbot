#r "coqCompatibility.dll"

open Logic
open Logic.Coq_bot_core
open System
open System.IO
open System.Text
open System.Net
open System.Net.Sockets
open CoqCompatibility

let server = Coq_bot_core.server |> CoqStringToString
let port = Coq_bot_core.port |> CoqZArithToInt

//let rec IORun f =
//    match f with
//    | coq_IO.PutStrLn (s, f) ->
//        printfn "%s" (CoqStringToString s)
//        f () |> IORun
//    | coq_IO.GetLine (_, f) ->
//        f (Console.ReadLine () |> StringToCoqString) |> IORun
//    | coq_IO.Unit _ -> ()

let rec IONetRun (writer : StreamWriter) (reader : StreamReader) f =
    match f with
    | coq_IO.IOString (s, f) ->
        let s1 = CoqStringToString s

        printfn "> %s" s1
        writer.WriteLine(s1)
        IONetRun writer reader f
    //| coq_IO.GetLine (_, f) ->
    //    let line = reader.ReadLine ()
    //    printfn "- %s" line
    //    f (StringToCoqString line) |> IONetRun writer reader
    | coq_IO.Delay _ -> ()

let ircClient = new TcpClient()
ircClient.Connect(server, port)

let ircReader = new StreamReader(ircClient.GetStream())
let ircWriter = new StreamWriter(ircClient.GetStream())

let spiritOfThePrint x =
    printfn "- %s" x
    x

ircWriter.AutoFlush <- true

IONetRun ircWriter ircReader init
while ircReader.EndOfStream = false do
    ircReader.ReadLine()
    |> spiritOfThePrint
    |> StringToCoqString
    |> handler
    |> List.map (IONetRun ircWriter ircReader)
    |> ignore
