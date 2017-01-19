
module Competences.Main

open System
open System.IO

open FSharp.Collections.ParallelSeq

open Competences.Tree
open Competences.Text
open Competences.Rename
open Competences.Export

//-------------------------------------------------------------------------------------------------

/// execute a file
let doubleClick (path : string) = 
    System.Diagnostics.Process.Start(path) 
    |> ignore

//-----
let allMetiersMaj path =
    File.ReadLines(path)
    |> PSeq.map (fun (str:string) -> str.ToLowerInvariant())

let allMetiers path =
    File.ReadLines(path)
    |> PSeq.collect tokeniser
let allAdj path =
    File.ReadLines(path)
    |> PSeq.map (fun (str:string) -> str.Substring(0, str.IndexOf(' ') ) )

//-------------------------------------------------------------------------------------------------
// main

let getModelPath (args:string array) =
    if args.Length >= 1 then args.[0] 
    else 
        printf "please print the name of your model file (ex : \"model.csv\") : "
        match System.Console.ReadLine() with 
        | "" -> "model.csv"
        | str -> str
(*
[<EntryPoint>]
let main(args) =
    let dossier = new DirectoryInfo("outputs/")
    let model = new Model(getModelPath args)
    let modelMetiers = 
        [|allMetiers "metiersWiki.txt"; allMetiers "metiersChild.txt"; allMetiersMaj "metiersMaj.txt"|]
        |> PSeq.concat 
        |> model.Restriction
    let modelAdj = allAdj "adjectifs.txt" |> model.Restriction
    if not dossier.Exists then 
        Console.Write "no \"outputs\" directory (containing the participants csv) found"
        0
    else
        Directory.CreateDirectory("htmls/") |> ignore
        let questionTree = importTree "Tree.csv"
        let renamedTree = rename model modelMetiers modelAdj questionTree
        for fichier in dossier.GetFiles() do
            let email = fichier.Name
            let pseudo = email.Split('@').[0]
            let path =  "htmls/" + pseudo + ".html"
            let tree = importAnswers fichier.FullName questionTree |> copyLabels renamedTree
            exportJson path pseudo tree
            printfn "exported %s" pseudo
        exportJson "RawTree.html" "" questionTree
        exportJson "Tree.html" "" renamedTree
        exportCSV "Tree.Questionnaire" renamedTree
        //printTree renamedTree
        //doubleClick "Tree.html"
        printfn "job done"
        1
*)
//-------------------------------------------------------------------------------------------------
// analogy demo

[<EntryPoint>]
let main(args) =

    let model = Model(getModelPath args) //Model("model.csv")

    let demo (x:string) (fx:string) (y:string) =
        try
            let xv = model.Vector x
            let fxv = model.Vector fx
            let yv = model.Vector y
            let fyv = yv .+ (fxv .- xv)
            printfn "%s -> %s / %s -> ?%s" x fx y <| model.Word(fyv,[x;fx;y])
            //printfn "%s -> %s, %s -> ?%A" x fx y <| model.Words(fyv,5)
        with _ -> printfn "erreur, not found"

    let rec loop () =
        printfn "type \"quit\" or three words separated by spaces:"
        let words = System.Console.ReadLine().Split(' ')
        match words.Length with 
        | 1 when words.[0] = "quit" -> printfn "good bye"
        | 3 -> demo words.[0] words.[1] words.[2]; loop ()
        | _ -> printfn "Please print at least three words"; loop ()

    demo "chat" "chaton" "chien"
    demo "france" "paris" "russie"
    demo "france" "paris" "italie"
    demo "homme""femme" "roi"
    demo "homme" "femme" "cheval"

    loop ()
    1


//-------------------------------------------------------------------------------------------------
// question demo
(*
[<EntryPoint>]
let simulator arg =
    let tree = importTree "Tree.Questionnaire"
    let rec loop tree =
        let question = getQuestion tree
        printfn "please write yes, no, bof, display or quit to answer the question :\n\"%s\"" question.enonce
        match Console.ReadLine() with 
        | "display" ->
            exportJson "testTree.html" "" tree
            doubleClick "testTree.html"
            loop tree
        | "yes" | "y" -> validateQuestion tree question 1. |> loop
        | "no" | "n" -> validateQuestion tree question 0. |> loop
        | "bof" | "b" -> validateQuestion tree question 0.5 |> loop
        | "quit" -> 1
        | _ -> 
            printf "I did not undestand your answer, "
            loop tree
    loop tree
*)


