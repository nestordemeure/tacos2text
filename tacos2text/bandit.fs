
// algorithm to select question in order to explore cleverly the serach space

module Competences.Tree

open System

/// generate random numbers
let randomGenerator = System.Random()

//-------------------------------------------------------------------------------------------------
// TYPES

/// basic type of question
type Riasec = int
let (R:Riasec) = 0 
let (I:Riasec) = 1 
let (A:Riasec) = 2
let (S:Riasec) = 3
let (E:Riasec) = 4
let (C:Riasec) = 5

//-----

/// caracteristics of a question
type Question = { 
    category : Riasec
    enonce : string
    answer : float option
    knownAnswer : float option  
    clef : string list }

/// tree storing the questions and the stats so far
type Arbre = 
    | Vide
    | F of Question 
    | FDead of Question
    | N of Node 
    | NDead of Node 

/// node storing the stats so far and some questions
and Node = { 
   children : (string*Arbre) list 
   sum : float 
   n : float }

//-------------------------------------------------------------------------------------------------
// BUILDING THE QUESTION TREE

let questionOfEnonce = new System.Collections.Generic.Dictionary<string,Question>()

//-----

/// adds a question to a questionnaire (currently does not count the existing answers in the construction of the nodes)
let rec addQ question (clef : string list) arbre = 
    match clef, arbre with 
    | _, FDead _ | _, NDead _ -> failwith "addQ : nodes already known"
    | [], Vide -> F question 
    | [], _ -> "addQ : specify the key for : " + question.enonce |> failwith
    | _::_, F question2 -> "addQ : specify the key for : " + question2.enonce |> failwith
    | c::clef2, Vide -> N {children = [c, addQ question clef2 Vide]; sum = 0.; n = 0.} 
    | c::clef2, N node -> 
        /// adds the question into one of the element of the list (or build an element)
        let rec addToList l =
            match l with
            | [] -> [c, addQ question clef2 Vide]
            | (c2,arbre2)::l2 when c=c2 -> (c, addQ question clef2 arbre2)::l2
            | (c2,arbre2)::l2 -> (c2,arbre2)::(addToList l2) 
        N {node with children = addToList node.children}

//-----

/// seed for a question tree
let questionnaire = Vide

/// builds a question and adds it to a questionnaire
let addQuestion riasec enonce clef arbre = 
    let question = {category = riasec; enonce = enonce; answer = None; knownAnswer = None; clef = clef}
    questionOfEnonce.[enonce] <- question
    addQ question clef arbre

//-------------------------------------------------------------------------------------------------
// STORES ANSWERED QUESTIONS

/// seach the value associated with a key and try to apply f, delete the value if f return None
let rec listMapSingle f clef l =
    match l with 
    | [] -> sprintf "searchAndDestroy : no %A key" clef |> failwith
    | (clef2,value)::q when clef2 = clef -> (clef2,f value)::q
    | (clef2,value)::q -> (clef2,value)::(listMapSingle f clef q)

/// return true is a tree is alive
let isAliveTree tree = 
    match tree with 
    | FDead _ | NDead _ | Vide -> false 
    | _ -> true

/// is a tree deletable (leaf or without children)
let deleteNode tree =
    match tree with 
    | F question -> FDead question
    | N node when List.exists (snd >> isAliveTree) node.children |> not -> NDead node 
    | _ -> tree

//-----

/// modify the internal state of a tree to take an answer to a question into account
let rec feedTree clef answer tree =
    match tree, clef with
    | FDead _, _ | NDead _, _ -> failwith "feedTree : nodes already known" 
    | Vide, _ -> failwith "feedTree : the tree is empty"
    | F _, _::_ -> failwith "feedTree : key too specific"
    | N _, [] -> failwith "feedTree : key not specific enough"
    | F question, _ -> F {question with answer = Some answer} 
    | N node, c::clef2 ->
        // search for the next tree and prune it if it should never be picked again
        let traverse = feedTree clef2 answer >> deleteNode
        N {n=node.n + 1.; sum=node.sum + answer; children=listMapSingle traverse c node.children}

//-----

/// stores the result of the question and update the state of the tree
let validateQuestion tree question answer = feedTree question.clef answer tree

//-------------------------------------------------------------------------------------------------
// IMPORTATION

/// import a precomputed global tree
let importTree path =
    // adds a question to the tree
    let growTree arbre question =
        match question with 
        | [] -> arbre
        | enonce::clef -> 
            let simpleClef = List.filter (fun (key:string) -> key.StartsWith("ZZZ") |> not) clef
            addQuestion R enonce simpleClef arbre
    // read a file line by line
    System.IO.File.ReadLines(path)
    |> Seq.map (fun s-> s.Split ';' |> Array.toList)
    |> Seq.fold growTree questionnaire 

/// import someones answer from a "enonce;0.0" table
let importAnswers path questionTree =
    let growTree tree (answer : string array) = 
        let enonce = answer.[0]
        let answer = float answer.[1]
        let question = questionOfEnonce.[enonce]
        validateQuestion tree question answer
    System.IO.File.ReadLines(path)
    |> Seq.map (fun s-> s.Split ';')
    |> Seq.fold growTree questionTree

//-------------------------------------------------------------------------------------------------
// ASK FOR QUESTION

/// follow the uct method to score a node
let UCT ni wi t =
    let mean = wi/ni
    mean + sqrt(2.*System.Math.Log(t,2.)/ni) // classic UCT

/// score a tree given the number of trial that went through his parent
let score n arbre = 
    if n=0. then randomGenerator.NextDouble() else // paret never explored before
    match arbre with
    | FDead _ | NDead _ -> -1.
    | Vide -> failwith "score : arbre vide"
    | F _ -> infinity // pick the first leaf found
    | N node when node.n = 0. -> infinity // pick the first unexplored node found
    | N node -> UCT node.n node.sum n

/// get the next question to ask given the tree
let rec getQuestion arbre = 
    match arbre with
    | FDead _ | NDead _ | Vide -> failwith "getQuestion : arbre vide"
    | F question -> question
    | N node when node.children.Length = 0 -> failwith "getQuestion : node without childrens"
    | N node -> let (_,arbre2) = List.maxBy (snd >> score node.n) node.children
                getQuestion arbre2
