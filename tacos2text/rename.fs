module Competences.Rename

open System
open System.IO
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

open Competences.Tree
open Competences.Text

//-------------------------------------------------------------------------------------------------
// FUNCTIONS

/// returns something only if t is a leaf
let ifLeaf tree a b =
      match tree with 
      | F _ | FDead _ -> a 
      | _ -> b

/// display a list as a string for debuggnig purposes
let printSeq sep s = Seq.reduce (fun acc str -> acc + sep + str) s

/// cache associations between words and vectors to simplify conflict resolution when assigning labels to vectors
type Association() =
      let dictVW = Dictionary<Vector,Word>()
      let dictWV = Dictionary<Word,float*Vector>()
      let mutable allWords = []
      member this.Add(v,w,dist) =
            dictVW.[v]<-w
            dictWV.[w]<-(dist,v)
            allWords <- w::allWords
      member this.Get(v:Vector) = dictVW.[v]
      member this.Get(w:Word) = dictWV.[w]
      member this.Contain(w:Word) = dictWV.ContainsKey w
      member this.AllWords = allWords

//-------------------------------------------------------------------------------------------------

/// adjust the disadvantage given to the adjectives in function of the depth into the tree
let weightOfDepth =
      function
      | 1 -> 0.8  //50%
      | 2 -> 1.7  //80%
      | 3 -> 2.   //95%
      | _ -> 1.7

/// pick a word from the two model (downweighting adjectives) and output the model and a weighted distance
let pickWord (modelMetiers:Model) (modelAdj:Model) weight vector =
      let metier = modelMetiers.Word vector
      let qMetier = distanceVectorWord modelMetiers vector metier
      let adj = modelAdj.Word vector
      let qAdj = distanceVectorWord modelAdj vector adj |> (*) weight
      if  qAdj < qMetier then qAdj, adj else qMetier, metier

/// pick a word from the two model (downweighting adjectives) and output the model and a weighted distance (adds some exceptions, words that cannot be picked)
let pickWordExcept (modelMetiers:Model) (modelAdj:Model) weight vector exceptions =
      let metier = modelMetiers.Word(vector,exceptions)
      let qMetier = distanceVectorWord modelMetiers vector metier
      let adj = modelAdj.Word(vector,exceptions)
      let qAdj = distanceVectorWord modelAdj vector adj |> (*) weight
      if qAdj < qMetier then qAdj, adj else qMetier, metier

//-------------------------------------------------------------------------------------------------

/// compute a label for the given vector, deal with conflict by assigning the label to minimise the total distance
let memorizeLabel (modelMetiers:Model) (modelAdj:Model) (assoc:Association) depth vector1 = 
      let weight = weightOfDepth depth
      let newdist,newLabel = pickWord modelMetiers modelAdj weight vector1
      match assoc.Contain newLabel with 
      | false -> assoc.Add(vector1,newLabel,newdist)
      | true ->
            let dist1,newLabel1 = pickWordExcept modelMetiers modelAdj weight vector1 assoc.AllWords
            let newdist2,vector2 = assoc.Get(newLabel)
            let dist2,newLabel2 = pickWordExcept modelMetiers modelAdj weight vector2 assoc.AllWords
            let totalDistance1 = newdist + dist2
            let totalDistance2 = newdist2 + dist1
            if totalDistance1 < totalDistance2 then 
                  assoc.Add(vector1,newLabel,newdist) 
                  assoc.Add(vector2,newLabel2,dist2)
            else  assoc.Add(vector1,newLabel1,dist1)

/// renames a tree recurcively
let rec renameRec (model:Model) (modelMetiers:Model) (modelAdj:Model) depth tree =
   match tree with
   | Vide -> model.Vector() , tree
   | F question | FDead question -> 
      let vector = question.enonce |> tokeniser |> model.Vector |> normalize
      vector , tree
   | N node ->
      let assoc = Association()
      let childrenVectors = List.map (fun (oldLabel,child) -> renameRec model modelMetiers modelAdj (depth+1) child) node.children // vector,tree
      let vector = childrenVectors |> List.map (fun (v,t) -> memorizeLabel modelMetiers modelAdj assoc depth v ; v) |> List.reduce (.+) // memorization; reduce vectors
      let childrens = List.map2 (fun (v:Vector,t) (oldLabel,_) -> ifLeaf t oldLabel (assoc.Get v),t) childrenVectors node.children // word, tree
      vector, N {node with children = childrens}
   | NDead node -> 
      let assoc = Association()
      let childrenVectors = List.map (fun (oldLabel,child) -> renameRec model modelMetiers modelAdj (depth+1) child) node.children // vector,tree
      let vector = childrenVectors |> List.map (fun (v,t) -> memorizeLabel modelMetiers modelAdj assoc depth v ; v) |> List.reduce (.+) // memorization; reduce vectors
      let childrens = List.map2 (fun (v:Vector,t) (oldLabel,_) -> ifLeaf t oldLabel (assoc.Get v),t) childrenVectors node.children // word, tree
      vector, NDead {node with children = childrens}

/// rename the root of a tree in parallel, without outputing a vector and renaming the top layeur
let rootRename model modelMetiers modelAdj tree =
   match tree with
   | N node ->
      let childrens = 
         node.children
         |> PSeq.ordered
         |> PSeq.map (fun (nom,arbre) -> nom, snd <| renameRec model modelMetiers modelAdj 1 arbre) 
         |> PSeq.toList
      N {node with children = childrens}
   | NDead node -> 
      let childrens = 
         node.children 
         |> PSeq.ordered
         |> PSeq.map (fun (nom,arbre) -> nom, snd <| renameRec model modelMetiers modelAdj 1 arbre) 
         |> PSeq.toList
      NDead {node with children = childrens}
   | _ -> tree

//-------------------------------------------------------------------------------------------------
(*
/// cache associations between words and vectors to simplify conflict resolution when assigning labels to vectors
type Association() =
      let dictVW = Dictionary<(Vector[])list,Word>()
      let dictWV = Dictionary<Word,float*(Vector[])list>()
      let mutable allWords = []
      member this.Add(v,w,dist) =
            dictVW.[v]<-w
            dictWV.[w]<-(dist,v)
            allWords <- w::allWords
      member this.Get(v:(Vector[])list) = dictVW.[v]
      member this.Get(w:Word) = dictWV.[w]
      member this.Contain(w:Word) = dictWV.ContainsKey w
      member this.AllWords = allWords

//-------------------------------------------------------------------------------------------------

/// returns true only if t is a leaf
let isLeaf tree =
      match tree with 
      | F _ | FDead _ -> true
      | _ -> false

let averageDistance vector normalizedVectors = 
      Array.averageBy (fun v -> quickCosineDistance v vector) normalizedVectors
let meanAverageDistance vectorsListNormalized vector = 
      List.averageBy (averageDistance vector) vectorsListNormalized

//-------------------------------------------------------------------------------------------------

/// pick a word from the two model (downweighting adjectives) and output the model and a weighted distance
let pickWord (modelMetiers:Model) (modelAdj:Model) vectorList =
      let metier, qMetier = modelMetiers.CustomWord(meanAverageDistance vectorList)
      let adj, qAdj = modelAdj.CustomWord(meanAverageDistance vectorList)
      if  qAdj*1.7 < qMetier then qAdj*1.7, adj else qMetier, metier

/// pick a word from the two model (downweighting adjectives) and output the model and a weighted distance (adds some exceptions, words that cannot be picked)
let pickWordExcept (modelMetiers:Model) (modelAdj:Model) vectorList exceptions =
      let metier, qMetier = modelMetiers.CustomWord(meanAverageDistance vectorList, exceptions)
      let adj, qAdj = modelAdj.CustomWord(meanAverageDistance vectorList, exceptions)
      if  qAdj*1.7 < qMetier then qAdj*1.7, adj else qMetier, metier

//-------------------------------------------------------------------------------------------------

/// compute a label for the given vector, deal with conflict by assigning the label to minimise the total distance
let memorizeLabel (modelMetiers:Model) (modelAdj:Model) (assoc:Association) vectorList1 = 
      let newdist,newLabel = pickWord modelMetiers modelAdj vectorList1
      match assoc.Contain newLabel with 
      | false -> assoc.Add(vectorList1,newLabel,newdist)
      | true ->
            let dist1,newLabel1 = pickWordExcept modelMetiers modelAdj vectorList1 assoc.AllWords
            let newdist2,vectorList2 = assoc.Get(newLabel)
            let dist2,newLabel2 = pickWordExcept modelMetiers modelAdj vectorList2 assoc.AllWords
            let totalDistance1 = newdist + dist2
            let totalDistance2 = newdist2 + dist1
            if totalDistance1 < totalDistance2 then 
                  assoc.Add(vectorList1,newLabel,newdist) 
                  assoc.Add(vectorList2,newLabel2,dist2)
            else  assoc.Add(vectorList1,newLabel1,dist1)

//-------------------------------------------------------------------------------------------------

/// renames a tree recurcively
let rec renameRec (model:Model) (modelMetiers:Model) (modelAdj:Model) tree =
   match tree with
   | Vide -> 
      [] , tree
   | F question | FDead question -> 
      let vectors = question.enonce |> tokeniser |> Array.choose model.TryVector |> Array.map normalize
      if Array.isEmpty vectors 
      then [],tree 
      else [vectors] , tree
   | N node ->
      let assoc = Association()
      let memo oldLabel (vectorList,arbre) =
            if isLeaf arbre then assoc.Add(vectorList,oldLabel,0.) 
            else if List.isEmpty vectorList then assoc.Add(vectorList,"UNKNOWN",0.) 
            else memorizeLabel modelMetiers modelAdj assoc vectorList
      let vectorList, newChildren =
            node.children
            |> List.map (fun (oldLabel,t) -> renameRec model modelMetiers modelAdj t |-> memo oldLabel)
            |> fun l -> List.collect (fst) l,
                        List.map (fun (vl:Vector[]list,t) -> assoc.Get(vl),t) l
      vectorList, N {node with children = newChildren}
   | NDead node -> 
      let assoc = Association()
      let memo oldLabel (vectorList,arbre) =
            if isLeaf arbre then assoc.Add(vectorList,oldLabel,0.) 
            else if List.isEmpty vectorList then assoc.Add(vectorList,"UNKNOWN",0.) 
            else memorizeLabel modelMetiers modelAdj assoc vectorList
      let vectorList, newChildren =
            node.children
            |> List.map (fun (oldLabel,t) -> renameRec model modelMetiers modelAdj t |-> memo oldLabel)
            |> fun l -> List.collect (fst) l,
                        List.map (fun (vl:Vector[]list,t) -> assoc.Get(vl),t) l
      vectorList, NDead {node with children = newChildren}

/// rename the root of a tree in parallel, without outputing a vector and renaming the top layeur
let rootRename model modelMetiers modelAdj tree =
   match tree with
   | N node ->
      let childrens = 
         node.children
         |> PSeq.ordered
         |> PSeq.map (fun (nom,arbre) -> nom, snd <| renameRec model modelMetiers modelAdj arbre) 
         |> PSeq.toList
      N {node with children = childrens}
   | NDead node -> 
      let childrens = 
         node.children 
         |> PSeq.ordered
         |> PSeq.map (fun (nom,arbre) -> nom, snd <| renameRec model modelMetiers modelAdj arbre) 
         |> PSeq.toList
      NDead {node with children = childrens}
   | _ -> tree
*)
//-------------------------------------------------------------------------------------------------

/// output a new tree with new names for the nodes
let rename model modelMetiers modelAdj tree = 
   printf "starting to rename a tree... "
   try rootRename model modelMetiers modelAdj tree
   finally printfn "tree renamed"

//-------------------------------------------------------------------------------------------------

/// given two tree with the same structure, output a new tree with labels taken from the source
let rec copyLabels sourceTree tree = 
   match sourceTree, tree with 
   | (N sourceNode | NDead sourceNode), N node ->
      let newChildrens = List.map2 (fun (sw,st) (w,t) -> sw, copyLabels st t) sourceNode.children node.children
      N {node with children=newChildrens}
   | (N sourceNode | NDead sourceNode), NDead node ->
      let newChildrens = List.map2 (fun (sw,st) (w,t) -> sw, copyLabels st t) sourceNode.children node.children
      NDead {node with children=newChildrens}
   | _ -> tree
