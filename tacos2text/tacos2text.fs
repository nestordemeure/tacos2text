(*
notes :
during the importation, i should make sure that all vectors have the same size (it causes vicious bugs)
should the aggregations of vector make sure the output has been scaled (average instead of sum) in case it will be combined latter ?
*)

module Competences.Text

// concurent dictionnary
open System.Collections.Generic
open System.Collections.Concurrent
// parallelism
open System.Threading.Tasks
open FSharp.Collections.ParallelSeq
// serialization
open System.IO
open ProtoBuf

type Text = string seq
type Word = string
type Vector = float array

/// to pipe into unit function
let inline (|->) x f = f x ; x

/// to pipe from an option
let inline (|~>) xOpt f = 
    match xOpt with 
    | Some x -> Some (f x)
    | None -> None

//-------------------------------------------------------------------------------------------------
// vector operations

/// dot product between two vectors
let (.*) (v1:Vector) (v2:Vector) = Array.fold2 (fun acc x1 x2 -> acc + x1*x2) 0. v1 v2

/// pointwise addition of vectors
let (.+) (v1:Vector) (v2:Vector) : Vector = Array.map2 (+) v1 v2

/// pointwise subtraction of vectors
let (.-) (v1:Vector) (v2:Vector) : Vector = Array.map2 (-) v1 v2

/// multiply a vector by a scalar
let weightVector (x:float) (v:Vector) : Vector = Array.map (fun y -> x*y) v

/// compute the norm of a vector
let norm (v:Vector) = Array.fold (fun acc x -> acc + x*x) 0. v |> sqrt

/// divide a vector by its norm
let normalize v = 
    match norm v with 
    | 0. | 1. -> v
    | n -> weightVector ( 1./n ) v

/// compute the cosine distance between two vectors assuming that the first has been normalized
/// (the norms can be precomputed (or omited for comparaisons) to reduce the computational cost)
let quickCosineDistance (vNormed:Vector) (v:Vector) =
    let mutable dotProduct = 0.
    let mutable normSquared = 0.
    for i = 0 to vNormed.Length - 1 do 
        let x1 = vNormed.[i]
        let x2 = v.[i]
        dotProduct <- dotProduct + x1*x2 
        normSquared <- normSquared + x2*x2
    1. - dotProduct/sqrt(normSquared)

/// build a vector full of zeros of the given size
let zeroVector size : Vector = Array.zeroCreate size

//-------------------------------------------------------------------------------------------------
// model importation

/// add te given line to the memory if it is long enough
let inline addLine (cDict:ConcurrentDictionary<Word, Vector>) (line:string) = 
    if line.IndexOf ';' > 2 then 
        let content = line.Split ';'
        let word = content.[0]
        let vector = Array.init (content.Length-1) (fun i -> float content.[i+1])
        cDict.[word] <- vector

//-----

/// build a dictionnary using the data at the given path (~4minutes)

let parseModel path =
    printfn "loading new model (it might take a few minutes)..."
    let model = new ConcurrentDictionary<Word, Vector>()
    Parallel.ForEach(File.ReadLines(path), addLine model) |> ignore
    //for line in File.ReadLines(path) do addLine model line // DEBUG
    model

/// save a model at a given path
let serializeModel path (model:ConcurrentDictionary<Word, Vector>) =
    printfn "caching the model for futur loadings..."
    use file = File.Create(path)
    Serializer.Serialize(file,model)

/// load a model cached at a given path (~30secondes)
let deserializeModel path = 
    printfn "loading cached model..."
    use file = File.OpenRead(path)
    Serializer.Deserialize<ConcurrentDictionary<Word, Vector>>(file)

//-----

/// import a model (from a .csv or a cache if it has already been loaded)
let importModel (path:string) =
    let rawName = path.Substring(0, path.LastIndexOf('.') )
    let cachedPath = sprintf "%s.model" rawName
    if File.Exists cachedPath then deserializeModel cachedPath
    else parseModel path |-> serializeModel cachedPath

//-------------------------------------------------------------------------------------------------
// model manipulation

/// represents a word embedding model using a dictionnary
type Model(memory:IDictionary<Word, Vector>) =
    /// the length of a typical vector
    let neutralVector = Seq.head memory |> fun kv -> kv.Value.Length |> zeroVector

    /// build a new model using the path to a .csv containing the data
    new(path) =
        let memory = importModel path
        printfn "%d words in memory, ready to work" memory.Count
        Model(memory)

    /// take a seq of words an output a new model wihch contains only those words
    member this.Restriction(words:Word seq) =
        words 
        |> PSeq.filter memory.ContainsKey
        |> PSeq.map (fun w -> w, memory.[w])
        |> dict 
        |-> fun d -> printfn "restriction of %d words established" d.Count
        |> Model

    //-----

    /// tries to get the vector associated with this word o(1), return an option type
    member this.TryVector(word:Word) : Vector option = 
        let result = ref [||]
        match memory.TryGetValue(word,result) with 
        | true -> Some (!result)
        | false -> None
    /// get a typical vector full of zero
    member this.Vector() : Vector = Array.copy neutralVector
    /// get the vector associated with this word o(1)
    member this.Vector(word:Word) = 
        memory.[word]
    /// get the vector associated with a seq of weighted words (the result is not divided by the sum of weights)
    member this.Vector(words:(Word*float) seq) =
        words 
        |> Seq.choose (fun (w,weight) -> this.TryVector(w) |~> weightVector weight)
        |> Seq.fold (.+) neutralVector
    /// get the vector associated with a seq of words (the result is not divided by the number of words)
    member this.Vector(words:Word seq) =
        words 
        |> Seq.choose (this.TryVector)
        |> Seq.fold (.+) neutralVector
    /// get the word closer to this vector o(n)
    member this.Word(vector:Vector) =
        let vectorNormalized = normalize vector
        memory
        |> Seq.minBy (fun (kv:KeyValuePair<Word,Vector>) -> quickCosineDistance vectorNormalized kv.Value)
        |> fun kv -> kv.Key
    /// get the word closer according to a given distance mesure o(n)
    member this.CustomWord(distanceMesure : Vector -> float) =
        memory
        |> Seq.minBy (fun (kv:KeyValuePair<Word,Vector>) -> distanceMesure kv.Value)
        |> fun kv -> (kv.Key, distanceMesure kv.Value)
    /// get the word closer to this vector but not included in the exceptions o(n)
    member this.Word(vector:Vector,exceptions:Word list) =
        let vectorNormalized = normalize vector
        memory
        |> Seq.filter (fun (kv:KeyValuePair<Word,Vector>) -> List.contains kv.Key exceptions |> not)
        |> Seq.minBy (fun (kv:KeyValuePair<Word,Vector>) -> quickCosineDistance vectorNormalized kv.Value)
        |> fun kv -> kv.Key
    /// get the word closer according to a given distance mesure o(n)
    member this.CustomWord(distanceMesure : Vector -> float,exceptions:Word list) =
        memory
        |> Seq.filter (fun (kv:KeyValuePair<Word,Vector>) -> List.contains kv.Key exceptions |> not)
        |> Seq.minBy (fun (kv:KeyValuePair<Word,Vector>) -> distanceMesure kv.Value)
        |> fun kv -> (kv.Key, distanceMesure kv.Value)
    /// get the words closer to this vector o(n)
    member this.Words(vector:Vector,k:int) =
        let vectorNormalized = normalize vector
        /// insert a key with an associated dist into a sorted list of length n at most
        let rec insertSort n key dist l =
            match l with 
            | _ when n <= 0 -> []
            | [] -> [key,dist]
            | (_,d)::_ when d >= dist -> (key,dist)::l 
            | (k,d)::q -> (k,d)::(insertSort (n-1) key dist q) 
        memory
        |> Seq.fold (fun acc (kv:KeyValuePair<Word,Vector>) -> insertSort k kv.Key (quickCosineDistance vectorNormalized kv.Value) acc) []
        |> List.map fst

//-------------------------------------------------------------------------------------------------
// text manipulation

/// stem words by cutting them at a given character ex : "Technicien/Technicienne"->"Technicien"
let stemmer (c:char) (str:Word) = 
    let pos = str.IndexOf(c)
    if pos > 0 then str.Substring(0,pos) else str

/// is the word longer than the given size
let longWord shortSize (word:Word) = word.Length > shortSize

/// is the word absent from the list of meaningless words
let usefulWord meaninglessWords (word:Word) = not <| List.contains word meaninglessWords

//-----

/// turns a text into an array of tokens
let tokeniser (txt:string) : Word array=
    let meaninglessWords = 
        ["des";"les";"chez";"dans";"sur";"une";"avec";"aux";"leurs";"leur";"comme";"etc";"lors";"mes";"pour";
        "par";"tout";"mon";"mes";"être";"suis";"est";"quand";"son";"même";"pas";"faire";"selon";"qui";"que";
        "besoin";"beaucoup";"souvent";"même";"moi";"sont";"fait";"fais";"très";"peu";"quelle";"tels";"gros";
        "montre";"façon";"façons";"plutôt";"tel";"passe";"autour";"près";"avant";"toute";"non";"jour";"entremise";
        "grand";"quelque";"généralement";"toutes";"sortes";"jour";"toujours";"autres";"autre";"vis"]
    txt
    |> fun (str:string) -> str.ToLowerInvariant() // lower
    |> fun (str:string) -> str.Split(' ','.',';','\'','’','!','?',':','\n','\t',',','(',')','-') // split
    |> Array.map (stemmer '/')  // stemming of words containing a '/'
    |> Array.filter (fun (str:string) -> longWord 2 str && usefulWord meaninglessWords str) // filter words

//-----

/// turns a sequence of string into a single word
let summarize (model:Model) (text:Text) = 
    text 
    |> Seq.collect tokeniser // turns a text into key words
    |> model.Vector // average vector
    |> model.Word // vector2word

//-------------------------------------------------------------------------------------------------

/// returns the distance between a vector and the vector associated with a given word
let distanceVectorWord (model:Model) vector (word:Word) = quickCosineDistance (normalize vector) (model.Vector(word))

(*
méthode pour calculer la distance entre un mot et une phrase
au lieu de calculer la distance du mot a la moyenne des vecteur de la phrase
on peux caculer la moyenne/median de la distance du mot au vecteurs de la phrase
*)