
module Competences.Export

open System
open System.IO

open Competences.Tree

//-------------------------------------------------------------------------------------------------
// CSV

let exportCSV path arbre =
    use streamWriter = new StreamWriter(path, false)
    /// write the tree line by line
    let rec export arbre line = 
        match arbre with 
        | Vide -> ()
        | F question | FDead question -> streamWriter.WriteLine(question.enonce + line)
        | N node | NDead node ->
            List.iter (fun (s,a) -> export a (line + ";" + s)) node.children
    export arbre ""

//-------------------------------------------------------------------------------------------------
// DISPLAY

// output a color given the number of trial and the sum of the results (each between 0. and 1.)
let colorOfTree n w =
    let m = w/n
    match n with 
    | 0. -> ConsoleColor.White
    | _ when m > 0.8 -> ConsoleColor.DarkBlue
    | _ when m > 0.7 -> ConsoleColor.Blue
    | _ when m > 0.6 -> ConsoleColor.DarkCyan
    | _ when m > 0.52 -> ConsoleColor.Cyan
    | _ when m < 0.2 -> ConsoleColor.DarkRed
    | _ when m < 0.3 -> ConsoleColor.Red
    | _ when m < 0.4 -> ConsoleColor.Red
    | _ when m < 0.48 -> ConsoleColor.Magenta
    | _ -> ConsoleColor.Green
//[|Black; DarkBlue; DarkGreen; DarkCyan; DarkRed; DarkMagenta; DarkYellow; Gray; DarkGray; Blue; Green; Cyan; Red; Magenta; Yellow; White|]

/// give a grade, ready to display, to a node
let noteOfNode n w = 
    match n with 
    | 0. -> ""
    | _ -> 100.*w/n |> int |> sprintf " : %i%%"

//-----

/// display a string in a given color
let printc s c = 
    System.Console.ForegroundColor <- c
    printf "%s" s

/// display a list of element, each with a given color
let rec printcl l = 
    match l with 
    | [] -> ()
    | (c,s)::q -> printcl q ; printc s c

//-----
let rec printTreeRec arbre label previously = 
    match arbre with 
    | Vide -> printc label ConsoleColor.White
    | F question | FDead question -> 
        match question.answer with 
        | None -> colorOfTree 0. 0. |> printc question.enonce
        | Some w -> colorOfTree 1. w |> printc question.enonce
    | N node | NDead node -> 
        let color = colorOfTree node.n node.sum
        let note = noteOfNode node.n node.sum
        printc (label+note) color
        let rec printBranches branches = 
            printc "\n" color
            match branches with 
            | [] -> ()
            | [c,a] -> 
                (color,"└─")::previously |> printcl  
                (color,"  ")::previously |> printTreeRec a c
            | (c,a)::q ->
                (color,"├─")::previously |> printcl  
                (color,"│ ")::previously |> printTreeRec a c
                printBranches q
        printBranches node.children

//-----

/// display a tree in ascii
let printTree arbre = 
    printTreeRec arbre "." []
    System.Console.ForegroundColor <- ConsoleColor.White
    printf "\n"

//-------------------------------------------------------------------------------------------------
// HTML DISPLAY

/// represents a Json
type Champ = 
| Ca of string * string 
| Li of string * (Json list)
and Json = Json of Champ List

//-----

/// from rgb to hexadecimal string
let colorOfRGB r g b = String.Format("#{0:X2}{1:X2}{2:X2}",r,g,b)

/// use a float from 0. to 1. and returns a color (string) from red to grey to green
let colorOfPercent x =
    match x with 
    | _ when x < 0.5 ->
        let y = 2.*x
        let r = 255.*(1.-y) + 240.*y
        let g = 240.*y
        let b = 240.*y
        colorOfRGB (int r) (int g) (int b)
    | _ ->
        let y = 2.*(x-0.5)
        let r = 240.*(1.-y)
        let g = 240.*(1.-y) + 255.*y
        let b = 240.*(1.-y)
        colorOfRGB (int r) (int g) (int b)

/// use a float from 0. to 1. and returns a percent (string)
let stringOfPercent x = x*100. |> int |> sprintf "%d%%"

//-----

/// used to write a json as a string
let rec stringOfChamp champ =
    match champ with 
    | Ca (nom,content) -> sprintf "\"%s\":\"%s\"" nom content 
    | Li (nom,[]) -> "null"
    | Li (nom,jsonList) ->
        jsonList
        |> List.map stringOfJson 
        |> List.reduce (sprintf "%s,%s")
        |> sprintf "\"%s\":[%s]" nom
and stringOfJson (Json json) =
    match json with 
    | [] -> "null"
    | _ -> json 
           |> List.map stringOfChamp 
           |> List.reduce (sprintf "%s,\n%s") 
           |> sprintf "{%s}"

//-----

/// takes a tree and returns a json representation containing display informations
let rec jsonOfTree label parent arbre = 
    match arbre with 
    | Vide -> Json [Ca ("name",label) ; Ca ("parent",parent)]
    | F question | FDead question -> 
        match question.answer with 
        | None -> Json [Ca ("name",question.enonce); Ca ("parent",parent);
                        Ca ("percent","inconnu")]
        | Some note -> Json [Ca ("name",question.enonce); Ca ("parent",parent); 
                             Ca ("color",colorOfPercent note); Ca ("percent",stringOfPercent note)]
    | N node | NDead node -> 
        let color = if node.n = 0.0 then "null" else node.sum/node.n |> colorOfPercent
        let percent = if node.n = 0.0 then "inconnu" else node.sum/node.n |> stringOfPercent
        let childrens = List.map (fun (newLabel,newArbre) -> jsonOfTree newLabel label newArbre) node.children
        match childrens with 
        | [] -> Json [Ca ("name",label); Ca ("parent",parent); Ca ("color",color); Ca ("percent",percent)]
        | _   -> Json [Ca ("name",label); Ca ("parent",parent); Li ("children",childrens); 
                       Ca ("color",color); Ca ("percent",percent)]

//-----

/// take a tree and returns a string
let jsonStringOfTree pseudo arbre =
    jsonOfTree pseudo "null" arbre
    |> stringOfJson

/// export a json as an html display using D3
let exportJson path pseudo arbre =
    let intro = """
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">

    <title>Arbre de compétences</title>

    <style>
	
	.node {
		cursor: pointer;
	}
	.node circle {
	  fill: #fff;
	  stroke: steelblue;
	  stroke-width: 3px;
	}
	.node text {
	  font: 12px sans-serif;
	}
	.link {
	  fill: none;
	  stroke: #ccc;
	  stroke-width: 2px;
	}
	
    </style>

  </head>

  <body>
<!-- load the d3.js library -->	
<script src="http://d3js.org/d3.v3.min.js"></script>
	
<script>
var treeData = [
"""
    let concl = """
];
// ************** Generate the tree diagram	 *****************
var margin = {top: 10, right: 20, bottom: 10, left: 120},
	width = 1250 - margin.right - margin.left,
	height = 650 - margin.top - margin.bottom;
	
var i = 0,
	duration = 750,
	root;
var tree = d3.layout.tree()
	.size([height, width]);
var diagonal = d3.svg.diagonal()
	.projection(function(d) { return [d.y, d.x]; });
var svg = d3.select("body").append("svg")
	.attr("width", width + margin.right + margin.left)
	.attr("height", height + margin.top + margin.bottom)
  .append("g")
	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
root = treeData[0];
root.x0 = height / 2;
root.y0 = 0;
  
var tooltip = d3.select("body")
    .append("div")
    .style("position", "absolute")
    .style("z-index", "10")
    .style("visibility", "hidden")
    .style("border", "1px solid #333")
    .style("background-color", "#161616")
    .style("border-radius", "3px")
    .style("padding", "2px")
    .style("color", "#fff")
    .style("font-size", "12px Arial")
    .text("a simple tooltip");

update(root);
d3.select(self.frameElement).style("height", "500px");
function update(source) {
  // Compute the new tree layout.
  //var nodes = tree.nodes(root).reverse(), // NOLEAF
  var nodes = tree.nodes(root).reverse().filter(function(d) {return root._root || d.children || d._children}),
	  links = tree.links(nodes);
  // Normalize for fixed-depth.
  //nodes.forEach(function(d) { d.y = d.depth * 180; }); // NOLEAF
  nodes.forEach(function(d) { root._root ? d.y = d.depth * 180 : d.y = d.depth * 220; });
  // Update the nodes…
  var node = svg.selectAll("g.node")
	  .data(nodes, function(d) { return d.id || (d.id = ++i); });
  // Enter any new nodes at the parent's previous position.
  var nodeEnter = node.enter().append("g")
	  .attr("class", "node")
	  .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
	  .on("click", click)
      .on("mouseover", function(d){return tooltip.style("visibility", "visible").text(d.percent);})
      .on("mousemove", function(d){return tooltip.style("top",
          (d3.event.pageY-10)+"px").style("left",(d3.event.pageX+10)+"px").text(d.percent);})
      .on("mouseout", function(){return tooltip.style("visibility", "hidden");});

  nodeEnter.append("circle")
	  .attr("r", 1e-6)
	  .style("fill", function(d) { return d._root ? "steelblue" : "#fff"; });
  nodeEnter.append("text")
	  .attr("x", function(d) { return d.children || d._children ? -13 : 13; })
	  .attr("dy", ".35em")
	  .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
	  .text(function(d) { return d.name; })
	  .style("fill-opacity", 1e-6);
  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
	  .duration(duration)
	  .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });
  nodeUpdate.select("circle")
	  .attr("r", 10)
      .style("stroke", function(d) { return d.color; }) // TEST
	  .style("fill", function(d) { return d._root ? "steelblue" : "#fff"; });
  nodeUpdate.select("text")
	  .style("fill-opacity", 1);
  // Transition exiting nodes to the parent's new position.
  var nodeExit = node.exit().transition()
	  .duration(duration)
	  .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
	  .remove();
  nodeExit.select("circle")
	  .attr("r", 1e-6);
  nodeExit.select("text")
	  .style("fill-opacity", 1e-6);
  // Update the links…
  var link = svg.selectAll("path.link")
	  .data(links, function(d) { return d.target.id; });
  // Enter any new links at the parent's previous position.
  link.enter().insert("path", "g")
	  .attr("class", "link")
      .style("stroke", function(d) { return d.target.color; })
	  .attr("d", function(d) {
		var o = {x: source.x0, y: source.y0};
		return diagonal({source: o, target: o});
	  });
  // Transition links to their new position.
  link.transition()
	  .duration(duration)
	  .attr("d", diagonal);
  // Transition exiting nodes to the parent's new position.
  link.exit().transition()
	  .duration(duration)
	  .attr("d", function(d) {
		var o = {x: source.x, y: source.y};
		return diagonal({source: o, target: o});
	  })
	  .remove();
  if (!root._root) { // NOLEAF
     link.filter(function(d) {return !(d.target.children || d.target._children)}).remove();
  }
  // Stash the old positions for transition.
  nodes.forEach(function(d) {
	d.x0 = d.x;
	d.y0 = d.y;
  });
}
// Toggle children on click.
function click(d) {
  if (d==root && d._root) {
    root = d._root;
    d._root = null;
  } else {
	d._root = root;
	root=d;
  }
  update(root);
}
</script>
	
  </body>
</html>
"""
    let body = jsonStringOfTree pseudo arbre
    use streamWriter = new StreamWriter(path, false)
    streamWriter.Write(intro + body + concl)