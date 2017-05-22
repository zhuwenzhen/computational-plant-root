(* ::Package:: *)

(* ::Title:: *)
(*Operation Functions for Corn Root Study*)


(* ::Subtitle:: *)
(*Wenzhen Zhu*)


(* ::Subsubtitle:: *)
(*Date: 05/22/2017*)


(* ::Section:: *)
(*To Do*)


(* ::Section:: *)
(*Public*)


BeginPackage["OperationFunctions`"];


OperationFunctions`Private`$PublicSymbols = {
	GraphConvert,
	FindVertexDegree3,
	FindVertexDegree3Position,
	DisconnectGraph,
	ConnectedEdges,
	ManipulateMetaEdge,
	DeleteEdge
};


Unprotect /@ OperationFunctions`Private`$PublicSymbols;


(* ::Section:: *)
(*Usage*)


$ArgStyle[arg_Integer] := "TR";
$ArgStyle["\[Ellipsis]"] := "TR";
$ArgStyle[str_String] := "TI";


$ArgString[arg_] :=
	"\!\(\*StyleBox[\"" <> ToString[arg] <> "\", \"" <> $ArgStyle[arg] <> "\"]\)"


$UsageString[str__] :=
	(StringTemplate[StringJoin[{str}]] /. {TemplateSlot[s_] :> $ArgString[s]})[]


Graph3DLength::usage = $UsageString[
	"Graph3DLength[`vertices, edges, length`] displays a three-dimensional graphics images of the roots."
];


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Subsection:: *)
(*GraphConvert*)


GraphConvert[a_ <-> b_] := {a, b}
GraphConvert[list_List] := GraphConvert/@list


(* ::Subsection:: *)
(*FindVertexDegree3Position*)


FindVertexDegree3Position[graphData_List]:= Module[
	{g, vertices, vertexDegree},
	g = Graph[graphData];
	vertices = VertexList[g];
	vertexDegree = VertexDegree[g, #]&/@ vertices;
	Flatten @ Position[vertexDegree, 3]
]


(* ::Subsection:: *)
(*FindVertexDegree3*)


FindVertexDegree3[graphData_List] := Module[
	{vertices, vd3Position},
	vertices = VertexList[Graph[graphData]];
	vd3Position = FindVertexDegree3Position[graphData];
	vertices[[vd3Position]]
]


(* ::Subsection:: *)
(*DisconnectGraph - Return graph*)


DisconnectGraph[graphData_, "Graph"]:= Module[
	{g, vertices, vertexDegree, vd3Position, vd3Vertex},
	g = Graph[graphData];
	vertices = VertexList[g]; 
	vd3Position = FindVertexDegree3Position[graphData];
	vd3Vertex = vertices[[vd3Position]];
	VertexDelete[graphData,vd3Vertex]
]


(* ::Subsection:: *)
(*DisconnectGraph - Return data*)


DisconnectGraph[graphData_, "Data"]:= Module[
	{g, vertices, vd3Position, degree3Vtx, triEdgePosition, 
	metaEdgesVertices, metaEdges, edges},
	g = Graph[graphData];
	vertices = VertexList[g]; 
	vd3Position = FindVertexDegree3Position[graphData];
	degree3Vtx = FindVertexDegree3[graphData];
	edges = graphConvert[graphData];
	triEdgePosition = First @ Transpose[Flatten[Position[edges, #] &/@ degree3Vtx, 1]];
	metaEdgesVertices = Delete[loopGraphVertices, vd3Position];
	metaEdges = Delete[edges, {#} &/@ triEdgePosition];
	MapThread[#1 <-> #2 &, Transpose @ metaEdges]
]


(* ::Subsection:: *)
(*ConnectedEdges*)


ConnectedEdges[metaGraphData_List] := Module[
	{groupedVertices},
	groupedVertices = ConnectedComponents[metaGraphData];
	Table[
		Flatten[
			Cases[metaGraphData, # <-> _] &/@ groupedVertices[[i]]
		],
		{i,1,Length[groupedVertices]}
	]
]


(* ::Subsection:: *)
(*ManipulateMetaEdge*)


ManipulateMetaEdge[graph_, groupedVertices_] :=
	Manipulate[
		HighlightGraph[graph, Style[groupedVertices[[i]],Orange]],
		{i, 1, Length @ groupedVertices, 1}
	]


ManipulateMetaEdge[graphData_List, groupedVertices_] :=
	Manipulate[
		Graph[graphData, GraphHighlight -> groupedEdges[[i]]],
		{i, 1, Length@groupedEdges, 1} 
	]


(* ::Subsection:: *)
(*DeleteEdge*)


deleteEdge[completeGraph_, metaEdge_] := Block[
	{newGraph, res, cond},
	newGraph = Complement[completeGraph, metaEdge];
	cond = ConnectedGraphQ[Graph @ newGraph];
	Print[cond];
	Print["new graph: ", Length[newGraph]];
	Print["complete graph: ", Length[completeGraph]];
	If[cond, 
		res = newGraph,
		res = completeGraph
	];
	res
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ OperationFunctions`Private`$PublicSymbols;


EndPackage[];
