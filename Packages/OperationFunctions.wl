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


Begin["`Private`"];


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


(* ::Subsection:: *)
(*GraphConvert*)


GraphConvert[a_ <-> b_] := {a, b}
GraphConvert[list_List] := GraphConvert/@list


(* ::Subsection:: *)
(*FindVertexDegree3Position*)


FindVertexDegree3Position[graphData_List]:= Module[
	{g, vertices, vertexDegree},
	g = Graph[graphData];
	vertices = Sort @ VertexList[g];
	vertexDegree = VertexDegree[g, #]&/@ vertices;
	Flatten @ Position[vertexDegree, 3]
]


(* ::Subsection:: *)
(*FindVertexDegree3*)


FindVertexDegree3[graphData_List] := Module[
	{vertices, vd3Position},
	vertices = Sort @ VertexList[Graph[graphData]];
	vd3Position = FindVertexDegree3Position[graphData];
	vertices[[vd3Position]]
]


(* ::Subsection:: *)
(*DisconnectGraph - Return graph*)


DisconnectGraph[graphData_, "Graph"]:= Module[
	{g, vertices, vertexDegree, vd3Position, vd3Vertex},
	g = Graph[graphData];
	vertices = Sort @ VertexList[g]; 
	vd3Position = FindVertexDegree3Position[graphData];
	vd3Vertex = vertices[[vd3Position]];
	VertexDelete[graphData,vd3Vertex]
]


(* ::Subsection:: *)
(*DisconnectGraph - Return data*)


DisconnectGraph[graphData_, "MetagraphData"]:= Module[
	{g, vertices, degree3Vtx, vd, vd3Position, triEdgePosition, 
	metaEdgesVertices, metaEdges, edges, metaGraphData, groupedVertices},
	g = Graph[graphData];
	vertices = Sort @ VertexList[g];
	vd = VertexDegree[g, #]&/@vertices;
	vd3Position = Flatten @ Position[vd, 3];
	degree3Vtx = vertices[[vd3Position]];
	
	edges = GraphConvert[graphData];
	triEdgePosition = First @ Transpose[Flatten[Position[edges, #] &/@ degree3Vtx, 1]];
	metaEdgesVertices = Delete[vertices, Position[vd,3]];
	
	metaEdges = Delete[edges, {#} &/@ triEdgePosition];
	metaGraphData = MapThread[#1 <-> #2 &, Transpose @ metaEdges]
]


DisconnectGraph[graphData_, "Data"]:= Module[
	{metaGraphData, groupedVertices},
	metaGraphData = DisconnectGraph[graphData, "MetagraphData"];
	groupedVertices = ConnectedComponents[metaGraphData];
	
	Table[
		Flatten[
			Cases[metaGraphData, # <-> _] &/@ groupedVertices[[i]]
		],
		{i,1,Length[groupedVertices]}
	]
]


(* ::Subsection:: *)
(*DisconnectGraph - Return Vertices*)


DisconnectGraph[graphData_, "Vertices"]:= Module[
	{metaGraphData},
	metaGraphData = DisconnectGraph[graphData, "MetagraphData"];
	ConnectedComponents[metaGraphData]
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


ManipulateMetaEdge[graphData_List, groupedEdges_] :=
	Manipulate[
		Graph[graphData, GraphHighlight -> groupedEdges[[i]]],
		{i, 1, Length@groupedEdges, 1} 
	]


(* ::Subsection:: *)
(*DeleteEdge*)


DeleteEdge[completeGraph_, metaEdge_] := Block[
	{newGraph, res, cond},
	newGraph = Complement[completeGraph, metaEdge];
	cond = ConnectedGraphQ[Graph @ newGraph];
	(*Print[cond];
	Print["new graph: ", Length[newGraph]];
	Print["complete graph: ", Length[completeGraph]];*)
	If[cond, 
		res = newGraph,
		res = completeGraph
	];
	res
]


(* ::Subsection:: *)
(*Connect*)


(* ::Subsubsection:: *)
(*Select Vertices With Degree*)


SelectVerticesWithDegree[edges_, degreeNumber_, "Edge Position"] := Module[
	{graphData, g, graphVertices, vertexDegreeList, position},
	graphData = MapThread[#1 <-> #2 &, Transpose @ edges];
	g = Graph[graphData];
	graphVertices = Sort @ VertexList[graphData];
	vertexDegreeList = VertexDegree[g, #] &/@ graphVertices;
	Flatten[Position[vertexDegreeList, degreeNumber]]
]


SelectVerticesWithDegree[edges_, degreeNumber_, "ID"] := Module[
	{graphData, g, graphVertices, vertexDegreeList, position},
	graphData = MapThread[#1 <-> #2 &, Transpose @ edges];
	g = Graph[graphData];
	graphVertices = Sort @ VertexList[graphData];
	vertexDegreeList = VertexDegree[g, #] &/@ graphVertices;
	position = Flatten[Position[vertexDegreeList, degreeNumber]];
	graphVertices[[position]]
]


SelectVerticesWithDegree[vertices_, edges_, degreeNumber_, "Vertices"]:= Block[
	{verticesID},
	verticesID = SelectVerticesWithDegree[edges, degreeNumber, "ID"];
	vertices [[verticesID]]
]


(* ::Subsubsection:: *)
(*Select Disconnected Part*)


SelectDisconnectedPart[edges_, "MetaEdge"] := Module[
	{graphData, graph, graphVertices, vertexDegreeList, verticesSubset},
	graphData = MapThread[#1 <-> #2 &, Transpose @ edges];
	verticesSubset = SelectDisconnectedPart[edges, "ID"];
	Flatten[ Cases[graphData, # <-> _] &/@ verticesSubset ]
]

SelectDisconnectedPart[edges_, "PairMetaEdge"] := Block[
	{metaEdge},
	metaEdge = SelectDisconnectedPart[edges, "MetaEdge"];
	GraphConvert @ metaEdge
]	


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ OperationFunctions`Private`$PublicSymbols;


EndPackage[];
