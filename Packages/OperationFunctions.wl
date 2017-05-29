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


GraphConvert::usage = $UsageString[
	"GraphConvert[`vertices, edges, length`] displays a three-dimensional graphics images of the roots."
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


(* ::Subsubsection:: *)
(*SelectEndPoints*)


SelectEndPoints[metaEdgePair_, "ID"] := 
	SelectVerticesWithDegree[metaEdgePair, 1, "ID"]


(* ::Subsubsection:: *)
(*getMeasure*)


getMeasure[id_]:= {thickness[[id]],width[[id]],length[[id]]}


(* ::Subsubsection:: *)
(*NextVertex*)


nextVertex[id_, edges_]:= 
	Select[Flatten[Select[edges, MemberQ[#, id]&]], # != id&]


(* ::Subsubsection:: *)
(*FindEdge *)


findEdge[id_, edges_]:= Flatten[Select[edges, MemberQ[#, id]&]]

findEdgeIndex[edge_, edges_]:= Select[edges, MemberQ[#, edge]&]


(* ::Subsubsection:: *)
(*Connect*)


connect[id1_, id2_, {edges_List, {thickness_List, width_List, length_List}}] := Module[
	{thickness1, thickness2, width1, width2, length1, length2, \[Epsilon] = 0.001, t, w, l, twoEdges, edgePositions,
	newThickness = thickness, newWidth = width, newLength = length, newEdges = edges},
	
	twoEdges = findEdge[#, edges] &/@ {id1, id2};
	edgePositions = Flatten[Position[edges, #] &/@ twoEdges];
	{{thickness1, width1, length1}, {thickness2, width2, length2}} = getMeasure/@ edgePositions;
	(*If[Abs[thickness-thickness2] < \[Epsilon] && Abs[width1 - width2] < \[Epsilon], $connect]*)
	{t, w, l} = {Mean[{thickness1, thickness2}], Mean[{width1, width2}], Mean[{length1, length2}]};
	(*Print[t, " ", w, " ", l];*)
	AppendTo[newEdges, {id1, id2}];
	AppendTo[newThickness, t];
	AppendTo[newWidth, w];
	AppendTo[newLength, l];
	
	{newEdges, {newThickness, newWidth, newLength}}
]


(* ::Subsection:: *)
(*Duplicate Edge*)


DuplicateEdge[index_, {vertices_, edges_, {thickness_, width_, length_}}]:= Block[
	{loopEdges, loopGraph, groupedVertices, groupedEdges, ids, 
	duplicatedIds, duplicatedEdges, duplicatedVertices, edgeId,
	newWidth, newThickness, newLength, newVertices, newEdges},
	
	loopEdges = ExtractInfiniteEdges[vertices, edges, length];
	loopGraph = MapThread[#1 <-> #2 &, Transpose @ loopEdges];
	groupedVertices = DisconnectGraph[loopGraph, "Vertices"];
	groupedEdges = DisconnectGraph[loopGraph, "Data"];
	ids = groupedVertices[[index]];
	(*Print[ids];*)
	duplicatedIds = Range @ Length[ids] + Length[vertices];
	(*Print[duplicatedIds];*)
	duplicatedEdges = Partition[duplicatedIds, 2, 1];
	(*Print[duplicatedEdges];*)
	
	edgeId = Flatten[Position[edges, #]&/@ GraphConvert[groupedEdges[[index]] ] ];
	(*Print[edgeId];*)
	duplicatedVertices = vertices[[ids]];
	(*Print[duplicatedVertices];*)
	newThickness = width[[edgeId]]/2;
	newWidth = width[[edgeId]]/2;
	newLength = length[[edgeId]];
	(*Print[newLength];*)
	newWidth = Join[width, newWidth];
	newThickness = Join[thickness, newThickness];
	newLength = Join[length, newLength];
	newVertices = Join[vertices, duplicatedVertices];
	newEdges = Join[edges, duplicatedEdges];
	
	{newVertices, newEdges, {newThickness, newWidth, newLength}}
	
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ OperationFunctions`Private`$PublicSymbols;


EndPackage[];
