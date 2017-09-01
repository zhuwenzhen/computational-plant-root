(* ::Package:: *)

(* ::Title:: *)
(*Operation Functions for Plant Root Study*)


(* ::Subtitle:: *)
(*Wenzhen Zhu*)


(* ::Subsubtitle:: *)
(*Date: 05/22/2017*)


(* ::Section:: *)
(*To Do*)


(* ::Text:: *)
(*Fix ConnectEdge*)


(* ::Section:: *)
(*Public*)


BeginPackage["OperationFunctions`"];


OperationFunctions`Private`$PublicSymbols = {
	ConvertEdgeToList,
	ConvertListToEdge,
	FindVertexDegree,
	FindVertexDegreePosition,
	FindVertexDegreeN,
	DisconnectGraph,
	ConnectedEdges,
	ManipulateMetaEdge,
	DeleteEdge,
	SelectVerticesWithDegree,
	SelectDisconnectedPart,
	SelectEndPoints,
	getMeasure,
	nextVertex,
	findEdge,
	findEdgeIndex,
	iConnectEdge,
	ConnectEdge,
	FindConnectionVerticesID,
	SortGraph,
	DuplicateEdgeWithIndex, (*need to switch name with the more general one *)
	DuplicateEdge,
	ExtractEdge
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


ConvertEdgeToList::usage = $UsageString[
	"ConvertEdgeToList[`a \[UndirectedEdge] b`] convert graph back to list representation {a,b}."
];


FindVertexDegreePosition::usage = $UsageString[
	"FindVertexDegree3Position[graphData_List] give the vertices' position in a graph where the vertice's degree = 3"
];


FindVertexDegreeN::usage = $UsageString[
	"FindVertexDegreeN[graphData_List, n_] give the vertices whose degree = n."
];


DisconnectGraph::usage = $UsageString[
	"DisconnectGraph[`graphData`, \"Graph\"] breaks graph into meta edges.",
	"DisconnectGraph[`graphData`, \"MetagraphData\"] breaks graph into meta edges and return the broken graph's data."
];


nextVertex::usage = $UsageString[
	"nextVertex[`id`, `edges`] finds the next vertex of a given vertex `id` from `edges`."
]


DeleteEdge::usage = $UsageString[
	"DeleteEdge[`edges`, {`id1`, `id2`}, {`thickness`, `width`, `length`}] delete edge {`id1`, `id2`}, from `edges`."
]


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*GraphConvert*)


ConvertEdgeToList[a_ <-> b_] := {a, b}
ConvertEdgeToList[list_List] := ConvertEdgeToList/@list

ConvertListToEdge[{a_, b_}]:= a <-> b;
ConvertListToEdge[list_List]:= ConvertListToEdge/@list



(* ::Subsection::Closed:: *)
(*FindVertexDegreePosition*)


FindVertexDegreePosition[graphData_List, n_]:= Module[
	{g, vertices, vertexDegree},
	g = Graph[graphData];
	vertices = Sort @ VertexList[g];
	vertexDegree = VertexDegree[g, #]&/@ vertices;
	Flatten @ Position[vertexDegree, n]
]


(* ::Subsection::Closed:: *)
(*FindVertexDegreeN*)


FindVertexDegreeN[graphData_List, n_] := Module[
	{vertices, vd3Position},
	vertices = Sort @ VertexList[Graph[graphData]];
	vd3Position = FindVertexDegreePosition[graphData, n];
	vertices[[vd3Position]]
]


(* ::Subsection::Closed:: *)
(*DisconnectGraph - Return graph*)


DisconnectGraph[graphData_, "Graph"]:= Module[
	{g, vertices, vertexDegree, vd3Position, vd3Vertex},
	g = Graph[graphData];
	vertices = Sort @ VertexList[g]; 
	vd3Position = FindVertexDegreePosition[graphData, 3];
	vd3Vertex = vertices[[vd3Position]];
	VertexDelete[graphData,vd3Vertex]
]


(* ::Subsection::Closed:: *)
(*DisconnectGraph - Return data*)


DisconnectGraph[graphData_, "MetagraphData"]:= Module[
	{g, vertices, degree3Vtx, vd, vd3Position, triEdgePosition, 
	metaEdgesVertices, metaEdges, edges, metaGraphData, groupedVertices},
	g = Graph[graphData];
	vertices = Sort @ VertexList[g];
	vd = VertexDegree[g, #]&/@vertices;
	vd3Position = Flatten @ Position[vd, 3];
	degree3Vtx = vertices[[vd3Position]];
	
	edges = ConvertEdgeToList[graphData];
	triEdgePosition = First @ Transpose[Flatten[Position[edges, #] &/@ degree3Vtx, 1]];
	metaEdgesVertices = Delete[vertices, Position[vd, 3]];
	
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


(* ::Subsection::Closed:: *)
(*DisconnectGraph - Return Vertices*)


DisconnectGraph[graphData_, "Vertices"]:= Module[
	{metaGraphData},
	metaGraphData = DisconnectGraph[graphData, "MetagraphData"];
	ConnectedComponents[metaGraphData]
]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


DeleteEdge[edges_, metaEdge_, {thickness_, width_, length_}] := Block[
	{deletePosition, newEdges, newThickness, newWidth, newLength},
	(*newEdges = Complement[edges, metaEdge];*)
	deletePosition = Flatten[Position[edges, #]&/@ metaEdge, 1];
	newEdges = Delete[edges, deletePosition];
	newThickness = Delete[thickness, deletePosition];
	newWidth = Delete[width, deletePosition];
	newLength = Delete[length, deletePosition];
	{newEdges,{newThickness, newWidth, newLength}}
]


DeleteEdge[edges_, {id1_, id2_}, {thickness_, width_, length_}] := Block[
	{metaEdge, deletePosition, newEdges, newThickness, newWidth, newLength},
	metaEdge = Sort /@ Partition[ExtractEdge[id1, id2, edges], 2, 1];
	deletePosition = Flatten[Position[edges, #]&/@ metaEdge, 1];
	newEdges = Delete[edges, deletePosition];
	newThickness = Delete[thickness, deletePosition];
	newWidth = Delete[width, deletePosition];
	newLength = Delete[length, deletePosition];
	{newEdges,{newThickness, newWidth, newLength}}
]


DeleteEdge[edges_, e_List, {thickness_, width_, length_}] := Block[
	{deletePosition, newEdges, newThickness, newWidth, newLength},
	deletePosition = Flatten[Position[edges, #]&/@ e, 1];
	newEdges = Delete[edges, deletePosition];
	newThickness = Delete[thickness, deletePosition];
	newWidth = Delete[width, deletePosition];
	newLength = Delete[length, deletePosition];
	{newEdges,{newThickness, newWidth, newLength}}
]


(* ::Subsection:: *)
(*ConnectEdge*)


(* ::Subsubsection::Closed:: *)
(*Select Vertices With Degree == n*)


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


(* ::Subsubsection::Closed:: *)
(*Select Disconnected Part*)


SelectDisconnectedPart[edges_, "Edge Position"] := Module[
	{graphData, graph},
	graphData = MapThread[#1 <-> #2 &, Transpose @ edges];
	graph = Graph[graphData];
	Last @ ConnectedComponents[graph] (* might need to change to "Select" the smaller part *)
]
SelectDisconnectedPart[edges_, "ID"] := Module[
	{graphData, graph, graphVertices, vertexDegreeList, edgePositions} ,
	graphData = MapThread[#1 <-> #2 &, Transpose @ edges];
	graph = Graph[graphData];
	graphVertices = Sort @ VertexList[graph];
	edgePositions = Last @ ConnectedComponents[graph];
	graphVertices[[edgePositions]]
]
SelectDisconnectedPart[edges_, "MetaEdge"] := Module[
	{graphData, graph, graphVertices, vertexDegreeList, verticesSubset},
	graphData = MapThread[#1 <-> #2 &, Transpose @ edges];
	verticesSubset = SelectDisconnectedPart[edges, "ID"];
	Flatten[ Cases[graphData, # <-> _] &/@ verticesSubset ]
]

SelectDisconnectedPart[edges_, "PairMetaEdge"] := Block[
	{metaEdge},
	metaEdge = SelectDisconnectedPart[edges, "MetaEdge"];
	ConvertEdgeToList @ metaEdge
]	


(* ::Subsubsection::Closed:: *)
(*SelectEndPoints*)


SelectEndPoints[metaEdgePair_, "ID"] := 
	SelectVerticesWithDegree[metaEdgePair, 1, "ID"]


(* ::Subsubsection::Closed:: *)
(*getMeasure*)


getMeasure[id_, thickness_, width_, length_]:= {thickness[[id]],width[[id]],length[[id]]}


(* ::Subsubsection::Closed:: *)
(*NextVertex*)


nextVertex[id_, edges_]:= 
	Select[Flatten[Select[edges, MemberQ[#, id]&]], # != id&]


(* ::Subsubsection:: *)
(*FindEdge *)


findEdge[id_, edges_]:= Flatten[Select[edges, MemberQ[#, id]&]]
findEdgeIndex[edge_, edges_]:= Select[edges, MemberQ[#, edge]&]


(* ::Subsubsection::Closed:: *)
(*FindConnectionVerticesID*)


FindConnectionVerticesID[partEdges_, vertices_, edges_] := Block[
	{partEndPtsID, allEndPts, partEndPts, distList, distMin, minPosition,
	minPts, minDistance, listPosition, minPtsID, id1, id2},
	(* 1. Find end points from the isolated part *)
	partEndPtsID = SelectEndPoints[partEdges, "ID"];
	partEndPts=vertices[[partEndPtsID]];
	(* 2. Find all end points from the whole root *)
	allEndPts = SelectVerticesWithDegree[vertices, edges, 1, "Vertices"];
	(*Print[allEndPts];*)
	(* 3. Compute the minimal distance *)
	distList=N@Outer[EuclideanDistance, partEndPts,allEndPts,1];
	distMin = Last @ Transpose[TakeSmallest[#, 2] &/@ distList];
	
	(* pick the minimum from the two *)
	{id1, minDistance} = Flatten @ MinimalBy[Transpose[{partEndPtsID, distMin}], Last];
	listPosition = First @ First @ Position[distMin, minDistance];
	(* 4. Find the min distance position *)
	minPosition = Flatten[Position[distList[[listPosition]], minDistance]];
	(* 5. Find the min distance points *)
	minPts = allEndPts[[minPosition]];

	id2 = First@Flatten[Position[vertices,#]&/@minPts];
	
	{id1, id2}
	
]


(* ::Subsubsection:: *)
(*Connect*)


iConnectEdge[{id1_, id2_}, {edges_List, {thickness_List, width_List, length_List}}] := Module[
	{thickness1, thickness2, width1, width2, length1, length2, t, w, l, twoEdges, edgePositions,
	newThickness = thickness, newWidth = width, newLength = length, newEdges = edges},
	
	twoEdges = findEdge[#, edges] &/@ Sort@{id1, id2};
	If[Length/@twoEdges == {2, 2}, 
		twoEdges = Sort/@ twoEdges, 
		If[(Length/@twoEdges)[[1]] == 2, 
			twoEdges = Sort/@ Join[{twoEdges[[1]], twoEdges[[2, 1;;2]]}],
			twoEdges = Sort/@Join[{twoEdges[[1,1;;2]], twoEdges[[2,1;;2]]}]
		]
	];
	
	edgePositions = Flatten[Position[edges, #] &/@ (Sort/@twoEdges)];
	
	{{thickness1, width1, length1}, {thickness2, width2, length2}} = getMeasure[#, thickness, width, length]&/@ edgePositions;
	
	{t, w, l} = {Mean[{thickness1, thickness2}], Mean[{width1, width2}], Mean[{length1, length2}]};
	
	AppendTo[newEdges, Sort @{id1, id2}];
	AppendTo[newThickness, t];
	AppendTo[newWidth, w];
	AppendTo[newLength, l];
	
	{newEdges, {newThickness, newWidth, newLength}}
]


ConnectEdge[newEdges_List, {edges_List,  {thickness_List, width_List, length_List}}]:= Block[
	{dummyNewEdges, dummyNewThickness, dummyNewWidth, dummyNewLength},
	{dummyNewEdges, dummyNewThickness, dummyNewWidth, dummyNewLength} = {edges, thickness, width, length};
	Do[
	{dummyNewEdges, {dummyNewThickness, dummyNewWidth, dummyNewLength}} = 
		iConnectEdge[Sort@newEdges[[i]], {dummyNewEdges, {dummyNewThickness,  dummyNewWidth, dummyNewLength}}],
	{i, Length[newEdges]}]
]


(* ::Subsection::Closed:: *)
(*DuplicateEdge*)


(* ::Subsubsection:: *)
(*SortGraph*)


SortGraph[graph_List, v_]:= Block[
	{edges, nextV, visited = {v}},
	edges = graph /. UndirectedEdge -> List;
	nextV = First @ nextVertex[v, edges];
	AppendTo[visited, nextV];
	Do[
		nextV = First@ Select[nextVertex[nextV, edges], !MemberQ[visited, #]&];
		AppendTo[visited, nextV],
		Length[graph] - 1
	];
	visited
]
SortGraph[edges_, v_]:= Block[
	{ nextV, visited = {v}},
	nextV = First @ nextVertex[v, edges];
	AppendTo[visited, nextV];
	Do[
		nextV = First@ Select[nextVertex[nextV, edges],!MemberQ[visited,#]&];
		AppendTo[visited, nextV],
		Length[edges] - 1
	];
	visited
]


(* ::Subsubsection:: *)
(*DuplicateEdge*)


$extractInfiniteEdges[edges_, length_] := Module[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[infPositions]];
	infEdges
]


DuplicateEdgeWithIndex[index_, {vertices_, edges_, {thickness_, width_, length_}}]:= Module[
	{loopEdges, loopGraph, groupedVertices, groupedEdges, ids, 
	duplicatedIds, duplicatedEdges, duplicatedVertices, edgeId,
	newWidth, newThickness, newLength, newVertices, newEdges, 
	edgeVertex1, edgeVertex2, end1, end2, metaEdgeVertices, vtxA, vtxB, 
	duplicatedEdgesCopy},
	
	(* Set up *)
	loopEdges = $extractInfiniteEdges[edges, length];
	loopGraph = MapThread[#1 <-> #2 &, Transpose @ loopEdges];
	groupedVertices = DisconnectGraph[loopGraph, "Vertices"];
	groupedEdges = DisconnectGraph[loopGraph, "Data"];
	
	(* Find the end points by picking vetex whose degree = 1 *)
	{end1, end2} = SelectVerticesWithDegree[
						groupedEdges[[index]]/.UndirectedEdge -> List, 1, "ID"];
	
	(* find edge vertex & vtx A, B *)
	metaEdgeVertices = groupedVertices[[index]]; 
	edgeVertex1 = First @ Select[nextVertex[end1, edges], !MemberQ[metaEdgeVertices, #]&];
	edgeVertex2 = First @ Select[nextVertex[end2, edges], !MemberQ[metaEdgeVertices, #]&];
	
	vtxA = First@Select[nextVertex[edgeVertex1, edges], # != end1&];
	vtxB = First@Select[nextVertex[edgeVertex2, edges], # != end2&];
	
	duplicatedEdges = Partition[SortGraph[groupedEdges[[index]], end1], 2, 1];

	duplicatedEdges = Join[{{edgeVertex1, duplicatedEdges[[1,1]]}},
							duplicatedEdges,
						   {{Last@Last@duplicatedEdges, edgeVertex2}}];

	duplicatedEdgesCopy = Join[duplicatedEdges, Reverse/@duplicatedEdges];
				   
						   
	(* ------------- Duplicate Vertices ------------- *)
	ids = Join[ {edgeVertex1}, SortGraph[groupedEdges[[index]], end1], {edgeVertex2}];
	(* duplicated vertex IDs *)
	duplicatedIds = Range @ Length[ids] + Length[vertices];	
	duplicatedVertices = vertices[[ids]];
	newVertices = Join[vertices, duplicatedVertices];
	
	(* ------------- Duplicate and Update Edges ------------- *)
	newEdges = Partition[duplicatedIds, 2, 1];
	newEdges = Join[edges, newEdges];
	newEdges = newEdges/.{
				{edgeVertex1, vtxA}->{First@duplicatedIds, vtxA},
				{vtxA, edgeVertex1}->{First@duplicatedIds, vtxA},
				{edgeVertex2, vtxB}->{Last@duplicatedIds, vtxB},
				{vtxB, edgeVertex2}->{Last@duplicatedIds, vtxB}
			};   
	(* now we have the whole edge we can use to duplicate *)
	edgeId = Flatten[Position[edges, #]&/@ duplicatedEdgesCopy];
	(* We need edge's id to assign the measurements values*)
	
	(* ------------- Duplicate Measurements ------------- *)
	newThickness = thickness[[edgeId]]/2;
	newWidth = width[[edgeId]]/2;
	newLength = length[[edgeId]];
	
	newWidth = Join[width, newWidth];
	newThickness = Join[thickness, newThickness];
	newLength = Join[length, newLength];
	
	{newVertices, newEdges, {newThickness, newWidth, newLength}}
]


(* ::Subsubsection:: *)
(*Duplicate*)


(* ::Text:: *)
(*e: {Subscript[v, 1], ..., Subscript[v, n]}, assume it's already sorted / sort it again anyway*)
(*dup: is abbr. for duplication*)


DuplicateEdge[e1_, e2_, e3_, e4_, e_, vertices_, edges_, {thickness_, width_, length_}]:= Block[
	{
		v1, v2, v3, v4, vA, vB,
		vtxDupID, vtxDup, newVtxID, edgeID, edgeDup, edgeDupCopy,
		newVtx,newEdges, newThickness, newWidth, newLength	
	},
	(*---------- ----------*)
	
	vA = First @ Intersection[e1, e2];
	vB = First @ Intersection[e3, e4];
	
	v1 = First @ Complement[e1, {vA}];
	v2 = First @ Complement[e2, {vA}];
	v3 = First @ Complement[e3, {vB}];
	v4 = First @ Complement[e4, {vB}];
	
	(*---------- Find Vertices needs to be duplicated ----------*)
	vtxDupID = Union @ Flatten[e];
	vtxDup = vertices[[vtxDupID]];
	newVtx = Join[vertices, vtxDup];
	
	(*---------- Construct new edges ----------*)
	newVtxID = Range[Length[e]+1] + Length[vertices]; (*if e doesn't including endpoints, +2 *)
	newEdges = Partition[newVtxID, 2, 1];
	edgeDup = Partition[SortGraph[e, vA], 2, 1];
	edgeDupCopy = Join[edgeDup, Reverse/@ edgeDup];
	newEdges = Join[edges, newEdges];

	newEdges = newEdges/. {{v2,vA}->{First@newVtxID, v2},
						   {vA,v2}->{First@newVtxID, v2},
						   {v4,vB}->{Last@newVtxID, v4},
						   {vB,v4}->{Last@newVtxID, v4} };
	
	edgeID = Flatten[Position[edges, #]&/@ edgeDupCopy];
	
	newThickness = thickness[[edgeID]]/2;
	newWidth = width[[edgeID]]/2;
	newLength = length[[edgeID]];
	
	newWidth = Join[width, newWidth];
	newThickness = Join[thickness, newThickness];
	newLength = Join[length, newLength];
	
	{newVtx, newEdges, {newThickness, newWidth, newLength}}
	
]


(* ::Subsection::Closed:: *)
(*Extract Edge*)


ExtractEdge[v1_, v2_, edges_]:= Module[
	{g, spf},
	g = ConvertListToEdge[edges];
	spf = FindShortestPath[g, v1, All];
	spf[v2]
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ OperationFunctions`Private`$PublicSymbols;


EndPackage[];
