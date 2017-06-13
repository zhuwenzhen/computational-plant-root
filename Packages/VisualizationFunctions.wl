(* ::Package:: *)

(* ::Title:: *)
(*Visualization Functions for Corn Root Study*)


(* ::Subtitle:: *)
(*Wenzhen Zhu*)


(* ::Subsubtitle:: *)
(*Start Date: 05/16/2017*)


(* ::Section:: *)
(*To Do*)


(* ::Text:: *)
(*Write Manipulate function to visualize the loop. [Done]*)
(*Implement options for several visualization functions*)


(* ::Section:: *)
(*Public*)


BeginPackage["VisualizationFunctions`"];


VisualizationFunctions`Private`$PublicSymbols = {
	Graph3DLength, 
	VisualizeRootGraphics3D,
	ExtractInfinitePart, 
	ExtractInfiniteEdges, 
	RescalingParameter,
	(*ShowIntersectionPointByIndex,*)
	ShowIntersectionPointByVertexPosition,
	ShowVerticesID,
	ColorMetaEdges3D,
	Rainbow
};


Unprotect /@ VisualizationFunctions`Private`$PublicSymbols;


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


ExtractInfinitePart::usage = $UsageString[
	"ExtractInfinitePart[`vertices, edges, length, color`] displays only the part of roots whose edges have infinite length."
];


ExtractInfiniteEdges::usage = $UsageString[
	"ExtractInfiniteEdges[`vertices, edges, length] gives a list of edges whose length is infinite."
];


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Graph3DLength*)


$EdgeColoring[u_?NumericQ]/; 0 <= u <= 1 := Blend[{{0,Blue}, {1,Red}}, u] 


Graph3DLength[vts_, edges_, length_]:= Module[
	{edgeColors, lines, lineColors, graphicsComplex},
	edgeColors = $EdgeColoring /@ Rescale[length];
	lineColors = Transpose[{edgeColors, Line /@ edges}];
	Graphics3D[GraphicsComplex[vts, lineColors], Boxed -> False]
]


(* ::Subsection:: *)
(*ExtractInfinitePart*)


ExtractInfinitePart[vertices_,edges_, length_, color_] := Module[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[infPositions]];
	Graphics3D[{color, GraphicsComplex[vertices, Line/@infEdges]}, Boxed -> False]
]


ExtractInfiniteEdges[edges_, length_] := Module[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[infPositions]];
	infEdges
]


(* ::Subsection:: *)
(*VisualizeRootGraphics3D*)


VisualizeRootGraphics3D[vertices_, edges_, color_] := 
	Graphics3D[
		{color, GraphicsComplex[vertices, Line/@edges]}, Boxed -> False]


(* ::Subsection:: *)
(*ShowVerticesID*)


ShowVerticesID[graphics_,id_, vertices_, edges_]:= Block[
	{vertex},
	vertex = vertices[[id]];
	Show[graphics, 
		Graphics3D[{
			Text[Style[id, Medium], vertex], 
			PointSize[Large], Red, 
			Point[vertex]},
			Boxed -> False
		]
	]	
]

ShowVerticesID[graphics_,id_List, vertices_, edges_]:= Block[
	{vertex},
	vertex = vertices[[#]]&/@id;
	Show[graphics, 
		Graphics3D[Flatten[{
			MapThread[Text[Style[#1, Medium], #2]&, {id,vertex}], 
			PointSize[Large], Red, 
			Point[vertex]}],
			Boxed -> False
		]
	]	
]


(* ::Subsection:: *)
(*ShowIntersectionPointByVertexPosition*)


ShowIntersectionPointByVertexPosition[graphics_,index_, vertices_, edges_]:= Block[
	{graphVtx, id, vertex},
	graphVtx = Sort @ Union @ Flatten[edges]; (* No need to sort *)
	id = graphVtx[[index]];
	vertex = vertices[[id]];
	Show[graphics,
		Graphics3D[{
			Text[Style[id, Medium], vertex], 
			PointSize[Large], Red, 
			Point[vertex]},
			Boxed -> False
		]
	]
]


ShowIntersectionPointByVertexPosition[graphics_,index_List, vertices_, loopEdges_]:= Block[
	{loopGraphVertices, id, vertex, midPt},
	loopGraphVertices = Union @ Flatten[loopEdges]; (* No need to sort *)
	id = loopGraphVertices[[#]]&/@index;
	vertex = vertices[[#]]&/@id;
	Show[graphics,
		Graphics3D[Flatten[{
			MapThread[Text[Style[#1, Medium], #2]&, {id,vertex}], 
			PointSize[Large], Red, 
			Point[vertex]}],
			Boxed -> False
		]
	]
]


(* ::Subsection:: *)
(*ColorMetaEdge3D*)


Rainbow[n_]:=ColorData["Rainbow"]/@Subdivide[n-1]

ColorMetaEdges3D[vertices_, groupedEdges_]:= Module[
	{len, edgeNum, c, colors, organized, metaEdgeColors},
	len = Length[groupedEdges];
	c = Rainbow[len];
	Print[c];
	edgeNum = Length/@groupedEdges ;
	colors = Table[Table[ c[[i]], {edgeNum[[i]]}], {i, 1, len}];
	organized = Transpose /@ (Transpose@{colors, groupedEdges/.UndirectedEdge -> List});
	metaEdgeColors = {#[[1]], Thick, Line[#[[2]]]} &/@ Flatten[organized, 1];
	Graphics3D[GraphicsComplex[vertices,metaEdgeColors], Boxed -> False]
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ VisualizationFunctions`Private`$PublicSymbols;


EndPackage[];
