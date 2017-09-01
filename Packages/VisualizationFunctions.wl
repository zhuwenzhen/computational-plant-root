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
	ExtractLargeWidthPart,
	ExtractLargeWidthEdges, 
	ExtractLargeThicknessPart,
	ExtractLargeThicknessEdges,
	RescalingParameter,
	(*ShowIntersectionPointByIndex,*)
	ShowIntersectionPointByVertexPosition,
	ShowVerticesID,
	VisualizeVerticesDegree,	
	HighlightGraphics3D,
	HighlightVertices,
	ColorMetaEdges3D,
	Rainbow,
	heatMap,
	HighlightEdge
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


VisualizeRootGraphics3D::usage = $UsageString[
	"VisualizeRootGraphics3D[`v`, `e`, `Color`] represents a three-dimentional graphical image of root.\n",
	"VisualizeRootGraphics3D[`v`, `e`, `width`, \"Rainbow\"] represents a three-dimentional graphical image of root and is colored by width measurement.",
	"VisualizeRootGraphics3D[`v`, `e`, `thickness`, \"Thickness\"] represents a three-dimentional graphical image of root and is colored by `thickness` measurement.",
	"VisualizeRootGraphics3D[`v`, `e`, `width`, \"Width\"] represents a three-dimentional graphical image of root and is colored by `width` measurement."
];


ExtractInfinitePart::usage = $UsageString[
	"ExtractInfinitePart[`vertices, edges, length, color`] displays only the part of root where edges have infinite length."
];


ExtractInfiniteEdges::usage = $UsageString[
	"ExtractInfiniteEdges[`vertices, edges, length`] gives a list of edges whose length is infinite."
];


ExtractLargeWidthPart::usage = $UsageString[
	"ExtractLargeWidthPart[`v`, `e`, `w`] displays only part of the root given vertices `v` and edges `e` where width `w` is large."
];


ExtractLargeWidthEdges::usage = $UsageString[
	"ExtractLargeWidthEdges[`e`, `w`] gives a list of edges `e` whose width `w` is large.\n",
	"ExtractLargeWidthEdges[`e`, `w`, \"Position\"] gives a list of edges `e` and positions {`edges`, `edgeID`} whose width `w` is large."
];


ExtractLargeThicknessPart::usage = $UsageString[
	"ExtractLargeThicknessPart[`v`, `e`, `t`] displays only part of the root given vertices `v` and edges `e` where thickness `t` is large."
];


ExtractLargeThicknessEdges::usage = $UsageString[
	"ExtractLargeWidthEdges[`e`, `t`] gives a list of edges `e` whose thickness `t` is large.\n",
	"ExtractLargeWidthEdges[`e`, `t`, \"Position\"] gives a list of edges `e` and positions {`edges`, `edgeID`} whose thickness `t` is large."
];


ShowVerticesID::usage = $UsageString[
	"ShowVerticesID[`graphics, id, vertices, edges`] represents a 3-D graphical image with highlighted vertices given veritices ID."
];


VisualizeVerticesDegree::usage = $UsageString[
	"VisualizeVerticesDegree[`graphics`, `id1`, `id2`, `v`, `e`] visualize vertices with degree 3 (in red) and degree 1 (in blue)."
];


HighlightGraphics3D::usage = $UsageString[
	"HighlightGraphics3D[`graphics`, `id`, `v`, `e`, `Color`] hightlight the 3D graphics by the given vertices id."
];


HighlightVertices::usage = $UsageString[
	"HighlightVertices[`graphics`, `id`, `v`, `e`, `Color`] hightlight the 3D graphics by the given vertices id."
];


(* ::Section:: *)
(*Implementation*)


(* ::Subsection::Closed:: *)
(*heatMap*)


heatMap[n_]:= Hue[0.7n]


(* ::Subsection::Closed:: *)
(*Graph3DLength*)


$EdgeColoring[u_?NumericQ]/; 0 <= u <= 1 := Blend[{{0,Blue}, {1,Red}}, u] 


Graph3DLength[vts_, edges_, length_]:= Module[
	{edgeColors, lines, lineColors, graphicsComplex},
	edgeColors = $EdgeColoring /@ Rescale[length];
	lineColors = Transpose[{edgeColors, Line /@ edges}];
	Graphics3D[GraphicsComplex[vts, lineColors], Boxed -> False]
]


(* ::Subsection::Closed:: *)
(*ExtractInfinitePart*)


ExtractInfinitePart[vertices_, edges_, length_, color_] := Module[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[infPositions]];
	Graphics3D[{color, GraphicsComplex[vertices, Line/@infEdges]}, Boxed -> False]
]


ExtractInfiniteEdges[edges_, length_] := Module[
	{infPositions},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	edges[[infPositions]]
]


(* ::Subsection::Closed:: *)
(*ExtractLargeWidthPart*)


ExtractLargeWidthPart[vertices_, edges_, width_]:= Module[
	{largeWidthPosition, largeWidthEdges, largeWidth, w, colors, edgeColors},
	
	largeWidthPosition = Flatten[Position[Round@Rescale[width], 1]];
	largeWidthEdges = edges[[largeWidthPosition]];
	largeWidth = width[[largeWidthPosition]];

	w = Rescale[largeWidth];
	(*colors = ColorData["Rainbow"]/@ w ;*)
	colors = heatMap/@(1 - w) ;
	edgeColors = Transpose[{colors, Line/@largeWidthEdges}];
	Graphics3D[GraphicsComplex[vertices, edgeColors], Boxed -> False]
]


ExtractLargeWidthEdges[edges_, width_]:= Module[
	{largeWidthPosition},
	largeWidthPosition = Flatten[Position[Round @ Rescale[width], 1]];
	edges[[largeWidthPosition]]
]


ExtractLargeWidthEdges[edges_, width_, "Position"]:= Module[
	{largeWidthPosition},
	largeWidthPosition = Flatten[Position[Round @ Rescale[width], 1]];
	{edges[[largeWidthPosition]], largeWidthPosition}
]


(* ::Subsection::Closed:: *)
(*ExtractLargeThicknessPart*)


ExtractLargeThicknessPart[vertices_, edges_, thickness_]:= Module[
	{largeThicknessPosition, largeThicknessEdges, largeThickness, t, colors, edgeColors},
	
	largeThicknessPosition = Flatten[Position[Round@Rescale[thickness], 1]];
	largeThicknessEdges = edges[[largeThicknessPosition]];
	largeThickness = thickness[[largeThicknessPosition]];

	t = Rescale[largeThickness];
	colors = heatMap/@(1 - t);
	(*colors = ColorData["Rainbow"]/@ t ;*)
	edgeColors = Transpose[{colors, Line/@largeThicknessEdges}];
	Graphics3D[GraphicsComplex[vertices, edgeColors], Boxed -> False]
]


ExtractLargeThicknessEdges[edges_, thickness_]:= Module[
	{largeThicknessPosition},
	largeThicknessPosition = Flatten[Position[Round @ Rescale[thickness], 1]];
	edges[[largeThicknessPosition]]
]


ExtractLargeThicknessEdges[edges_, thickness_, "Position"]:= Module[
	{largeThicknessPosition},
	largeThicknessPosition = Flatten[Position[Round @ Rescale[thickness], 1]];
	{edges[[largeThicknessPosition]], largeThicknessPosition}
]


(* ::Subsection:: *)
(*VisualizeRootGraphics3D*)


VisualizeRootGraphics3D[vertices_, edges_, color_] := 
	Graphics3D[
		{color, GraphicsComplex[vertices, Line/@edges]}, Boxed -> False]

VisualizeRootGraphics3D[vertices_, edges_, width_, "Width"]:= Module[
	{w, colors, edgeColors},
	w = Rescale[width];
	colors = heatMap/@(1 - w) ;
	edgeColors = Transpose[{colors, Line/@edges}];
	Graphics3D[GraphicsComplex[vertices, edgeColors], Boxed -> False]
]

VisualizeRootGraphics3D[vertices_, edges_, thickness_, "Thickness"]:= Module[
	{t, colors, edgeColors},
	t = Rescale[thickness];
	colors = heatMap/@(1 - t);
	edgeColors = Transpose[{colors, Line /@ edges}];
	Graphics3D[GraphicsComplex[vertices, edgeColors], Boxed -> False]
]
(*
VisualizeRootGraphics3D[vertices_, edges_, width_, "Wharl"]:= Module[
	{r, maxPos, p0},
	r = (Max @ width) /2*)


(* ::Subsection::Closed:: *)
(*ShowVerticesID*)


ShowVerticesID[graphics_,id_, vertices_, edges_]:= Block[
	{vertex},
	vertex = vertices[[id]];

	Show[graphics, 
		Graphics3D[{
			Text[Style[id, Medium], vertex], 
			PointSize[Medium], Red, 
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
			PointSize[Medium], Red, 
			Point[vertex]}],
			Boxed -> False
		]
	]	
]


(* ::Subsection::Closed:: *)
(*Highlight3DGraphics*)


HighlightGraphics3D[graphics_,id_List, vertices_, edges_, Color_]:= Block[
	{vertex},
	vertex = vertices[[#]]&/@id;
	Show[graphics, 
		Graphics3D[Flatten[{ 
			PointSize[Medium], Color, Point[vertex]}],
			Boxed -> False
		]
	]	
]


(* ::Subsection::Closed:: *)
(*HighlightVertices*)


HighlightVertices[graphics_,id_List, vertices_, edges_, Color_]:= Block[
	{vertex},
	vertex = vertices[[#]]&/@id;
	Show[graphics, 
		Graphics3D[Flatten[{ 
			PointSize[0.01], Color, Point[vertex]}],
			Boxed -> False
		]
	]	
]


(* ::Subsection:: *)
(*VisualizeVerticesDegree*)


VisualizeVerticesDegree[graphics_, deg1_List, deg3_List, deg4_List, vertices_]:= Block[
	{v1, v3, v4},
	v1 = vertices[[#]]&/@deg1;
	v3 = vertices[[#]]&/@deg3;
	v4 = vertices[[#]]&/@deg4;
	Show[graphics, 
		Graphics3D[Flatten[{
			MapThread[Text[Style[#1, Medium], #2]&, {Join[deg1, deg3, deg4], Join[v1,v3,v4]}], 
			PointSize[Medium], Red, Point[v3],
			PointSize[Medium], Green, Point[v1],
			PointSize[0.01], Orange, Point[v4]}],
			Boxed -> False
		]
	]	
]

VisualizeVerticesDegree[graphics_, deg1_List, deg3_List, deg4_List, vertices_, "No Label"]:= Block[
	{v1, v3, v4},
	v1 = vertices[[#]]&/@deg1;
	v3 = vertices[[#]]&/@deg3;
	v4 = vertices[[#]]&/@deg4;

	Show[graphics, 
		Graphics3D[Flatten[{ 
			PointSize[Medium], Orange, Point[v4],
			PointSize[Medium], Red, Point[v3],
			PointSize[Medium], Green, Point[v1]}],
			Boxed -> False
		]
	]	
]

VisualizeVerticesDegree[graphics_, deg1_List, deg3_List, vertices_, edges_]:= Block[
	{v1, v3},
	v1 = vertices[[#]]&/@deg1;
	v3 = vertices[[#]]&/@deg3;

	Show[graphics, 
		Graphics3D[Flatten[{
			MapThread[Text[Style[#1, Medium], #2]&, {Join[deg1, deg3], Join[v1,v3]}], 
			PointSize[Medium], Red, Point[v3],
			PointSize[Medium], Green, Point[v1]}],
			Boxed -> False
		]
	]	
]


(* ::Subsection:: *)
(*ShowNewEdge*)


HighlightEdge[graphics_, newEdge_List, v_]:= 
	Show[graphics, Graphics3D[{Thick, Red, GraphicsComplex[v,Line[newEdge]]}]]	


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
	Graphics3D[GraphicsComplex[vertices, metaEdgeColors], Boxed -> False]
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ VisualizationFunctions`Private`$PublicSymbols;


EndPackage[];
