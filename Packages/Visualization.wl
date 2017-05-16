(* ::Package:: *)

(* ::Title:: *)
(*Corn Roots Visualization*)


(* ::Section:: *)
(*To Do*)


(* ::Text:: *)
(*Write Manipulate function to visualize the loop. [Done]*)


(* ::Section:: *)
(*Private*)


(* ::Section:: *)
(*Visualization*)


(* ::Subsection:: *)
(*Graph3DLength*)


EdgeColoring[u_?NumericQ]/; 0 <= u <= 1 := Blend[{{0,Blue}, {1,Red}}, u] 


Graph3DLength[vts_, edges_, length_]:= Block[
	{edgeColors, lines, lineColors, graphicsComplex},
	edgeColors = EdgeColoring /@ Rescale[length];
	lineColors = Transpose[{edgeColors, Line /@ edges}];
	Graphics3D[GraphicsComplex[vts, lineColors], Boxed -> False]
]


(* ::Subsection:: *)
(*ExtractInfinitePart*)


ExtractInfinitePart[vertices_,edges_, length_, color_] := Block[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[#]]&/@ infPositions;
	Graphics3D[{color, GraphicsComplex[vertices, Line/@infEdges], Boxed -> False}]
]


ExtractInfiniteEdges[vertices_, edges_, length_] := Block[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[#]]&/@ infPositions;
	infEdges
]


(*ComputeBoundingBox[vertices_, infEdges_, offset_] := Block[
	
]*)


(* ::Subsection:: *)
(*InfinitePartManipulate*)


$rescalingParameter[{{xmin_,xmax_}, {ymin_,ymax_}, {zmin_,zmax_}}] := 
	{{xmin - 10, xmax + 10}, {ymin - 10, ymax + 10}, {zmin - 10, zmax + 10}}


InfinitePartManipulate[vertices_, edges_, length_] := Block[
	{infPositions, infEdges, verticesSubset, minMaxOfVertices, boundingBox},
	
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[#]] &/@ infPositions;
	verticesSubset = vertices[[#]] &/@ Union[Flatten[infEdges]];
	minMaxOfVertices = MinMax /@ Transpose[verticesSubset];
	boundingBox = $rescalingParameter[minMaxOfVertices];
	Manipulate[
		Show[
			Graphics3D[{
				Thick, Red, 
				GraphicsComplex[vertices, Line /@ infEdges[[1;;i+1]] ], Box -> False}
			],
			PlotRange -> boundingBox
		],
	{i,1,Length[infEdges]-1,1}]
]


(* ::Subsection:: *)
(*ShowIntersectionPointByIndex*)


ShowIntersectionPointByIndex[graphics_,index_, vertices_, loopEdges_]:= Block[
	{loopGraphVertices, id, vertex, midPt},
	loopGraphVertices = Union @ Flatten[loopEdges]; (* No need to sort *)
	id = loopGraphVertices[[index]];
	vertex = vertices[[id]];
	Show[graphics,
		Graphics3D[{
			Text[Style[id, Medium], vertex], 
			PointSize[Large], Red, 
			Point[vertex],
			Box -> False}
		]
	]
]


ShowIntersectionPointByIndex[graphics_,index_List, vertices_, loopEdges_]:= Block[
	{loopGraphVertices, id, vertex, midPt},
	loopGraphVertices = Union @ Flatten[loopEdges]; (* No need to sort *)
	id = loopGraphVertices[[#]]&/@index;
	vertex = vertices[[#]]&/@id;
	Show[graphics,
		Graphics3D[Flatten[{
			MapThread[Text[Style[#1, Medium], #2]&, {id,vertex}], 
			PointSize[Large], Red, 
			Point[vertex],
			Box -> False}]
		]
	]
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ CornRootsFileReader`Private`$PublicSymbols;


EndPackage[];
