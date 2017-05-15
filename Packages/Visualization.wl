(* ::Package:: *)

(* ::Title:: *)
(*Corn Roots Visualization*)


(* ::Section:: *)
(*To Do*)


(* ::Text:: *)
(*Write Manipulate function to visualize the loop. [Done]*)


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Yajie's Code*)


jet[u_?NumericQ] :=
	Blend[{
		{0,RGBColor[0,0,9/16]},
		{1/9,Blue},
		{23/63,Cyan},
		{13/21,Yellow},
		{47/63,Orange},
		{55/63,Red},
		{1,RGBColor[1/2,0,0]}},
		u
	]/;0<=u<=1
	
linColor[u_?NumericQ] := 
	Blend[{
		{0,RGBColor[0,0,9/16]},
		{1/6,Blue},
		{2/6,Cyan},
		{3/6,Yellow},
		{4/6,Orange},
		{5/6,Red},
		{1,RGBColor[1/2,0,0]}},
		u
	]/;0<=u<=1		


graph3dWithMsure[vts_,edgs_,perEdgMsure_] := Module[
	{eColors, lines3d, linesColored3d(*fColors,faces3d,facesColored3d*), gc},
	eColors = jet/@Rescale[perEdgMsure];
	linesColored3d = Thread[{eColors,Line[#]&/@edgs}];
	gc = GraphicsComplex[vts,{linesColored3d(*,faces*)}];
	Graphics3D[gc, Boxed->False]
]


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


ExtractInfinitePart[vertices_,edges_, length_, opt_] := Block[
	{infPositions, infEdges},
	infPositions = Flatten[Position[Round @ Rescale[length], 1]];
	infEdges = edges[[#]]&/@ infPositions;
	Graphics3D[{opt, GraphicsComplex[vertices, Line/@infEdges], Boxed -> False}]
]


(* ::Subsection:: *)
(*InfinitePartManipulate*)


$rescalingParameter[{{xmin_,xmax_}, {ymin_,ymax_}, {zmin_,zmax_}}] := 
	{{xmin - 10, xmax + 10}, {ymin - 10, ymax + 10}, {zmin - 10, zmax + 10}}


InfinitePartManipulate[vertices_, edges_, length_, opt_] := Block[
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
				GraphicsComplex[vertices, Line /@ infEdges[[1;;i+1]]], Box -> False}
			],
			PlotRange -> boundingBox
		],
	{i,1,Length[infEdges]-1,1}]
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ CornRootsFileReader`Private`$PublicSymbols;


EndPackage[];
