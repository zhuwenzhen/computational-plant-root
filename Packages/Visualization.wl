(* ::Package:: *)

(* ::Title:: *)
(*Corn Roots Visualization*)


(* ::Section:: *)
(*To Do*)


(* ::Text:: *)
(*Write Manipulate function to visualize the loop*)


(* ::Section:: *)
(*Private*)


jet[u_?NumericQ] :=
	Blend[{
		{0,RGBColor[0,0,9/16]},
		{1/9,Blue},
		{23/63,Cyan},
		{13/21,Yellow},
		{47/63,Orange},
		{55/63,Red},{
		1,RGBColor[1/2,0,0]}},
		u
	]/;0<=u<=1
	
linColor[u_?NumericQ] := 
	Blend[{
		{0,RGBColor[0,0,9/16]},{1/6,Blue},
		{2/6,Cyan},{3/6,Yellow},
		{4/6,Orange},{5/6,Red},{1,RGBColor[1/2,0,0]}},
		u
	]/;0<=u<=1		


graph3dWithMsure[vts_,edgs_,perEdgMsure_] := Module[
	{eColors, lines3d, linesColored3d(*fColors,faces3d,facesColored3d*), gc},
	eColors = jet/@Rescale[perEdgMsure];
	linesColored3d = Thread[{eColors,Line[#]&/@edgs}];
	gc = GraphicsComplex[vts,{linesColored3d(*,faces*)}];
	Graphics3D[gc, Boxed->False]
]
