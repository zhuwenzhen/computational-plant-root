(* ::Package:: *)

(* ::Title:: *)
(*Fair Simplify*)


(* ::Section:: *)
(*Public*)


BeginPackage["FairSimplify`"];


FairSimplify`Private`$PublicSymbols = {
	Fair2D
};


Unprotect /@ FairSimplify`Private`$PublicSymbols;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Fair2D*)


fair2DIter[curve_, \[Lambda]_]:= Module[
	{verts = curve[[1]], vecs, totw, v},
	
	totw=Map[0&,verts];
	vecs=Map[Map[0&,#]&,verts];

	(* gather vectors to mid-points in a single pass over all edges *)
	Scan[{
		 v = verts[[#[[2]]]] - verts[[#[[1]]]];
		totw[[#[[1]]]] +=1;
		totw[[#[[2]]]] +=1;
		vecs[[#[[1]]]] +=v;
		vecs[[#[[2]]]] -=v 
		}&, 
		curve[[2]]
	];
	vecs = MapThread[(#1 / #2)&,{ vecs,totw}];
	(* perform smoothing *)
	verts = MapThread[(#1 + \[Lambda]*#2)&,{verts,vecs}];
	{verts,curve[[2]]}
]
		
Fair2D[curve_, \[Lambda]_, \[Kappa]_, num_]:=Module[
	{ncurve=curve, \[Lambda]2 = 1/(\[Kappa]-1/\[Lambda])},
	Do[
		ncurve=fair2DIter[ncurve, \[Lambda]];
		ncurve=fair2DIter[ncurve, \[Lambda]2],
		{num}
	];
	ncurve
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ FairSimplify`Private`$PublicSymbols;


EndPackage[];
