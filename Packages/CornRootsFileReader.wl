(* ::Package:: *)

(* ::Title:: *)
(*Corn Roots File Reader*)


(* ::Section:: *)
(*To Do*)


(* ::Text:: *)
(*Write clear usage for each function.*)


(* ::Section:: *)
(*Public*)


BeginPackage["CornRootsFileReader`"];


CornRootsFileReader`Private`$PublicSymbols = {
	ConvertExp,
	ParseSKMfile
};


Unprotect /@ CornRootsFileReader`Private`$PublicSymbols;


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Usage*)


$ArgStyle[arg_Integer] := "TR";
$ArgStyle["\[Ellipsis]"] := "TR";
$ArgStyle[str_String] := "TI";


$ArgString[arg_] :=
	"\!\(\*StyleBox[\"" <> ToString[arg] <> "\", \"" <> $ArgStyle[arg] <> "\"]\)"


$UsageString[str__] :=
	(StringTemplate[StringJoin[{str}]] /. {TemplateSlot[s_] :> $ArgString[s]})[]


ConvertExp::usage = $UsageString[
	"ConvertExp[`string`] converts e+/e- notation to Mathematica's notation."
];


ParseSKMfile::usage = $UsageString[
	"ParseSKMfile[`SKMfile`] parse the `SKMfile` into a list of element with structure {vertices, edges, faces, edge-measure, face-measure}."
];


(* ::Subsection:: *)
(*ConvertExp*)


ConvertExp[string_] :=
    StringReplace[string, {"e+" :> "*^", "e-" :> "*^-"}]


(* ::Subsection:: *)
(*ParseSKM*)


ParseSKMfile[skmFile_] := Block[
	{raw, parsed, numVts, numEdges, numFaces, vts, edges, faces, edgeMeasure, faceMeasure},
	raw = ConvertExp[ReadString[skmFile]];
	parsed = ToExpression/@
			(StringSplit/@Select[
				StringSplit[raw,"\n"],(StringTake[#, {1}] != "#") &]);
	{numVts, numEdges, numFaces} = parsed[[1]];
	vts = parsed[[2 ;; numVts + 1]];
	edges = If[numEdges > 0, parsed[[numVts + 2 ;; numVts + numEdges + 1 ]], {}];
	faces = If[numFaces > 0, parsed[[numVts + numEdges + 2 ;; numVts + numEdges + numFaces + 1]], {}];
	
	If[numEdges > 0, 
		edgeMeasure = edges[[All, 3;;5]];
		edges = #[[1;;2]]+1 &/@ edges,
		edges = {}
	];
	
	If[numFaces > 0,
		faceMeasure = faces[[All, 4;;5]];
		faces = #[[1;;3]]+1 &/@ faces,
		faces = {}
	];
	{vts, edges, faces, edgeMeasure, faceMeasure}
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ CornRootsFileReader`Private`$PublicSymbols;


EndPackage[];
