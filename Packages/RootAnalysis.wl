(* ::Package:: *)

(* ::Title:: *)
(*Root Analysis*)


(* ::Subtitle:: *)
(*Wenzhen Zhu*)


(* ::Subsubtitle:: *)
(*Date: 07/18/2017*)


(* ::Section:: *)
(*To Do*)


(* ::Section:: *)
(*Public*)


BeginPackage["RootAnalysis`"];


RootAnalysis`Private`$PublicSymbols={
	GenerateStem,
	GenerateBranch,
	GenerateRoot,
	RootGraph
};


Unprotect/@RootAnalysis`Private`$PublicSymbols;


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


GenerateStem::usage = $UsageString[
	"GenerateStem[`start, n`] generate a stem with start node and length n."
];


GenerateBranch::usage = $UsageString[
	"GenerateBranch[`start, end, n`] generate a branch with start node and length n, end is the last node in the current graph."
];


GenerateRoot::usage = $UsageString[
	"GenerateRoot[`wharlNode, stemLength, {seminalLength, numSeminal}, {lateralLength, numLateral}`] ",
	"generate a tree with specify wharlNode, length of stem, seminal's length, number of seminals, lateral's length, and number of laterals."
];


RootGraph::usage = $UsageString[
	"RootGraph[{\!\(\*SubscriptBox[\(e\), \(1\)]\),\!\(\*SubscriptBox[\(e\), \(2\)]\), ...}] yields a graph in root structure with edges \!\(\*SubscriptBox[\(e\), \(j\)]\)"
];


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*GenerateRoot*)


GenerateStem[start_, n_]:= 
	Thread[Range[n] + start - 1 <-> Range[n] + start]
GenerateBranch[startNode_, stemEndNode_, n_]:= 
	Join[{startNode <-> stemEndNode + 1}, GenerateStem[stemEndNode + 1, n]]
GenerateRoot[wharlNode_, stemLength_, {seminalLength_, numSeminal_}, {lateralLength_, numLateral_}]/; numLateral < seminalLength - 1:= Module[
	{stem, seminalList, lateralList, i, j, k, counter, startNodeList, endNodeList, v, step},
	
	stem = GenerateStem[1, stemLength];
	(* Generate seminals *)
	seminalList = ToExpression["s" <> ToString @ #] &/@ Range[numSeminal];
	seminalList[[1]] = GenerateBranch[wharlNode, stemLength + 1, seminalLength];
	For[ i = 2, i <= numSeminal, i++,
		seminalList[[i]] = GenerateBranch[wharlNode, Last @ Last @ seminalList[[i - 1]], seminalLength]];
		
	(* Generate laterals *)
	counter = Last @ Last @ seminalList[[-1]];
	lateralList ={};
	For[j = 1, j <= numSeminal, j++,
		(* vertices of j-th seminal  *)
		v = Union @ Flatten @ (seminalList[[j, 2;;All]]/.UndirectedEdge->List);	
		step = Floor[Length[v] / numLateral];
		startNodeList = Table[v[[step*i]], {i, 1, numLateral}];
		endNodeList = FoldList[Last @ Last @ GenerateBranch[#2, #1, lateralLength]&, counter, startNodeList][[;;-2]];
		AppendTo[lateralList, MapThread[GenerateBranch[#1, #2, lateralLength]&,{startNodeList,endNodeList}] ];
		counter = Last @ Last @ lateralList[[-1, -1]];
	];
	
	(*lateralList = Table[ToExpression["l" <> ToString@i <>ToString@j], {i, 1, numSeminal},{j, 1, numLateral}];
	
	lateralList[[1,1]] = generateBranch[Last @ seminalList[[1, Floor[Length[Last[seminalList]]/numLateral]]], Last @ Last @ seminalList[[numSeminal]], lateralLength];
	For[j = 2, j \[LessEqual]numSeminal, j++,
		lateralList[[j, 1]] = generateBranch[Last @ seminalList[[j, Floor[Length[Last[seminalList]]/numLateral]]], Last @ Last @ lateralList[[j-1,1]], lateralLength]]
	For[ j = 1, j <= numSeminal, j++,
		For[k = 2, k \[LessEqual] numLateral, k++,
			lateralList[[j,k]] = generateBranch[ j * Floor[Length[Last[seminalList]]/numLateral], 
						    Last @ Last @ lateralList[[j, k- 1]], 
							lateralLength]
		]
	];*)
	Flatten @ {stem, seminalList, lateralList}
]


(* ::Subsection:: *)
(*RootGraph*)


RootGraph[graphData_]:= Graph[
	graphData, 
	VertexLabels -> "Name", 
	ImagePadding -> 10, 
	GraphLayout -> {"LayeredDigraphEmbedding",
				  "Orientation" -> Top,
				  "RootVertex" -> 1}
]


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ RootAnalysis`Private`$PublicSymbols;


EndPackage[];
