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


BeginPackage["RootAnalysis`", {"OperationFunctions`"}];


RootAnalysis`Private`$PublicSymbols={
	GenerateStem,
	GenerateBranch,
	GenerateRoot,
	GenerateDirStem,
	GenerateDirBranch,
	GenerateDirRoot,
	RootGraph,
	FindWhorlNode
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


FindWhorlNode::usage = $UsageString[
	"FindWhorlNode[`v`, `e`, `w`, \"Coordinate\"] gives the whorl node coordinate of a root given vertices `v`, edges `e`, and width `w`.\n",
	"FindWhorlNode[`v`, `e`, `w`, \"VertexID\"] gives the whorl node Vertex ID of a root given vertices `v`, edges `e`, and width `w`.\n",
	"FindWhorlNode[`v`, `e`, `w`, \"Edge\"] gives the whorl node edge of a root given  `v`, edges `e`, and width `w`.\n"
];


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*GenerateDirRoot*)


GenerateDirStem[start_, n_]:= 
	Thread[Range[n] + start - 1 -> Range[n] + start]
GenerateDirBranch[startNode_, stemEndNode_, n_]:= 
	Join[{startNode <-> stemEndNode + 1}, GenerateDirStem[stemEndNode + 1, n]]
GenerateDirRoot[wharlNode_, stemLength_, {seminalLength_, numSeminal_}, {lateralLength_, numLateral_}]/; numLateral < seminalLength - 1:= Module[
	{stem, seminalList, lateralList, i, j, k, counter, startNodeList, endNodeList, v, step},
	
	stem = GenerateDirStem[1, stemLength];
	(* Generate seminals *)
	seminalList = ToExpression["s" <> ToString @ #] &/@ Range[numSeminal];
	seminalList[[1]] = GenerateDirBranch[wharlNode, stemLength + 1, seminalLength];
	For[ i = 2, i <= numSeminal, i++,
		seminalList[[i]] = GenerateDirBranch[wharlNode, Last @ Last @ seminalList[[i - 1]], seminalLength]];
		
	(* Generate laterals *)
	counter = Last @ Last @ seminalList[[-1]];
	lateralList ={};
	For[j = 1, j <= numSeminal, j++,
		(* vertices of j-th seminal  *)
		v = Union @ Flatten @ (seminalList[[j, 2;;All]]/.DirectedEdge->List);	
		step = Floor[Length[v] / numLateral];
		startNodeList = Table[v[[step*i]], {i, 1, numLateral}];
		endNodeList = FoldList[Last @ Last @ GenerateDirBranch[#2, #1, lateralLength]&, counter, startNodeList][[;;-2]];
		AppendTo[lateralList, MapThread[GenerateDirBranch[#1, #2, lateralLength]&,{startNodeList,endNodeList}] ];
		counter = Last @ Last @ lateralList[[-1, -1]];
	];
	Flatten @ {stem, seminalList, lateralList}
]


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
(*FindWhorlNode*)


FindWhorlNode[v_, e_, w_, "VertexID"]:= Module[
	{maxPos},
	maxPos = First @ First @ Position[w, Max @ w];
	e[[maxPos]]
]


FindWhorlNode[v_, e_, w_, "deg3"]:= Module[
	{candidates, widthCandidates, pos},
	candidates = FindVertexDegreeN[e, 3]
]



(*

FindWhorlNode[v_, e_, w_, "VertexID"]:= Module[
	{candidates, widthCandidates, pos},
	candidates = FindVertexDegreeN[e, 3];
	widthCandidates = w[[candidates]];
	pos = First @ First @Position[widthCandidates, Max[widthCandidates]];
	candidates[[pos]]
]
FindWhorlNode[v_, e_, w_, "Coordinate"]:= Module[
	{pos},
	pos = First @ First @ Position[w, Max[w]];
	v\[LeftDoubleBracket]First[e\[LeftDoubleBracket]pos\[RightDoubleBracket]]\[RightDoubleBracket]
]
FindWhorlNode[v_, e_, w_, "VertexID"]:= Module[
	{pos},
	pos = First @ First @ Position[w, Max[w]];
	First[e\[LeftDoubleBracket]pos\[RightDoubleBracket]]
]
FindWhorlNode[v_, e_, w_, "Edge"]:= Module[
	{pos},
	pos = First @ First @ Position[w, Max[w]];
	e\[LeftDoubleBracket]pos\[RightDoubleBracket]
]*)


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


Protect /@ RootAnalysis`Private`$PublicSymbols;


EndPackage[];
