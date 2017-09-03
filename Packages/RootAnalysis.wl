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
	FindWhorlNode,
	VertexToEdgeIndex
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


VertexToEdgeIndex::usage = $UsageString[
	"VertexToEdgeIndex[`v`, `e`] converts a sequence of vertices `v` to indices of edge `e`"
];


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*BFS*)


BFS[graph_, startNode_]:= Block[
	{V, u, queue, visited, frontier, i, counter, result ={}, outVetices, distance},
	
	V = VertexList[graph];
	(* initialize *)
	visited = Association[Thread[ V -> Table[0, Length[V]]]];
	distance = Association[Thread[ V -> Table[0, Length[V]]]];
	visited [startNode] = 1;
	queue = {startNode};
	
	(*counter = 0;*)
	While[Length[queue] != 0,
		u = First[queue];  (* Dequeue *)
		(*Print["queue ", queue];*)
		queue = Delete[queue, 1]; (* Dequeue *)
		(*Print["u ", u];*)
		AppendTo[result, u];		
		(*Print["queue ", queue];*)
		
		frontier = VertexOutComponent[graph, u, 1] ;
		For[i = 1, i <= Length[frontier], i++,
			If[visited[frontier[[i]]] == 0,
				visited[frontier[[i]]] = 1;
				(*Print["u2 ", u, " u.d = ", distance[u]];*)
				distance[frontier[[i]]] = distance[u] + 1; 
				AppendTo[queue, frontier[[i]]];	
			];	
		];
		visited[u] = 1
	];
	{result, distance}
]


(* ::Subsection:: *)
(*DFS*)


DFS[g_, startNode_]:= Block[
	{V, stack = {}, visited, current, i, frontier, counter = 0, result = {}, parent},	
	V = VertexList[g];
	(* initialize *)
	visited = Association[Thread[ V -> Table[0, Length[V]]]];
	AppendTo[stack, startNode]; (* Let S be a stack *)
	parent = Association[Thread[ V -> Table[0, Length[V]]]];
	
	While[Length[stack] != 0 && counter <= 50 ,
		current = Last[stack]; (* v = S.pop() *)	
		stack = Delete[stack, -1];
		counter++;
	
		If[visited[current] == 0, (* if v is not labeled as visited: *)
			visited[current] = 1; (* label v as visited *)
			AppendTo[result, current];
			If[VertexDegree[g, current] == 1, AppendTo[result, "Nil"]];	
			frontier = AdjacencyList[g, current];
			(*Print["frontier: ", frontier];*)
			If[Length[frontier]==0, Print["Nil"]];
			For[i = 1, i <= Length[frontier], i++,
				If[visited[frontier[[i]]] == 0,
					(*visited[frontier\[LeftDoubleBracket]i\[RightDoubleBracket]] = 1;*)
					AppendTo[stack, frontier[[i]]];
					parent[frontier[[i]]] = current;
				];	
			]
			(*Print["Stack: ", stack];*)
		];
	];
	{result, parent}
]


(* ::Text:: *)
(*From CLRS Page 603*)


(*myDFS[g_,s_]:=Block[
	{result = {}, V, parents, color, discovered, frontier, i, time},
	V = VertexList[g];
	
	(*initialize*)
	parents = Association[Thread[V \[Rule] Table[0, Length[V]]]];
	color = Association[Thread[V \[Rule] Table[0, Length[V]]]];
	discovered = Association[Thread[V \[Rule] Table[0, Length[V]]]];
	
	time = 0;
	For[i = 1, i < Length[V],
		i++,
		dfsVisit[g, vtxList[V[i]], time, color, depth]
	];
	result
]

dfsVisit[g_, u_, time_, color_, depth_]:= Block[
	{frontier, i},
	time = time + 1;
	depth[u] = time;
	color[u] = 1;
	frontier = AdjacencyList[g, u];
	For[i = 1, i \[LessEqual] Length[frontier], i++,
		If[color[frontier\[LeftDoubleBracket]i\[RightDoubleBracket]] \[Equal] 0,
			dfsVisit[g, u, time, color, depth]
		]
	];
	color[u] = 2;
	time = time + 1;
	[u] = time;	
]*)


(* ::Subsection:: *)
(*Reverse Tree*)


ReverseTree[g_]:= DirectedEdge @@@ (Reverse/@ List@@@g)


(* ::Subsection:: *)
(*AutomateLabeling*)



labeling[g_, root_, distance_]:= Block[
	{V, current, visited, queue, frontier, label, nextVtx, labels, i, result, distanceFromTheLeaf, 
	distanceOrdering, frontierLabels},
	
	V = VertexList[g];
	
	visited = Association[Thread[ V -> Table[0, Length[V]]]];
	visited[root] = 1;
	
	label = Association[Thread[ V -> Table[1, Length[V]]]];
	queue = {root};
	Print[label];
	While[Length[queue] != 0,
		current = First[queue];
		queue = Delete[queue, 1];
		
		frontier = DeleteCases[VertexOutComponent[g, current, 1], current];
		distanceFromTheLeaf = distance /@ frontier;
		distanceOrdering = Ordering[distanceFromTheLeaf, All, Greater] - 1;
		(*Print[distanceOrdering]*);
		
		frontierLabels = label[current] + distanceOrdering;
		
		Do[label[frontier[[k]]] = frontierLabels[[k]], {k,1,Length[frontier]}];
		
		(*Print[frontierLabels];*)
		(*Print[frontier]*);
		For[i = 1, i <= Length[frontier], i++,
			If[visited[frontier[[i]]] == 0,
				visited[frontier[[i]]] = 1;
				(*Print["u2 ", u, " u.d = ", distance[u]];*)			
				AppendTo[queue, frontier[[i]]];	
			];	
		];
		visited[current] = 1
	];
	label
	
]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*RootGraph*)


RootGraph[graphData_]:= Graph[
	graphData, 
	VertexLabels -> "Name", 
	ImagePadding -> 10, 
	GraphLayout -> {"LayeredDigraphEmbedding",
				  "Orientation" -> Top,
				  "RootVertex" -> First@First@graphData}
]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*VertexToEdgeIndex*)


VertexToEdgeIndex[vtx_List, edges_]:= Module[
	{edgeList},
	(* Apply Sort to make v1 < v2 in {v1, v2} pair so it could be found in orginal edges *)
	edgeList = Sort /@ Partition[vtx, 2, 1];
	Flatten[Position[edges, #] &/@ edgeList]
]


(* ::Subsection::Closed:: *)
(*End*)


End[];


(* ::Section::Closed:: *)
(*End*)


Protect /@ RootAnalysis`Private`$PublicSymbols;


EndPackage[];
