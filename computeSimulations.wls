#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Text:: *)
(*This script produces simulations data and figures for Coherent Enhancement paper.*)


(* ::Section:: *)
(*Initialization*)


(* ::Text:: *)
(*Inflation simulator can be found at https://github.com/maxitg/InflationSimulator.*)


<< InflationSimulator`


figuresDirectory = FileNameJoin[{".", "figures"}];
If[!FileExistsQ[figuresDirectory], CreateDirectory[figuresDirectory]];


LaunchKernels[];
ParallelEvaluate[<<InflationSimulator`];


(* ::Section:: *)
(*Options*)


supersymmetryPointCount = 10000;


imageSize = 250;
fontFamily = "Latin Modern Roman";


(* ::Section:: *)
(*Supersymmetry*)


(* ::Subsection:: *)
(*Simulations*)


(* ::Text:: *)
(*Note, B, being an overall factor in front of potential, only affects the time-scale, but not dynamics of inflation, therefore, we can set it to one without affecting inflation observables.*)


supersymmetryLagrangian[f_, G_, B_: 1][bm_, t_] := 1/2 D[bm, t]^2 - 4 f^4 B^2 (
	Sum[
		l G[[l]] r G[[r]] (1 - Cos[r / Sqrt[2] bm / f]),
		{l, Length[G]}, {r, Length[G]}
	] - Sum[
		l r G[[l]] G[[r]] (1 - Cos[(r - l) / Sqrt[2] bm / f]),
		{l, Length[G]}, {r, l + 1, Length[G]}
	]
)


supersymmetryInputs = BlockRandom[Table[With[
	{
		fValue = RandomReal[{0, 1}],
		G5 = RandomReal[{-0.88931, -0.88920}],
		NPivot = RandomReal[{50, 60}]
	}, <|
		"fTrue" -> fValue,
		"G5" -> G5,
		"fieldInitial" -> RandomReal[{0, Pi fValue Sqrt[2]}],
		"pivotEfoldingsCount" -> NPivot
	|>], supersymmetryPointCount], RandomSeeding -> "NPCoherentSupersymmetry"];


Print["Simulating supersymmetry..."];


supersymmetryResults = ParallelMap[If[
	ExperimentallyConsistentInflationQ[
		supersymmetryLagrangian[#["fTrue"], {0, 0, 0, 1, #["G5"]}][bm[t], t],
		{bm[t], #["fieldInitial"], 0}, t, #["pivotEfoldingsCount"]
	],
	Join[
		#,
		Association @ Thread[{"fDynamic", "fStatic"} -> InflationValue[
			supersymmetryLagrangian[#["fTrue"], {0, 0, 0, 1, #["G5"]}][bm[t], t],
			{bm[t], #["fieldInitial"], 0}, t, #["pivotEfoldingsCount"],
			{
				InflationProperty[
					"EffectiveAxionDecayConstant",
					"HorizonExit",
					Method -> "FromHubbleParameter"],
				InflationProperty[
					"EffectiveAxionDecayConstant",
					"HorizonExit",
					Method -> "FromPotential"]
			}
		]]
	],
	Nothing] &, supersymmetryInputs];


(* ::Subsection:: *)
(*Figures*)


Print["Plotting supersymmetry..."];


Export[FileNameJoin[{figuresDirectory, "supersymmetryInputs.pdf"}], ListPlot[
	supersymmetryResults[[All, {"G5", "fTrue"}]],
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> {
		"\!\(\*FormBox[
			SubscriptBox[\(G\), \(5\)],
			TraditionalForm]\)",
		"\!\(\*FormBox[
			\(f/\*SubscriptBox[\"M\", StyleBox[\"P\",\nFontSlant->\"Plain\"]]\),
			TraditionalForm]\)"},
		ImageSize -> imageSize,
		LabelStyle -> Directive[FontFamily -> fontFamily],
		FrameTicks -> {
			{Automatic, None},
			{{#, NumberForm[#, {6, 5}]} & /@ Range[-0.88930, -0.88920, 0.00002],
			 None}}]];


Export[FileNameJoin[{figuresDirectory, "supersymmetryEnhancement.pdf"}], ListPlot[
	supersymmetryResults[[All, {"fTrue", "fStatic"}]],
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> {
		"\!\(\*FormBox[
			\(f/\*SubscriptBox[\"M\", StyleBox[\"P\",\nFontSlant->\"Plain\"]]\),
			TraditionalForm]\)",
		"\!\(\*FormBox[
			\(\*SubscriptBox[
				\"f\", StyleBox[\"e\",\nFontSlant->\"Italic\"]
			]/\*SubscriptBox[
				\"M\", StyleBox[\"P\",\nFontSlant->\"Plain\"]]\),
			TraditionalForm]\)"},
	ImageSize -> imageSize,
	LabelStyle -> Directive[FontFamily -> fontFamily]]];


supersymmetryDecayConstantsComparisonPointsPlot = ListPlot[
	supersymmetryResults[[All, {"fStatic", "fDynamic"}]], PlotRange -> All];


Export[
	FileNameJoin[{figuresDirectory, "supersymmetryDecayConstantsComparison.pdf"}],
	Show[
		Plot[
			x,
			{x, 0, 10},
			PlotStyle -> Black,
			Frame -> True,
			FrameLabel -> {
				"\!\(\*FormBox[
					\(\*SubscriptBox[
						\"f\", StyleBox[\"e\",\nFontSlant->\"Italic\"]
					]/\*SubscriptBox[
						\"M\", StyleBox[\"P\",\nFontSlant->\"Plain\"]]\),
					TraditionalForm]\)",
				"\!\(\*FormBox[
					\(\*SubscriptBox[
						\"f\", StyleBox[\"eH\",\nFontSlant->\"Italic\"]
					]/\*SubscriptBox[
						\"M\", StyleBox[\"P\",\nFontSlant->\"Plain\"]]\),
					TraditionalForm]\)"},
			ImageSize -> imageSize,
			LabelStyle -> Directive[FontFamily -> fontFamily],
			Evaluate @ AbsoluteOptions[
				supersymmetryDecayConstantsComparisonPointsPlot, PlotRange]],
		supersymmetryDecayConstantsComparisonPointsPlot]]


(* ::Section:: *)
(*Deinitialization*)


CloseKernels[];


Exit[];
