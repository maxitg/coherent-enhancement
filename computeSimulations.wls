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


supersymmetryPointCount = 100000;


supersymmetryRange = <|
	"fTrue" -> {0, 1},
	"G5" -> {-0.88931, -0.88920},
	"pivotEfoldingsCount" -> {50, 60}|>;


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
		fValue = RandomReal[supersymmetryRange[["fTrue"]]],
		G5 = RandomReal[supersymmetryRange[["G5"]]],
		NPivot = RandomReal[supersymmetryRange[["pivotEfoldingsCount"]]]
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
		Association @ Thread[{"fDynamic", "fStatic", "r"} -> InflationValue[
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
					Method -> "FromPotential"],
				InflationProperty[
					"TensorToScalarRatio",
					"HorizonExit",
					Method -> "FromHubbleParameter"]
			}
		]]
	],
	Nothing] &, supersymmetryInputs];


(* ::Subsection:: *)
(*Figures*)


Print["Plotting supersymmetry..."];


Export[FileNameJoin[{figuresDirectory, "supersymmetryPotential.pdf"}], Plot[
	Evaluate[With[{f = #[["fTrue"]], G5 = #[["G5"]]},
		supersymmetryLagrangian[f, {0, 0, 0, 1, G5}][f bf, t] /
			supersymmetryLagrangian[f, {0, 0, 0, 1, G5}][f \[Pi] Sqrt[2], t]] & /@
				supersymmetryResults],
	{bf, -0.1 Pi Sqrt[2], 1.1 Pi Sqrt[2]},
	Frame -> True,
	FrameLabel -> {
		"\!\(\*FormBox[\(\*SubscriptBox[\(b\), \(-\)]/f\), TraditionalForm]\)",
		"\!\(\*FormBox[\(V/\*SubscriptBox[\(V\), \(max\)]\), TraditionalForm]\)"},
	PlotStyle -> ColorData[97, 1],
	ImageSize -> imageSize,
	LabelStyle -> Directive[FontFamily -> fontFamily]]];


Export[FileNameJoin[{figuresDirectory, "supersymmetryTensorToScalar.pdf"}], ListPlot[
	supersymmetryResults[[All, {"fTrue", "r"}]],
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> {
		"\!\(\*FormBox[
			RowBox[{
				\"f\",
				\"/\",
				SubscriptBox[\"M\", StyleBox[\"P\",\nFontSlant->\"Plain\"]]}],
			TraditionalForm]\)",
		"\!\(\*FormBox[\(r\), TraditionalForm]\)"},
	ImageSize -> imageSize,
	LabelStyle -> Directive[FontFamily -> fontFamily]]];


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
		supersymmetryDecayConstantsComparisonPointsPlot]];


distributionToTeX[{min_, max_}] :=
	"\\mathcal{U}\\left(" <> ToString[min] <> ", " <>
	ToString[max] <> "\\right)"


Export[
	FileNameJoin[{figuresDirectory, "supersymmetry.txt"}],
	StringJoin[{
		"Simulation results for the global supersymmetry model ",
		"Eq.~(\\ref{eq:supersymmetry:Vslow}). ",
		"All " <> ToString[Length @ supersymmetryResults] <> " points shown ",
		"are consistent with experimental data on $r$ and $n_s$. ",
		"Here $q = 5$, $G_1 = G_2 = G_3 = 0$, $G_4 = 1$, ",
		"$G_5 \\sim ",
		distributionToTeX[NumberForm[#, {6, 5}] & /@ supersymmetryRange[["G5"]]],
		"$, where $\\mathcal{U}$ refers to a uniform distribution. ",
		"We set $B = 1$, as it only affects the time scale of inflation, but not ",
		"the values of $n_s$, $r$, and $f_e$. ",
		"Finally, $f / M_\\text{P} \\sim ",
		distributionToTeX[supersymmetryRange[["fTrue"]]] <> "$, ",
		"$b_{-, 0} \\sim " <> distributionToTeX[{0, "\\pi \\sqrt{2} f"}] <> "$, ",
		"$N_\\text{pivot} \\sim ",
		distributionToTeX[supersymmetryRange[["pivotEfoldingsCount"]]] <> "$. ",
		"Top left panel shows the superimposed potentials for all values of $G_5$ ",
		"considered. It looks like a single line due to fine-tuning in $G_5$. ",
		"Top right panel shows tensor-to-scalar ratios we obtained. ",
		"Bottom left panel demonstrates the coherent enhancement of the decay constant,",
		" and the bottom right panel compares effective decay constants computed ",
		"from the potential and from the Hubble parameter, ",
		"black line corresponds to $f_{eH} = f_e$."}]]


(* ::Section:: *)
(*Deinitialization*)


CloseKernels[];


Exit[];
