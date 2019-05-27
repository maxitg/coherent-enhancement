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


cacheDirectory = FileNameJoin[{".", "cache"}];
If[!FileExistsQ[cacheDirectory], CreateDirectory[cacheDirectory]];


LaunchKernels[];
ParallelEvaluate[<<InflationSimulator`];


(* ::Section:: *)
(*Simulation code*)


(* ::Subsection:: *)
(*Input generator*)


generateInputs[parameterDistributions_, seed_, n_] :=
	BlockRandom[
		Table[Association @ KeyValueMap[
			#1 -> RandomVariate[#2] &,
			parameterDistributions], n],
		RandomSeeding -> seed]


(* ::Subsection:: *)
(*Simulation*)


minSubhorizonEfoldings = 5;


simulate[lagrangian_, initialConditions_, inputs_, options_] := Module[
		{hash, filename, results, counter},
	hash = Hash[{PacletInformation["InflationSimulator"], DownValues[simulate],
			lagrangian,
			(lagrangian[MapIndexed[#2[[1, 1]] &, inputs[[1]]]]
				@@ Through[initialConditions[[All, 1]][t]])[t],
			initialConditions, inputs, options}];
	filename = FileNameJoin[{cacheDirectory, IntegerString[hash, 16] <> ".wxf"}];
	If[FileExistsQ[filename],
		(* read cache *)
		Print["Using cached simulation results"];
		Import[filename, "WXF"],
		(* no cache, compute from scratch *)
		counter = 0;
		SetSharedVariable[counter];
		SetSharedFunction[Print];
		SetOptions[$Output, FormatType -> OutputForm];
		results = ParallelMap[Module[{L, evolution, result, c, exitValues},
			ClearSystemCache[];
			L = (lagrangian[#] @@ Through[initialConditions[[All, 1]][t]])[t];
			evolution = InflationEvolution[L, initialConditions /. #, t, options];
			result = If[
				evolution["TotalEfoldings"] >=
						#["pivotEfoldings"] + minSubhorizonEfoldings &&
					evolution["FinalDensitySign"] == 0,
				Join[
					#,
					Association @ Thread[
						{"fDynamic",
								"r",
								"ns",
								"exitFields",
								"endFields",
								"fStatic",
								"consistentQ"} ->
							Join[
								exitValues = InflationValue[
									L,
									evolution,
									t,
									#["pivotEfoldings"],
									{"EffectiveAxionDecayConstant",
										"TensorToScalarRatio",
										"ScalarSpectralIndex",
										initialConditions[[All, 1]]},
									"HorizonExit"],
								InflationValue[
									L,
									evolution,
									t,
									#["pivotEfoldings"],
									{initialConditions[[All, 1]]},
									"End"],
								InflatonLagrangianValue[
									L,
									Transpose[
										{initialConditions[[All, 1]],
											exitValues[[4]]}],
									t,
									{"EffectiveAxionDecayConstant"}],
								{ExperimentallyConsistentInflationQ[
									L, evolution, t, #["pivotEfoldings"]]}]]],
				Nothing];
				c = ++counter;
				If[Quotient[100 c, Length[inputs]] >
						Quotient[100 (c - 1), Length[inputs]],
					Print[Quotient[100 c, Length[inputs]], "%..."]];
			result] &, inputs];
		Export[filename, results, "WXF"];
		results
	]
];


(* ::Subsection:: *)
(*Plotting*)


Options[makeFigure] =
	{"imageSize" -> 250,
	 "fontFamily" -> "Latin Modern Roman"};


traditionalLabel[l_] := "\!\(\*FormBox[\(" <> l <> "\), TraditionalForm]\)"
plainLabel[l_] := "\(\*StyleBox[\"" <> l <> "\", FontSlant -> \"Plain\"]\)"
italicLabel[l_] := "\(\*StyleBox[\"" <> l <> "\", FontSlant -> \"Italic\"]\)"
subscriptLabel[m_, s_] := "\(\*SubscriptBox[" <> m <> ", " <> s <> "]\)"
rowLabel[el_List] := "\(\*RowBox[{" <> StringRiffle[el, ", "] <> "}]\)"
ratioLabel[l1_, l2_] := rowLabel[{l1, "\"/\"", l2}]
planckMassLabel = subscriptLabel[italicLabel["M"], plainLabel["P"]];
label[x_] := traditionalLabel[If[Head[$label[x]] === $label, x, $label[x]]]


$label["r"] = $label["TensorToScalarRatio"] = italicLabel["r"];
$label["ns"] = $label["ScalarSpectralIndex"] =
	subscriptLabel[italicLabel["n"], italicLabel["s"]];
$label["f"] = ratioLabel[italicLabel["f"], planckMassLabel];
$label["fStaticUnits"] = subscriptLabel[italicLabel["f"], italicLabel["e"]];
$label["fStatic"] = ratioLabel[$label["fStaticUnits"], planckMassLabel];
$label["fDynamic"] =
	ratioLabel[subscriptLabel[italicLabel["f"], italicLabel["eH"]], planckMassLabel];
$label["fieldRange", field_] := rowLabel[{italicLabel["\[CapitalDelta]"], field}];
$label["fieldRangeUnitless", field_] :=
	ratioLabel[$label["fieldRange", field], planckMassLabel];
$label["pivotEfoldings"] = subscriptLabel[italicLabel["N"], plainLabel["pivot"]];
$label["VNormalized"] = ratioLabel[
	italicLabel["V"], subscriptLabel[italicLabel["V"], plainLabel["max"]]];
$label["NonGaussianityAmplitude"] = subscriptLabel[italicLabel["f"], italicLabel["NL"]];
$label["Alpha1"] = subscriptLabel[italicLabel["\[Alpha]"], plainLabel["1"]];


figureName[name_String] := name <> ".pdf"
figureName[{f_, p1_String, p2_String, args___}] := p1 <> "_" <> p2 <> ".pdf"
figureName[{name_String, args___}] := name <> ".pdf"


makeFigure[specs_, name_, output_, OptionsPattern[]] := (
	Print[figureName[name], "..."];
	Export[
		FileNameJoin[{
			figuresDirectory,
			specs["name"] <> "_" <> figureName[name]}],
		Show[
			plot[name, specs, output],
			ImageSize -> OptionValue["imageSize"],
			LabelStyle -> Directive[FontFamily -> OptionValue["fontFamily"]]]])


plot[{"potentialRange", min_, max_, step_}, specs_, output_] := ListPlot[
	If[Length[specs["initialConditions"]] != 1,
		Print["potentialRange can only be plotted for single field models."];
		Quit[]];
	ParallelTable[{bf, #[
		Evaluate[With[{f = #[["f"]]},
			specs["lagrangian"][#][f bf][t] /
				specs["lagrangian"][#][f \[Pi] Sqrt[2]][t]] &
					/@ Select[#["consistentQ"] &] @ output]]},
	{bf, min, max, step}] & /@ {Min, Max},
	Axes -> False,
	Frame -> True,
	FrameLabel -> {
		label[ratioLabel[specs["fieldLabels"][[1]], italicLabel["f"]]],
		label[$label["VNormalized"]]},
	PlotStyle -> ColorData[97, 1],
	Joined -> True,
	Filling -> {1 -> {2}},
	PlotRange -> All]


plot[{"potential", parameters_, min_, max_}, specs_, output_] := Plot[
	If[Length[specs["initialConditions"]] != 1,
		Print["potentialRange can only be plotted for single field models."];
		Quit[]];
	-specs["lagrangian"][parameters][b][t], {b, min, max},
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> {
		label[ratioLabel[specs["fieldLabels"][[1]], planckMassLabel]],
		label[$label["VNormalized"]]}]


plot[{func_, p1_, p2_}, specs_, output_] := func[
	Select[#["consistentQ"] &][output][[All, {p1, p2}]],
	PlotRange -> All,
	PlotStyle -> ColorData[97, 3],
	Frame -> True,
	FrameLabel -> label /@ {p1, p2}]


plot["ns_r", specs_, output_] := Module[{
		consistentOutput, inconsistentOutput, func, consistentRange,
		minNs, minR, maxNs, maxR},
	{consistentOutput, inconsistentOutput} = Table[
		Select[func @ #["consistentQ"] &][output][[All, {"ns", "r"}]],
		{func, {Identity, Not}}];
	{consistentRange["ns"], consistentRange["r"]} =
		- Subtract @@ MinMax[consistentOutput[[All, #]]] & /@ {"ns", "r"};
	{minNs, minR} = Max[
		Min[consistentOutput[[All, #]]] - 0.5 consistentRange[#],
		Min[inconsistentOutput[[All, #]]]] & /@ {"ns", "r"};
	{maxNs, maxR} = Min[
		Max[consistentOutput[[All, #]]] + 0.5 consistentRange[#],
		Max[inconsistentOutput[[All, #]]]] & /@ {"ns", "r"};
ListPlot[
	{Select[!#["consistentQ"] &][output][[All, {"ns", "r"}]],
		Select[#["consistentQ"] &][output][[All, {"ns", "r"}]]},
	PlotRange -> {{minNs, maxNs}, {minR, maxR}},
	PlotStyle -> {ColorData[97, 4], ColorData[97, 3]},
	Frame -> True,
	FrameLabel -> label /@ {"ns", "r"}]]


plot["fStatic_fDynamic", specs_, output_] := With[
	{pointsPlot = plot[
		{ListPlot, "fStatic", "fDynamic"},
		specs["lagrangian"],
		Select[#["consistentQ"] &][output]]},
	Show[
		Plot[
			x,
			{x, 0, 10},
			PlotStyle -> Black,
			Frame -> True,
			FrameLabel -> {label["fStatic"], label["fDynamic"]},
			Evaluate @ AbsoluteOptions[pointsPlot, PlotRange]],
		pointsPlot]]


plot["fieldRange_fStatic_ratio", specs_, output_] := Module[{consistentOutput},
	consistentOutput = Select[#["consistentQ"] &][output];
	ListPlot[
		Transpose[{
			Norm /@ (consistentOutput[[All, "exitFields"]]
					- consistentOutput[[All, "endFields"]]) /
				consistentOutput[[All, "fStatic"]],
			Norm /@ (consistentOutput[[All, "exitFields"]]
					- consistentOutput[[All, "endFields"]])}],
		PlotRange -> All,
		PlotStyle -> ColorData[97, 3],
		Frame -> True,
		FrameLabel -> {
			label[ratioLabel[
				$label["fieldRange", #], $label["fStaticUnits"]]],
			label[$label["fieldRangeUnitless", #]]} &
				@ If[
					Length[specs["fieldLabels"]] == 1,
					specs["fieldLabels"][[1]],
					"\[Phi]"]]]


plot[{"density", sortingFunction_}, specs_, output_] := Module[{
		chosenExample, L, time, evolution, densityFunction, initialDensity},
	chosenExample = First @ SortBy[Select[#["consistentQ"] &][output], sortingFunction];
	Print["Using example: ", chosenExample];
	L = (specs["lagrangian"][#]
			@@ Through[specs["initialConditions"][[All, 1]][time]])[time] &
		@ chosenExample;
	evolution = InflationEvolution[
		L,
		specs["initialConditions"] /. chosenExample,
		time,
		Lookup[specs, "evolutionOptions", {}]];
	densityFunction =
		InflatonDensity[L, Through[specs[["initialConditions"]][[All, 1]][time]], time]
			/. evolution;
	initialDensity = densityFunction /. time -> 0;
	ParametricPlot[
		Evaluate[{
			evolution["Efoldings"][time] - evolution["TotalEfoldings"],
			densityFunction / initialDensity}],
		{time, 0, evolution["IntegrationTime"]},
		PlotRange -> All,
		Frame -> True,
		AspectRatio -> 1 / GoldenRatio,
		PlotPoints -> 1000,
		FrameLabel -> {
			label[italicLabel["N"]],
			label[ratioLabel[
				italicLabel["\[Rho]"],
				subscriptLabel[italicLabel["\[Rho]"], plainLabel["0"]]]]}]
]


(* ::Subsection:: *)
(*Figure captions*)


distributionToTeX[UniformDistribution, min_String, max_String] :=
	"\\mathcal{U}\\left(" <> min <> ", " <> max <> "\\right)"


distributionToTeX[distribution_[{min_, max_}]] :=
	distributionToTeX[distribution, ToString[TeXForm[min]], ToString[TeXForm[max]]]


distributionToTeX[distribution_[{min_, max_}], numberFormSpec_] := distributionToTeX[
	distribution,
	ToString[NumberForm[min, numberFormSpec]], ToString[NumberForm[max, numberFormSpec]]]


distributionToTeX[specs_, p_, args___] :=
	distributionToTeX[specs["parameterDistributions"][p], args]


makeCaption[specs_, results_] := StringTemplate[
	specs["caption"],
	InsertionFunction -> (
			ToString[# /. distribution[args___] :> distributionToTeX[specs, args]] &)][
		<|"totalPoints" -> specs["pointCount"], "points" -> Length @ results|>]


makeCaption[specs_, results_] := Export[
	FileNameJoin[{figuresDirectory, specs["name"] <> ".txt"}],
	StringTemplate[
		specs["caption"],
		InsertionFunction -> (ToString[
				# /. distribution[args___] :> distributionToTeX[specs, args]] &)][
			<|"totalPoints" -> specs["pointCount"],
				"points" -> Length @ Select[#["consistentQ"] &] @ results|>]]


(* ::Subsection:: *)
(*Evaluator*)


evaluateModel[modelSpecs_] := Module[{inputs, results, time},
	Print[modelSpecs["name"] <> ": generating inputs..."];
	inputs = generateInputs[
		modelSpecs["parameterDistributions"],
		"NP.coherent." <> modelSpecs["name"],
		Round[modelSpecs["pointCount"] pointCountMultiplier]];
	Print[modelSpecs["name"] <> ": simulating inflation..."];
	{time, results} = AbsoluteTiming[simulate[
		modelSpecs["lagrangian"],
		modelSpecs["initialConditions"],
		inputs,
		Lookup[modelSpecs, "evolutionOptions", {}]]];
	Print["Done in ", time, " seconds."];
	Print["Found ", Length @ results, " points."];
	Print[modelSpecs["name"] <> ": generating figures..."];
	makeFigure[modelSpecs, #, results] & /@ modelSpecs["figures"];
	Print[modelSpecs["name"] <> ": generating captions..."];
	makeCaption[modelSpecs, results];
	Print[modelSpecs["name"] <> ": done."];
]


(* ::Section:: *)
(*Global parameters*)


pointCountMultiplier = 1;


genericParameterDistributions = <|
	"f" -> UniformDistribution[{0, 1}],
	"fieldInitialOverF" -> UniformDistribution[{3 / 4 Pi Sqrt[2], Pi Sqrt[2]}],
	"pivotEfoldings" -> UniformDistribution[{50, 60}]|>;


(* ::Section:: *)
(*Supersymmetry*)


Print["Starting supersymmetry..."];


supersymmetryLagrangian[f_, G_, B_: 1][bm_][t_] := 1/2 D[bm, t]^2 - 4 f^4 B^2 (
	Sum[
		l G[[l]] r G[[r]] (1 - Cos[r / Sqrt[2] bm / f]),
		{l, Length[G]}, {r, Length[G]}
	] - Sum[
		l r G[[l]] G[[r]] (1 - Cos[(r - l) / Sqrt[2] bm / f]),
		{l, Length[G]}, {r, l + 1, Length[G]}
	]
)


bMinusFieldLabel = subscriptLabel[italicLabel["b"], plainLabel["-"]];


evaluateModel[<|
	"name" -> "supersymmetry",
	"lagrangian" -> (supersymmetryLagrangian[#["f"], {0, 0, 0, 1, #["G5"]}] &),
	"initialConditions" -> {{bm, "fieldInitialOverF" "f", 0}},
	"pointCount" -> 20000,
	"evolutionOptions" -> {"FinalDensityPrecisionGoal" -> 6},
	"parameterDistributions" -> Join[genericParameterDistributions, <|
		"G5" -> UniformDistribution[{-0.88931, -0.88920}]|>],
	"figures" -> {
		"ns_r",
		"fieldRange_fStatic_ratio",
		{ListPlot, "f", "fStatic"},
		"fStatic_fDynamic",
		{"potentialRange", -0.1 Pi Sqrt[2], 1.1 Pi Sqrt[2], 0.05},
		{"density", -#["r"] &}},
	"fieldLabels" -> <|bm -> bMinusFieldLabel|>,
	"caption" -> "Simulation results for the global supersymmetry model " <>
		"Eq.~(\\ref{eq:supersymmetry:Vslow}). " <>
		"Simulation consisted of a total of `totalPoints` points, out of which " <>
		"`points` shown are consistent with experimental data on $r$ and $n_s$. " <>
		"Here $q = 5$, $G_1 = G_2 = G_3 = 0$, $G_4 = 1$, " <>
		"$G_5 \\sim <*distribution[\"G5\", {6, 5}]*>$, " <>
		"where $\\mathcal{U}$ refers to a uniform distribution. " <>
		"We set $B = 1$, as it only affects the time scale of inflation, but not " <>
		"the values of $n_s$, $r$, and $f_e$. " <>
		"Finally, $f / M_\\text{P} \\sim <*distribution[\"f\"]*>$, " <>
		"$b_{-, \\text{init}} / f \\sim <*distribution[\"fieldInitialOverF\"]*>$, " <>
		"$N_\\text{pivot} \\sim <*distribution[\"pivotEfoldings\"]*>$."|>];


(* ::Section:: *)
(*Supergravity*)


Print["Starting supergravity..."]


supergravityLagrangian[f_, \[Gamma]_, A_][bm_][t_] := 1/2 D[bm, t]^2 - 4 Exp[2 f^2] Sum[
	Exp[n + m] A[[n]] A[[m]] (
		\[Gamma][[n]] \[Gamma][[m]] / f^2 (
			1
			- Cos[\[Gamma][[n]] bm / (Sqrt[2] f)]
			- Cos[\[Gamma][[m]] bm / (Sqrt[2] f)]
			+ Cos[(\[Gamma][[n]] - \[Gamma][[m]]) bm / (Sqrt[2] f)])
		+ (2 \[Gamma][[n]] + 2 \[Gamma][[m]] - 3 + 4 f^2) (
			1
			- Cos[\[Gamma][[n]] bm / (Sqrt[2] f)]
			- Cos[\[Gamma][[m]] bm / (Sqrt[2] f)]
			+ 1/2 Cos[(\[Gamma][[n]] - \[Gamma][[m]]) bm / (Sqrt[2] f)]
			+ 1/2 Cos[(\[Gamma][[n]] + \[Gamma][[m]]) bm / (Sqrt[2] f)])),
	{n, Length[\[Gamma]]}, {m, Length[\[Gamma]]}]


evaluateModel[<|
	"name" -> "supergravity",
	"lagrangian" ->
		(supergravityLagrangian[#["f"], {1, 2, 3}, {1, #["A2"], #["A3"]}] &),
	"initialConditions" -> {{bm, "fieldInitialOverF" "f", 0}},
	"pointCount" -> 70000,
	"evolutionOptions" -> {"FinalDensityPrecisionGoal" -> 2},
	"parameterDistributions" -> Join[genericParameterDistributions, <|
		"A2" -> UniformDistribution[{0.080, 0.085}],
		"A3" -> UniformDistribution[{0.0030, 0.0037}]|>],
	"figures" -> {
		"ns_r",
		"fieldRange_fStatic_ratio",
		{ListPlot, "f", "fStatic"},
		"fStatic_fDynamic",
		{"potentialRange", -0.1 Pi Sqrt[2], 1.1 Pi Sqrt[2], 0.05},
		{"density", -#["r"] &}},
	"fieldLabels" -> <|bm -> bMinusFieldLabel|>,
	"caption" -> "Simulation results for the supergravity model " <>
		"Eq.~(\\ref{eq:supergravity:Vslow3}). " <>
		"Simulation consisted of a total of `totalPoints` points, out of which " <>
		"`points` shown are consistent with experimental data on $r$ and $n_s$. " <>
		"Here $A_1 = 1$, " <>
		"$A_2 \\sim <*distribution[\"A2\", {4, 3}]*>$, " <>
		"$A_3 \\sim <*distribution[\"A3\", {5, 4}]*>$, " <>
		"$f / M_\\text{P} \\sim <*distribution[\"f\"]*>$, " <>
		"$b_{-, \\text{init}} / f \\sim <*distribution[\"fieldInitialOverF\"]*>$, " <>
		"$N_\\text{pivot} \\sim <*distribution[\"pivotEfoldings\"]*>$."|>];


(* ::Section:: *)
(*DBI*)


Print["Starting DBI..."];


Print["DBI: Reading simulation results..."];


dbiData = Import["data/dbi.wxf"];
dbiFiltered = Join[
		#,
		<|"consistentQ" -> ExperimentallyConsistentInflationQ[#["ns"], #["r"]]|>] & /@
	Select[
		#["f"] <= 1
			&& #["TotalEfoldingsCount"] - #["PivotEfoldingsCount"] >= 5 &] @ dbiData;


Print["DBI: Computing effective axion decay constant..."];


dbiFilteredWithFDynamic = Join[#, <|"fDynamic" -> 1 / Sqrt[
	1 - #["ns"] - #["r"] / 4]|>] & /@ dbiFiltered;


Print["DBI: generating figures..."];


aMinusFieldLabel = subscriptLabel[italicLabel["a"], plainLabel["-"]];


dbiSpecs = <|
	"name" -> "DBI",
	"pointCount" -> Length[dbiFilteredWithFDynamic],
	"figures" -> {
		"ns_r",
		{ListLogLogPlot, "Alpha1", "NonGaussianityAmplitude"},
		{ListPlot, "f", "fDynamic"}},
	"fieldLabels" -> <|bm -> aMinusFieldLabel|>,
	"caption" -> "Simulation results for the DBI model " <>
		"Eq.~(\\ref{eq:dbi:lagrangian}). " <>
		"Simulation consisted of the same $0.5 \\times 10^6$ points as " <>
		"Fig.~(1) of~\\cite{Nath:2018xxe}, " <>
		"out of which `totalPoints` " <>
		"displayed are consistent with experimental data on $r$ and $n_s$. " <>
		"Points are distributed according to the following: " <>
		"$\\alpha_1 \\sim X_{0, \\infty}$, $\\alpha_2 = \\alpha_3 = 0$ " <>
		"$T = 10^{-12} M_\\text{P}^4$, " <>
		"$f \\sim X_{0, \\infty} M_\\text{P}$, " <>
		"$\\tilde\\beta \\sim X_{0, \\infty}$, " <>
		"$m = 6$, " <>
		"${\\cal{G}}_1 = {\\cal{G}}_2 = {\\cal{G}}_3 = 0$, ${\\cal{G}}_4 = 1$, " <>
		"${\\cal{G}}_5 \\sim {\\cal{G}}_6 \\sim " <>
		"\\mathcal{U}\\left\\{-1, 1\\right\\} \\times X_{0, \\infty}$, " <>
		"$N_{\\rm pivot} \\sim \\mathcal{U}\\left(50, 60\\right)$, " <>
		"$a_{-, 0} \\sim \\mathcal{U}\\left(0, 2\\pi\\right) \\times f$, " <>
		"$\\dot a_{-, 0} \\sim \\mathcal{U}\\left(-1, 1\\right) " <>
		"\\times \\left(2 \\sqrt{T}/\\left(\\sqrt{2} \\sqrt{\\alpha_1} + " <>
		"2\\right)^{1/2}\\right)$ " <>
		"where $X_{0, \\infty} = \\mathcal{U}\\left\\{X_{0, 1}, " <>
		"1 / X_{0, 1}\\right\\}$, " <>
		"$X_{0, 1} = \\mathcal{U}\\left(0, 1\\right)$, " <>
		"$\\mathcal{U}$ refers to a uniform distribution, " <>
		"and the distribution of $\\dot a_{-, 0}$ is chosen such that " <>
		"the expression under the square root in Eq.~(\\ref{eq:dbi:lagrangian}) " <>
		"is positive. " <>
		"Points are further filtered such that " <>
		"the mass of the inflaton $m_{a_-} < 0.1 M_\\text{P}$. " <>
		"Note that despite no fine-tuning being present in the distribution above, " <>
		"a significant fraction of points is consistent with " <>
		"experimental constraints."|>;


makeFigure[dbiSpecs, #, dbiFilteredWithFDynamic] & /@ dbiSpecs[["figures"]];


Print["DBI: generating captions..."]


makeCaption[dbiSpecs, dbiFilteredWithFDynamic];


Print["DBI: done."];


(* ::Section:: *)
(*Evaluation*)


Print["All done."];


(* ::Section:: *)
(*Deinitialization*)


CloseKernels[];


Exit[];
