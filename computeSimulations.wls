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


simulate[lagrangian_, inputs_, options_] := Module[{hash, filename, results, counter},
	hash = Hash[{DownValues[simulate], lagrangian, inputs}];
	filename = FileNameJoin[{cacheDirectory, IntegerString[hash, 16] <> ".wxf"}];
	If[FileExistsQ[filename],
		Print["Using cached simulation results"];
		Import[filename, "WXF"],
		counter = 0;
		SetSharedVariable[counter];
		SetSharedFunction[Print];
		SetOptions[$Output, FormatType -> OutputForm];
		results = ParallelMap[Module[{result, c},
			result = If[
				ExperimentallyConsistentInflationQ[
					lagrangian[#][bm[t], t],
					{bm[t], #["f"] #["fieldInitialOverF"], 0},
					t,
					#["pivotEfoldings"],
					options[#]] &&
				InflationEfoldingsCount[
					lagrangian[#][bm[t], t],
					{bm[t], #["f"] #["fieldInitialOverF"], 0},
					t,
					options[#]] >=
							#["pivotEfoldings"] + minSubhorizonEfoldings,
				Join[
					#,
					Association @ Thread[
						{"fDynamic", "fStatic", "r", "exitField", "endField"} ->
							InflationValue[
								lagrangian[#][bm[t], t],
								{bm[t], #["f"] #["fieldInitialOverF"], 0},
								t,
								#["pivotEfoldings"],
								{InflationProperty[
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
									Method -> "FromHubbleParameter"],
								 InflationProperty[
									"Field",
									"HorizonExit"],
								 InflationProperty[
									"Field",
									"End"]},
								options[#]]]],
				Nothing];
				c = ++counter;
				If[Quotient[100 c, Length[inputs]] >
						Quotient[100 (c - 1), Length[inputs]],
					Print[Quotient[100 c, Length[inputs]], "% done..."]];
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


$label["r"] = italicLabel["r"];
$label["f"] = ratioLabel[italicLabel["f"], planckMassLabel];
$label["fStaticUnits"] = subscriptLabel[italicLabel["f"], italicLabel["e"]];
$label["fStatic"] = ratioLabel[$label["fStaticUnits"], planckMassLabel];
$label["fDynamic"] =
	ratioLabel[subscriptLabel[italicLabel["f"], italicLabel["eH"]], planckMassLabel];
$label["fieldRange", field_] := rowLabel[{italicLabel["\[CapitalDelta]"], field}];
$label["fieldRangeUnitless", field_] :=
	ratioLabel[$label["fieldRange", field], planckMassLabel];
$label["pivotEfoldings"] = subscriptLabel[italicLabel["N"], plainLabel["pivot"]];


figureName[name_String] := name <> ".pdf"
figureName[{f_, p1_String, p2_String, args___}] := p1 <> "_" <> p2 <> ".pdf"
figureName[{name_String, args___}] := name <> ".pdf"


makeFigure[specs_, name_, output_, OptionsPattern[]] :=
	Export[
		FileNameJoin[{
			figuresDirectory,
			specs["name"] <> "_" <> figureName[name]}],
		Show[
			plot[name, specs, output],
			ImageSize -> OptionValue["imageSize"],
			LabelStyle -> Directive[FontFamily -> OptionValue["fontFamily"]]]]


plot[{"potentialRange", min_, max_, step_}, specs_, output_] := ListPlot[
	ParallelTable[{bf, #[
		Evaluate[With[{f = #[["f"]]},
			specs["lagrangian"][#][f bf, t] /
				specs["lagrangian"][#][f \[Pi] Sqrt[2], t]] & /@ output]]},
	{bf, min, max, step}] & /@ {Min, Max},
	Axes -> False,
	Frame -> True,
	FrameLabel -> {
		label[ratioLabel[specs["fieldLabel"], italicLabel["f"]]],
		label[$label["fieldRangeUnitless", specs["fieldLabel"]]]},
	PlotStyle -> ColorData[97, 1],
	Joined -> True,
	Filling -> {1 -> {2}},
	PlotRange -> All]


plot[{"potential", parameters_, min_, max_}, specs_, output_] := Plot[
	-specs["lagrangian"][parameters][b, t], {b, min, max},
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> {
		label[ratioLabel[specs["fieldLabel"], planckMassLabel]],
		label[ratioLabel[
			italicLabel["V"], subscriptLabel[italicLabel["V"], plainLabel["0"]]]]}]


plot[{func_, p1_, p2_}, specs_, output_] := func[
	output[[All, {p1, p2}]],
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> label /@ {p1, p2}]


plot["fStatic_fDynamic", specs_, output_] := With[
	{pointsPlot = plot[{ListPlot, "fStatic", "fDynamic"}, specs["lagrangian"], output]},
	Show[
		Plot[
			x,
			{x, 0, 10},
			PlotStyle -> Black,
			Frame -> True,
			FrameLabel -> {label["fStatic"], label["fDynamic"]},
			Evaluate @ AbsoluteOptions[pointsPlot, PlotRange]],
		pointsPlot]]


plot["fieldRange_fStatic_ratio", specs_, output_] := ListPlot[
	Transpose[{
		Abs[output[[All, "exitField"]] - output[[All, "endField"]]] /
			output[[All, "fStatic"]],
		Abs[output[[All, "exitField"]] - output[[All, "endField"]]]}],
	PlotRange -> All,
	Frame -> True,
	FrameLabel -> {
		label[ratioLabel[
			$label["fieldRange", specs["fieldLabel"]], $label["fStaticUnits"]]],
		label[$label["fieldRangeUnitless", specs["fieldLabel"]]]}]


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
			<|"totalPoints" -> specs["pointCount"], "points" -> Length @ results|>]]


(* ::Subsection:: *)
(*Evaluator*)


evaluateModel[modelSpecs_] := Module[{inputs, results},
	Print[modelSpecs["name"] <> ": generating inputs..."];
	inputs = generateInputs[
		modelSpecs["parameterDistributions"],
		"NP.coherent." <> modelSpecs["name"],
		Round[modelSpecs["pointCount"] pointCountMultiplier]];
	Print[modelSpecs["name"] <> ": simulating inflation..."];
	results = simulate[
		modelSpecs["lagrangian"], inputs, Lookup[modelSpecs, "evolutionOptions", {} &]];
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


supersymmetryLagrangian[f_, G_, B_: 1][bm_, t_] := 1/2 D[bm, t]^2 - 4 f^4 B^2 (
	Sum[
		l G[[l]] r G[[r]] (1 - Cos[r / Sqrt[2] bm / f]),
		{l, Length[G]}, {r, Length[G]}
	] - Sum[
		l r G[[l]] G[[r]] (1 - Cos[(r - l) / Sqrt[2] bm / f]),
		{l, Length[G]}, {r, l + 1, Length[G]}
	]
)


bMinusFieldLabel = subscriptLabel[italicLabel["b"], plainLabel["-"]]


supersymmetrySpecs = <|
	"name" -> "supersymmetry",
	"lagrangian" -> (supersymmetryLagrangian[#["f"], {0, 0, 0, 1, #["G5"]}] &),
	"pointCount" -> 20000,
	"parameterDistributions" -> Join[genericParameterDistributions, <|
		"G5" -> UniformDistribution[{-0.88931, -0.88920}]|>],
	"figures" -> {
		{ListPlot, "f", "r"},
		"fieldRange_fStatic_ratio",
		{ListPlot, "f", "fStatic"},
		"fStatic_fDynamic",
		{"potentialRange", -0.1 Pi Sqrt[2], 1.1 Pi Sqrt[2], 0.05}},
	"fieldLabel" -> bMinusFieldLabel,
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
		"$N_\\text{pivot} \\sim <*distribution[\"pivotEfoldings\"]*>$."|>;


(* ::Section:: *)
(*Supergravity*)


supergravityLagrangian[f_, \[Gamma]_, A_][bm_, t_] := 1/2 D[bm, t]^2 - 4 Exp[2 f^2] Sum[
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


supergravitySpecs = <|
	"name" -> "supergravity",
	"lagrangian" ->
		(supergravityLagrangian[#["f"], {1, 2, 3}, {1, #["A2"], #["A3"]}] &),
	"pointCount" -> 70000,
	"parameterDistributions" -> Join[genericParameterDistributions, <|
		"A2" -> UniformDistribution[{0.080, 0.085}],
		"A3" -> UniformDistribution[{0.0030, 0.0037}]|>],
	"figures" -> {
		{ListPlot, "f", "r"},
		"fieldRange_fStatic_ratio",
		{ListPlot, "f", "fStatic"},
		"fStatic_fDynamic",
		{"potentialRange", -0.1 Pi Sqrt[2], 1.1 Pi Sqrt[2], 0.05}},
	"fieldLabel" -> bMinusFieldLabel,
	"caption" -> "Simulation results for the supergravity model " <>
		"Eq.~(\\ref{eq:supergravity:Vslow3}). " <>
		"Simulation consisted of a total of `totalPoints` points, out of which " <>
		"`points` shown are consistent with experimental data on $r$ and $n_s$. " <>
		"Here $A_1 = 1$, " <>
		"$A_2 \\sim <*distribution[\"A2\", {4, 3}]*>$, " <>
		"$A_3 \\sim <*distribution[\"A3\", {5, 4}]*>$, " <>
		"$f / M_\\text{P} \\sim <*distribution[\"f\"]*>$, " <>
		"$b_{-, \\text{init}} / f \\sim <*distribution[\"fieldInitialOverF\"]*>$, " <>
		"$N_\\text{pivot} \\sim <*distribution[\"pivotEfoldings\"]*>$."|>;


(* ::Section:: *)
(*Evaluation*)


evaluateModel /@ {supersymmetrySpecs, supergravitySpecs};


Print["All done."];


(* ::Section:: *)
(*Deinitialization*)


CloseKernels[];


Exit[];
