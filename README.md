# Enhancement of the Axion Decay Constant in Inflation and the Weak Gravity Conjecture
Models of axion inflation based on a single cosine potential require the axion decay constant `$f$` to be super-Planckian in size. However, `$f > M_{Pl}$` is disfavored by the Weak Gravity Conjecture (WGC). It is then pertinent to ask if one can construct axion inflation models in conformity with WGC. In this work we assume that WGC holds for the microscopic Lagrangian so that `$f < M_{Pl}$`. However, inflation is controlled by an effective Lagrangian much below the Planck scale where the inflaton is an effective axionic field associated with an effective decay constant `$f_e$` which could be very different from `$f$`. In this work we propose a Coherent Enhancement Mechanism (CEM) for slow roll inflation controlled by flat potentials which can produce `$f_e \gg M_{Pl}$` while `$f < M_{Pl}$`. In the analysis we consider a landscape of chiral fields charged under a `$U\left(1\right)$` global shift symmetry and consider breaking of the `$U\left(1\right)$` symmetry by instanton type symmetry breaking terms. In the broken phase there is one light pseudo-Nambu-Goldstone-Boson (pNGB) which acts as the inflaton. We show that with an appropriate choice of symmetry breaking terms the inflaton potential is a superposition of many cosines and the condition that they produce a flat potential allows one to enhance `$f_e$` so that `$f_e / M_{Pl} \gg 1$`. We discuss the utility of this mechanism for a variety of inflaton models originating in supersymmetry and supergravity. The Coherent Enhancement Mechanism allows one to reduce an inflation model with an arbitrary potential to an effective model of natural inflation, i.e. with a single cosine, by expanding the potential near a field point where horizon exit occurs, and matching the expansion coefficients to those of natural inflation. We demonstrate that this approach can predict the number of e-foldings in a given inflation model without the need for numerical simulation. Further we show that the effective decay constant `$f_e$` can be directly related to the spectral indices so that `$f_e = M_{Pl} / \sqrt{1 - n_s - r / 4}$` where `$n_s$` is the spectral index for curvature perturbations and `$r$` is the ratio of the power spectrum of tensor perturbations and curvature perturbations. The current data on `$n_s$` and `$r$` constrains the effective axion decay constant so that `$4.9 \leq f_e / M_{Pl} \leq 10.0$` at `$95\%$` CL. Thus an important result of the analysis is that the effective axion decay constant has an upper limit of `$\sim 10 M_{Pl}$` in axion cosmology for any potential-based model which produces successful inflation. For the Dirac-Born-Infeld inflation and more generally k-flation CEM is not applicable. Nonetheless in this case also we show that successful inflation can occur with `$f < M_{Pl}$`. Further, one can define slow-roll parameters as well as the effective axion decay constant in terms of inflaton density which is valid both for models using potentials as well as DBI-flation and more generally k-flation. In the models considered in this work, all the moduli are stabilized and the inflation model in each case is consistent with astrophysical observations with `$f_e > M_{Pl}$` and the axion decay constant of the microscopic theory `$f < M_{Pl}$` consistent with the Weak Gravity Conjecture. In conclusion, among the models we considered those with flat potentials and consistent with WGC have `$r < O\left(10^{-3}\right)$`, and the only single field model we considered consistent with WGC and `$r$` as large as the experimental limit `$r = 0.07$` is the DBI model.

## Build Prerequisites

* LaTeX distrubution including `tlmgr`;
* LaTeX packages that can be installed by running `provision.sh`;
* [Wolfram Mathematica 12.0+](http://www.wolfram.com/mathematica/?source=nav) including Wolfram Script;
* [Inflation Simulator v0.3.1](https://github.com/maxitg/InflationSimulator/releases/tag/0.3.1);
* [Latin Modern Roman fonts](http://www.gust.org.pl/projects/e-foundry/latin-modern).

## Build Steps

1. `cd` to the root directory, i.e., directory containing `build.sh`.
2. Run `./build.sh`. That will call `computeSimulations.wls`, that will use Wolfram Script to generate figures, and then build the paper with `pdflatex`. Note generating figures takes a while.
