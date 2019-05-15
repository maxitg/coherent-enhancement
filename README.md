# Coherent Enhancement of the Decay Constant in Axionic Inflation
Models of axion inflation based on a single cosine potential require the axion decay constant to be super Planckian in size. However, a super Plankian axion decay constant is disfavored in quantum gravity and in strings. Here we propose a coherent enhancement mechanism which can produce an effective axion decay which is super Planckian even when the true axion decay constant is sub Planckian. We discuss the utility of the coherent enhancement mechanism for a variety of axion potentials originating in supersymmetry, supergravity and strings. The coherent enhancement mechanism allows one to reduce an inflation model with an arbitrary potential to an effective model of natural inflation, i.e. with a single cosine, by expanding the potential near an inflationary point, and matching the expansion coefficients to those of natural inflation. We demonstrate that this approach can predict the number of e-foldings in a given inflation model without the need for numerical simulation. Further we show that the effective decay constant can be directly related to the spectral indices. We consider briefly the application of the mechanism to DBI type models.

## Build Prerequisites

* LaTeX distrubution including `tlmgr`;
* LaTeX packages that can be installed by running `provision.sh`;
* [Wolfram Mathematica 12.0+](http://www.wolfram.com/mathematica/?source=nav) including Wolfram Script;
* [Latin Modern Roman fonts](http://www.gust.org.pl/projects/e-foundry/latin-modern).

## Build Steps

1. `cd` to the root directory, i.e., directory containing `build.sh`.
2. Run `./build.sh`. That will call `computeSimulations.wls`, that will use Wolfram Script to generate figures, and then build the paper with `pdflatex`. Note generating figures takes a while.
