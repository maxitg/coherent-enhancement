#!/bin/bash

pointCountMultiplier=1.0
while getopts p: option
do
  case "${option}"
    in
    p) pointCountMultiplier=${OPTARG};;
  esac
done

./computeSimulations.wls $pointCountMultiplier &&
pdflatex coherent-enhancement.tex &&
pdflatex coherent-enhancement.tex &&
pdflatex coherent-enhancement.tex
