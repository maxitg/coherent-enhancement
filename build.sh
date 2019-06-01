#!/bin/bash

usage() { cat << EOF
usage: ./build.sh [-ah][-p <pointCountMultiplier>]

  a : make .tar.gz suitable for arXiv upload
  h : print this usage message

  p multiplier : produce the paper with p times the normal number of points
                 on scatter plots
EOF
exit 1;
}

pointCountMultiplier=1.0
while getopts "p:ah" option; do
  case "${option}" in
    p)
      pointCountMultiplier=${OPTARG};;
    a)
      createArxivFile=true;;
    *)
      usage;;
  esac
done

if [ "$createArxivFile" = true ] ; then
  rm -rf figures
fi

./computeSimulations.wls $pointCountMultiplier &&
pdflatex coherent-enhancement.tex &&
pdflatex coherent-enhancement.tex &&
pdflatex coherent-enhancement.tex

if [ "$createArxivFile" = true ] ; then
  tar -cvzf coherent-enhancement-arxiv.tar.gz coherent-enhancement.tex figures
fi
