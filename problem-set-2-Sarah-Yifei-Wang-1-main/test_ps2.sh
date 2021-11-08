#!/bin/bash

# Run the test  files
Rscript -e "knitr::purl('./ps2_answers.Rmd')"
Rscript -e "testthat::test_file('test_ps2_answers.R')" >> test_results.md

cat test_results.md
