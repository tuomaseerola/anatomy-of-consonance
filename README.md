
<!-- README.md is generated from README.Rmd. Please edit that file -->

# anatomy-of-consonance

<!-- badges: start -->
<!-- badges: end -->

This repository contains data and analyses related to study titled *The
Anatomy of Consonance/Dissonance: Evaluating Acoustic and Cultural
Predictors Across Multiple Datasets with Chords* by Tuomas Eerola and
Imre Lahdelma (Durham University, UK).

The study is organised into three experiments. Experiment 1 relates to
(Durham Chord Dataset)\[<https://github.com/tuomaseerola/DCD>\],
Experiment 2 to the analysis of three datasets, and Experiment 3 to the
analysis of nine datasets.

## Experiment 1

``` r
#source('compile_DCD_predictors.R')
```

## Analysis files

-   `exp2_alternative_analysis_fixed_factors.R` : Alternative analysis
    where chord size is included as a fixed factor (and we drop datasets
    entirely, it does not seem to be doing much), chord size is
    consistently a significant predictor in all models with a small
    positive coefficient.

-   `permutated_analysis.R` : A control analyses with all 24
    permutations of the predictor category orders (e.g. starting with
    “Roughness”, “Harmonicity”,“Familiarity”, and “Spectral Envelope” as
    reported and moving onto “Roughness”, “Harmonicity”, “Spectral
    Envelope”, and “Familiarity”, until all 24 permutations had gone
    through with the same type of analysis where all candidates are
    tested and the strongest one is taken forward to represent that
    category). The outcome of this analysis did not change in any
    iteration, so the same predictors won the comparison despite their
    order in the sequence. However, the strength of the predictor
    improvement did vary across the orders.
