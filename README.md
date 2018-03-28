
<!-- README.md is generated from README.Rmd. Please edit that file -->
polygenicPTSD
=============

The goal of polygenicPTSD is to make an easy-to-use package for multi-center sites in the PGC-PTSD working group to conduct analysis on the interaction between PTSD and BP Polygenic Risk Score (PRS) in predicting various BP outcomes. It includes functions to ensure data has appropriate variables required for analysis, creates new variables which categorizes continuous BP into categorical hypertension outcomes based on old and new AHA criteria, stratifies data by ancestry, runs linear regressions on continuous BP outcomes and generalized linear models on categorical hypertension outcomes, and more.

Installation
------------

You can install polygenicPTSD from github with:

``` r
# install.packages("devtools")
devtools::install_github("margarethannum/PolygenicPTSD")
```

Example
-------

This is a basic example which shows you how to run the analysis pipeline on simulated data:

``` r
## Load example simulated data
#data("merged_example")

## Create dataframe with 7 new categorical variables for hypertension 
## classification, continuous BP adjustment for antihypertension use, 
## and ancestry (necessary for analysis)
#classed <- htn_outcome_classify(merged_example)

## Stratify based on ancestry and test to see if there is a 
## significant difference between old vs new AHA hypertension 
## classifications within ancestry; saves results in working directory

#First set working directory
#setwd("~/results/")
#Then run function
#


## Run linear regression analyses on continuous outcomes 
## (SBP and DBP); saves results in working directory
#polygenicPTSD_contin(classed)

## Run multinomial regressions on categorical hypertension outcome 
## (tri-level: normotensive, pre-hypertensive, hypertensive); 
## saves results in working directory
#polygenicPTSD_multinom(classed)

## Run logistic regressions on binary categorical hypertension outcome 
## (non-hypertensive vs hypertensive); saves results in working directory
#polygenicPTSD_logit(classed)
```
