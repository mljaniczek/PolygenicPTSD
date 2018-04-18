
<!-- README.md is generated from README.Rmd. Please edit that file -->
polygenicPTSD
=============

The goal of polygenicPTSD is to make an easy-to-use package for multi-center sites in the PGC-PTSD Physical Health Workgroup to conduct analysis on the interaction between PTSD and BP Polygenic Risk Score (PRS) in predicting various BP outcomes. It includes functions to ensure data has appropriate variables required for analysis, creates new variables which categorizes continuous BP into categorical hypertension outcomes based on old and new AHA criteria, stratifies data by ancestry, runs linear regressions on continuous BP outcomes and generalized linear models on categorical hypertension outcomes, and more.

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
#Load package and the packages it depends on (dplyr and nnet)
#install.packages("nnet", "dplyr") #if you need to install
library(polygenicPTSD)
library(dplyr)
library(nnet)

## STEP 1a Load and merge
## Load and merge data (or see example simulated data included with our 
## package)

data("merged_example")

## Replace STUDYPI below with last name of Principle Investigator (For example "Sumner")
study <- "StudyPI"

## STEP 1b Categorization
## Create dataframe with 7 new categorical variables for hypertension 
## classification, continuous BP adjustment for antihypertension use, 
## and ancestry (necessary for analysis)

classed <- htn_outcome_classify(merged_example)

## Create ancestry stratification indices

index_eur = grepl(0, classed$ancestry)
index_afr = grepl(1, classed$ancestry)

## STEP 1C Testing
#Save summary statistics, stratified by ancestry
variable.summary.stratified(classed)

## Fisher's test on old vs new AHA guideline classifications stratified by ancestry
x = paste(study, "ancestry_strat_test", sep = "_") #creates study-specific name using input from earlier in script 
assign(x, ancestry.strat.test(classed), envir = .GlobalEnv) #uses custom function to make contingency tables of AHA old vs new, stratified by ancestry, and tests those tables with Fisher's exact
save(list = x, file=paste(x, ".RData", sep="_")) #Saves test outputs and contingency tables as an R list

## Fisher's test on old vs new AHA guideline classifications 
## stratified by ancestry

## Below functions all save output in working directory.
## We recommend you create a folder "results" and change your working directory to that location.

setwd("~/results/")

######################################################################
#Step 2: Find best PRS 
######################################################################
prs_vars <- prsVars(classed) #makes object of PRS variables, divided by type of outcome
#Function to save R^2 of all PRS to be added **


######################################################################
#Step 3: Test outcomes using functions from polygenicPTSD
######################################################################
#Each of the below uses functions from the polygenicPTSD package to run the required models for the continous and categorical outcomes.

#Linear regression analysis for continuous outcomes (SBP & DBP)
polygenicPTSD_contin(classed)

#Logistic regression analysis for binary hypertension outcomes (non-hypertensive & hypertensive)
polygenicPTSD_logit(classed)

#Multinomial regression analysis for 3-level hypertension outcomes (normotensive, pre-hypertensive & hypertensive)
polygenicPTSD_multinom(classed)


## For any questions on documentation refer to full pipeline document 
## and/or type: ?function to see help documentation

?merged_example
?polygenicPTSD_logit
```
