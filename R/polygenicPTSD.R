#' polygenicPTSD: A package for examining interaction between PTSD and Polygenic Risk Scores in predicting blood pressure outcomes
#'
#' The polygenicPTSD package provides three categories of important functions:
#' classification, stratification, and analysis.
#'
#' @section Classification functions:
#' The classification functions use imported continuous BP data and classifies
#' categorical hypertension outcomes based on AHA guidelines (both old and new).
#' ...
#'
#' @section Stratification functions:
#' Stratifies data based on ancestry (European and African, based on availability
#' of ancestry-specific PRS) and performs Fishers exact test
#' to determine if dataset has significant difference between old and new
#' AHA hypertension guidelines within ancestry.
#'
#' @section Analysis functions:
#' Runs linear regression for continuous outcomes (SBP and DBP)
#' and multinomial regression for categorical hypertension outcome,
#' using base model which includes covariates for age, sex, and first three
#' ancestry principle components, along with PTSD and PRS variables.
#'
#'
#'
#'
#' @docType package
#' @name polygenicPTSD
NULL
