#' A Function to Classify continuous SBP and DBP outcomes as categorical hypertension
#'
#' This function allows you to create new categorical hypertension
#' variables in your dataset based on old and new AHA guidelines.
#'
#' OLD AHA BP CUTOFFS:
#'    Normotensive: SBP <120 mm Hg AND DBP <80 mm Hg
#'    Pre-hypertensive: SBP 120-139 mm Hg OR DBP 80-89 mm Hg
#'    Hypertensive: SBP ≥140 mm Hg OR DBP ≥90 mm Hg
#'
#' NEW AHA BP CUTOFFS:
#'    Normotensive: SBP <120 mm Hg AND DBP <80 mm Hg
#'    Pre-hypertensive/Elevated: SBP 120-129 mm Hg AND DBP <80 mm Hg
#'    Hypertensive: SBP ≥130 mm Hg OR DBP ≥80 mm Hg

#'
#'
#' @param data_entry Dataframe input that includes the variables SBP_meas, DBP_meas, and antihtn_use -0 or 1-
#'
#' @return Will return a new dataframe with 7 new variables:
#'    \item{htn_aha_old}{Three categories (normotensive = 0,
#'    pre-hypertensive = 1, hypertensive = 2) of hypertension based
#'    on old AHA guidelines}
#'    \item{htn_aha_new}{Three categories (normotensive = 0,
#'    pre-hypertensive = 1, hypertensive = 2) of hypertension based
#'    on new AHA guidelines}
#'    \item{htn_aha_old_bi}{Two categories (non-hypertensive = 0,
#'    hypertensive = 1) of hypertension based on old AHA guidelines,
#'    grouping normo- and pre-hypertensive}
#'    \item{htn_aha_new_bi}{Two categories (non-hypertensive = 0,
#'    hypertensive = 1) of hypertension based on new AHA guidelines,
#'    grouping normo- and pre-hypertensive}
#'    \item{SBP_meas_adj}{New SBP outcome variable adjusting +10 for anti-hypertensive use}
#'    \item{DBP_meas_adj}{New DBP outcome variable adjusting +5 for anti-hypertensive use}
#'    \item{ancestry}{New binary variable for european (0) and african (1) ancestry (combines African and African American)}
#'
#' @keywords SBP, DBP, hypertension, classification
#'
#' @export
#' @examples
#' classed <- htn_outcome_classify(merged)

htn_outcome_classify <- function(data_entry){
  merged_class <- mutate(data_entry,
                         htn_aha_old =
                           ifelse((SBP_meas<140 | DBP_meas<90) & antihtn_use !=1, 1,
                                  ifelse(SBP_meas<120 & DBP_meas<80, 0,
                                         ifelse(SBP_meas>=140 | DBP_meas>=90 | antihtn_use== 1, 2, NA))),
                         htn_aha_new =
                           ifelse(SBP_meas<130 & DBP_meas<80 & antihtn_use !=1, 1,
                                  ifelse(SBP_meas<120 & DBP_meas<80, 0,
                                         ifelse(SBP_meas>=130 | DBP_meas>=80 | antihtn_use==1, 2, NA))),
                         htn_aha_old_bi =
                           ifelse((SBP_meas<140 | DBP_meas<90) & antihtn_use != 1, 0,
                                  ifelse(SBP_meas>=140 | DBP_meas>=90 | antihtn_use==1, 1, NA)),
                         htn_aha_new_bi =
                           ifelse(SBP_meas<130 & DBP_meas<80 & antihtn_use != 1, 0,
                                  ifelse((SBP_meas>=130 | DBP_meas>=80) | antihtn_use==1, 1, NA)),
                         SBP_meas_adj =
                           ifelse(antihtn_use == 1, SBP_meas + 10, NA),
                         DBP_meas_adj =
                           ifelse(antihtn_use ==1, DBP_meas + 5, NA),
                         ancestry =
                           ifelse(bestpop == "eur", 0,
                                  ifelse(bestpop == "afr" | bestpop == "aam", 1, NA)))
  return(merged_class)
}
