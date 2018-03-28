#' A Function to Classify continuous SBP and DBP outcomes as categorical hypertension
#'
#' This function allows you to create new categorical hypertension variables in your dataset based on old and new AHA guidelines
#' @param data_entry Dataframe input that includes the variables SBP_meas, DBP_meas, and antihtn_use -0 or 1-
#' @keywords SBP, DBP, hypertension, classification
#' @export
#' @examples
#' htn_outcome_classify(merged)

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
