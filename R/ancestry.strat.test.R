#' A function to test if there is a difference between old and new AHA classifications, stratified by ancestry
#'
#' This function allows you to test two- (non-hypertensive vs. hypertensive)
#' and three-level hypertension (normotensive, pre-hypertensive, hypertensive)
#' classifications to see if there is a significant difference in classification
#' based on old and new AHA guidelines.
#' The tests are stratified by ancestry. 
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
#' @param data_entry Dataframe input that includes the variables SBP_meas, DBP_meas, 
#' and hypertension variables created using the function "htn_outcome_classify"
#'
#' @return Will return a new list with 5 objects:
#'    \item{testSummary}{Summary table of P-values for all 4 tests}
#'    \item{stratSumAfr}{Contingency table of old vs new AHA 3-level in African ancestry}
#'    \item{stratSumEur}{Contingency table of old vs new AHA 3-level in European ancestry}
#'    \item{stratSumAfrBi}{Contingency table of old vs new AHA 2-level in African ancestry}
#'    \item{stratSumEurBi}{Contingency table of old vs new AHA 2-level in European ancestry}
#'    
#' @keywords ancestry stratification, hypertension classification, Fisher's exact
#'
#' @export
#' @examples
#' STUDYPI_ancestry_strat_test <- ancestry.strat.test(classed)



ancestry.strat.test <- function(data_entry){
  stratSumEur <- table(data_entry[index_eur,]$htn_aha_new, data_entry[index_eur,]$htn_aha_old) #Used index to make contingency table of data_entry data
  eurTest <- fisher.test(stratSumEur, simulate.p.value = TRUE, B=1e5) #test on stratified data, comparing new to old classes of hypertension
  
  stratSumAfr<- table(data_entry[index_afr,]$htn_aha_new, data_entry[index_afr,]$htn_aha_old) #Used index to make contingency table of data_entry data
  afrTest <- fisher.test(stratSumAfr, simulate.p.value = TRUE, B=1e5) #test on stratified data, comparing new to old classes of hypertension
  
  #Now tests below after collapsing normotensive and pre-hypertensive (in case there were not enough samples in pre-hypertensive)
  stratSumEurBi<- table(data_entry[index_eur,]$htn_aha_new_bi, data_entry[index_eur,]$htn_aha_old_bi) #Used index to make contingency table of data_entry data
  #Used index to make contingency table of data_entry data
  eurTestBi <- fisher.test(stratSumEurBi, simulate.p.value = TRUE, B=1e5) #test on stratified data, comparing new to old classes of hypertension
  
  stratSumAfrBi<- table(data_entry[index_afr,]$htn_aha_new_bi, data_entry[index_afr,]$htn_aha_old_bi)
  afrTestBi <- fisher.test(stratSumAfrBi, simulate.p.value = TRUE, B=1e5)
  
  #Make summary table; save output#
  testSummary <- data.frame(Fisher_PValue = c(eurTest$p.value, afrTest$p.value, eurTestBi$p.value, afrTestBi$p.value),
                            row.names = c("European 3-class", "African 3-class", "European 2-class", "African 2-class"))
  newList <- list(testSummary = testSummary, stratSumAfr = stratSumAfr, stratSumEur = stratSumEur, stratSumAfrBi = stratSumAfrBi, stratSumEurBi = stratSumEurBi)
  return(newList)
}
