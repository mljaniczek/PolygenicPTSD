#' A Function to grab PRS and PTSD variables from the dataset
#'
#' This function allows you to create new categorical hypertension variables in your dataset based on old and new AHA guidelines
#' @param data_entry Dataframe input that includes the variables for SBP PRS (starting with "sbp_prs"), DBP PRS (starting with "dbp_prs") and Hypertension PRS (starting with "bp_prs")
#' @keywords SBP, DBP, PRS
#' @export
#' @examples
#' prsVars(classed)

prsVars <- function(data_entry){
  sbp_prs_vars <- colnames(data_entry)[grepl("sbp_prs", colnames(data_entry))] #selecting column names for sbp PRS
  dbp_prs_vars <- colnames(data_entry)[grepl("dbp_prs", colnames(data_entry))] #selecting column names for dbp PRS
  htn_prs_vars <- colnames(data_entry)[grepl("^bp_prs", colnames(data_entry))]
  ptsd_vars <- colnames(data_entry[grepl("ptsd", colnames(data_entry))]) #selecting the four ptsd variable names
  return(list(sbp_prs_vars = sbp_prs_vars, dbp_prs_vars = dbp_prs_vars, htn_prs_vars = htn_prs_vars, ptsd_vars = ptsd_vars))
}