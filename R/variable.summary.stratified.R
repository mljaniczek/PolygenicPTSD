#' A function to save summary statistics to working directory
#'
#' This function allows you to easily save summary/descriptive statistics of variables
#' stratified by ancestry  
#'   
#'
#' @param data_entry Dataframe input that includes all required variables
#' 
#' @return Will return two summaries (one for each ancestry) and save those summaries to your working directory
#'    \item{STUDYPI_summary_vars_index_afr}{Summary statistics for varibles in the African ancestry subset}
#'    \item{STUDYPI_summary_vars_index_eur}{Summary statistics for varibles in the European ancestry subset}
#'    
#' @keywords summary, ancestry stratification
#'
#' @export
#' @examples
#' STUDYPI_ancestry_strat_test <- ancestry.strat.test(classed)



variable.summary.stratified <- function(data_entry)
#Save summary statistics of variables
for (pop in c("index_afr", "index_eur")){ #Loop subsets to eur/afr population and uses correct PCs for each ancestry
  tryCatch({
    if (pop == "index_afr"){ 
      dat = data_entry[index_afr,] #so if the population is afr, we have the afr subset of data
    }
    if (pop == "index_eur"){
      dat = data_entry[index_eur,] #subset of eur population
    }
    
    modelname = paste(study, "summary_vars", pop, sep = "_")
    assign(modelname, summary(dat), envir = .GlobalEnv)
    save(list = modelname, file=paste(modelname, ".RData", sep="_")) #Save model outputs as an R object
    
  }, #Trycatch end curly
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } # loop end curly
