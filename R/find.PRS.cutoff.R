#' A Function to run identify best PRS cutoff
#'
#' This function creates an easy-to-interpret summary to select the best PRS cutoff 
#' for use in the main step of the regression analysis
#'
#' @param data_entry Dataframe input
#'
#' @return Returns plot of R-squared values for each PRS cutoff
#'
#'
#' @keywords polygenic risk score
#'
#' @export
#' @examples
#' polygenicPTSD_contin(classed)


find.PRS.cutoff <- function(data_entry){
  #First create list of PRS variables
  sbp_prs_vars <- colnames(data_entry)[grepl("sbp_prs", colnames(data_entry))] #selecting column names for sbp PRS
  dbp_prs_vars <- colnames(data_entry)[grepl("dbp_prs", colnames(data_entry))] #selecting column names for dbp PRS
  htn_prs_vars <- colnames(data_entry)[grepl("^bp_prs", colnames(data_entry))]
  ptsd_vars <- colnames(data_entry[grepl("ptsd", colnames(data_entry))]) #selecting the four ptsd variable names
  i = 0
  for (bp_outcome in c("SBP_meas_adj_15", "DBP_meas_adj_10")){ #Loop runs through outcomes. Currently just SBP and DBP but will add HTN when ready.
    tryCatch({ #Adding this in case there is an error in any regression; it prints the error without stopping the loop or crashing R! This makes it easy to debug.
      if (bp_outcome == "SBP_meas_adj_15" | bp_outcome == "SBP_meas_adj_10"){ ##If statement to have loop run on correct PRS according to outcome
        prs_vars = prs_variables$sbp_prs_vars
        model = "lm"
        family = ", "
      }
      if (bp_outcome == "DBP_meas_adj_10" | bp_outcome == "DBP_meas_adj_5"){ #Indicate dbp prs variables if outcome is DBP
        prs_vars = prs_variables$dbp_prs_vars
        model = "lm"
        family = ", "
      }
      if (bp_outcome == "htn_aha_new_bi" | bp_outcome == "htn_aha_old_bi"){
        prs_vars = prs_variables$htn_prs_vars
        model = "glm"
        family = ", family=binomial(), "
        }
      if (bp_outcome == "htn_aha_new" | bp_outcome == "htn_aha_old"){
        prs_vars = prs_variables$htn_prs_vars
        model = "multinom"
        family = ", "
      }
      for (pop in c("index_afr", "index_eur")){ #Loop subsets to eur/afr population and uses correct PCs for each ancestry
        tryCatch({
          if (pop == "index_afr"){
            dat = data_entry[index_afr,] #so if the population is afr, we have the afr subset of data
          }
          if (pop == "index_eur"){
            dat = data_entry[index_eur,] #subset of eur population
          }
          for (prs in c(prs_vars)){ #Loop runs through PRS variables within the correct subset specified above based on outcome
            tryCatch({
                              for (covar in c("base", "adjust")){ #Loop does original base with sex, pcs, age, then adds extra covars
                                tryCatch({
                                  if (covar == "base"){
                                    covars = ""
                                  }
                                  if (covar == "adjust"){
                                    covars = "+ educ + trauma_count_lt"
                                  }
                                  #Add 55 based on nhaynes
                                  modelformula = paste(bp_outcome, "~ sex + P1 + P2 + P3 + age +", prs, sep = "")
                                  modelname = paste(study, bp_outcome, prs, model, pop, sep = "_")
                                  assign(modelname, lm(as.formula(modelformula), data=dat), envir = .GlobalEnv)
                                  #save(list = modelname, file=paste(modelname, ".RData", sep="_")) #Save model outputs as an R object
                                  #modelformula = paste(bp_outcome, "~ sex + P1 + P2 + P3 + age*",age_choice, "+", antihtn, ptsd, sign, prs, sep = "")
                                  #assign(paste(study, model, effect, bp_outcome, pop, "age", age_choice, "gender", gender, ptsd, prs,  sep = "_"), lm(as.formula(modelformula), data=dat_gen), envir = .GlobalEnv)
                                  #save(paste(study, model, effect, bp_outcome, pop, "age", age_choice, "gender", gender, ptsd, prs,  sep = "_"), file=paste(study, model, effect, bp_outcome, pop, "age", age_choice, "gender", gender, ptsd, prs,".RData", sep="_")) #Save model outputs as an R object
                                  i = i+1
                                  #grab r-squared value for each PRS
                                  r <- summary(get(modelname))$r.squared
                                  rsq <- c(rsq, r)
                                  print(i)
                                  #rm(list = ls(pattern = "gender")) to clear workspace after each loop
                                }, #Trycatch 7 end curly
                                error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } #gender loop end curly
                              
                  }, #Trycatch 2 end curly
            error=function(e){cat("ERROR :",conditionMessage(e), "\n")})   } #prs loop end curly
        }, #Trycatch 1 end curly
        error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) } #ancestry loop end curly
    }, #Trycatch 1 end curly
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #BP outcome end
  } #end loop
  
} #end function loop