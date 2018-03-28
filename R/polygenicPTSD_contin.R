#' A Function to run analysis on continuous BP measures
#'
#' This function allows you to quickly run and save the required regression
#' analyses for the pipeline.
#'
#' @param data_entry Dataframe input 
#'
#' @return Will return regression objects for all required models, 
#' and save them in your working directory. 
#' 
#' Specifically, runs main effect and interaction models for all 
#' 4 PTSD variables and PRS variables, using a base model of sex, age, first three 
#' ancestry principle components. 
#' 
#' Regression is stratified by ancestry, and sensitivity analyses
#' performed for gender, education, and trauma count.
#'
#'
#' @keywords linear regression, continuous outcome
#'
#' @export
#' @examples
#' polygenicPTSD_contin(classed)


polygenicPTSD_contin <- function(data_entry){
  #First create list of PRS variables 
  sbp_prs_vars <- colnames(data_entry)[grepl("sbp_prs", colnames(data_entry))] #selecting column names for sbp PRS
  dbp_prs_vars <- colnames(data_entry)[grepl("dbp_prs", colnames(data_entry))] #selecting column names for dbp PRS
  htn_prs_vars <- colnames(data_entry)[grepl("^bp_prs", colnames(data_entry))]
  ptsd_vars <- colnames(data_entry[grepl("ptsd", colnames(data_entry))]) #selecting the four ptsd variable names
  i = 0 
  for (bp_outcome in c("SBP_meas", "DBP_meas", "SBP_meas_adj", "DBP_meas_adj")){ #Loop runs through outcomes. Currently just SBP and DBP but will add HTN when ready.
    tryCatch({ #Adding this in case there is an error in any regression; it prints the error without stopping the loop or crashing R! This makes it easy to debug.
      if (bp_outcome == "SBP_meas"){ ##If statement to have loop run on correct PRS according to outcome
        prs_vars = sbp_prs_vars
        antihtn = "antihtn_use +"
        model = "lm"
        family = ", "
      }
      if (bp_outcome == "DBP_meas"){ #Indicate dbp prs variables if outcome is DBP
        prs_vars = dbp_prs_vars
        antihtn = "antihtn_use +"
        model = "lm"
        family = ", "
      }
      if (bp_outcome == "SBP_meas_adj"){ ##If statement to have loop run on correct PRS according to outcome
        prs_vars = sbp_prs_vars
        antihtn = "" #Not including anti-htn medication as covariate in this model since outcome was adjusted +10 to SBP and +5 to DBP if antihtn_use = 1
        model = "lm"
        family = ", "
      }
      if (bp_outcome == "DBP_meas_adj"){ #Indicate dbp prs variables if outcome is DBP
        prs_vars = dbp_prs_vars
        antihtn = ""
        model = "lm"
        family = ", "
      }
      if (bp_outcome == "htn_aha_new_bi" | bp_outcome == "htn_aha_old_bi"){
        prs_vars = htn_prs_vars
        model = "glm"
        family = ", family=binomial(), "
        antihtn = "" #Not including anti-htn medication as covariate in this model since it went into the classification of outcome
      }
      if (bp_outcome == "htn_aha_new" | bp_outcome == "htn_aha_old"){
        prs_vars = htn_prs_vars
        model = "multinom"
        family = ", "
        antihtn = ""
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
              for (ptsd in c(ptsd_vars)){ #Loop runs through PTSD variables
                tryCatch({
                  for (sign in c("+", "*")){ #Loop runs through main effect/interaction
                    tryCatch({
                      if (sign == "+"){ #Define model name for file title (Main Effects model)
                        effect = "ME"
                      }
                      if (sign == "*"){ #Define model name for file title (Interaction model)
                        effect = "Int"
                      }
                      for (age_choice in c("1", "age")){ #Loop runs through age and age*age (still figuring out how to make good file name for this, currently it is age_1 and age_age)
                        tryCatch({
                          for (gender in c("all", "male", "female")){ #Loop subsets based on gender
                            tryCatch({
                              if (gender =="all"){
                                dat_gen = dat
                              }
                              if (gender =="male"){
                                dat_gen = dat[grep(1, dat$sex),]
                              }
                              if (gender =="female"){
                                dat_gen = dat[grep(0, dat$sex),]
                              }
                              for (covar in c("base", "adjust")){ #Loop does original base with sex, pcs, age, then adds extra covars
                                tryCatch({
                                  if (covar == "base"){
                                    covars = ""
                                  }
                                  if (covar == "adjust"){
                                    covars = "+ educ + trauma_count_lt"
                                  }
                                  #Add 55 based on nhaynes
                                  modelformula = paste(bp_outcome, "~ sex + P1 + P2 + P3 + age*",age_choice, "+", antihtn, ptsd, sign, prs, sep = "")
                                  modelname = paste(study, bp_outcome, prs, ptsd, model, effect,  pop, "age", age_choice, "gender", gender,  sep = "_")
                                  assign(modelname, lm(as.formula(modelformula), data=dat_gen), envir = .GlobalEnv)
                                  save(list = modelname, file=paste(modelname, ".RData", sep="_")) #Save model outputs as an R object
                                  #modelformula = paste(bp_outcome, "~ sex + P1 + P2 + P3 + age*",age_choice, "+", antihtn, ptsd, sign, prs, sep = "")
                                  #assign(paste(study, model, effect, bp_outcome, pop, "age", age_choice, "gender", gender, ptsd, prs,  sep = "_"), lm(as.formula(modelformula), data=dat_gen), envir = .GlobalEnv)
                                  #save(paste(study, model, effect, bp_outcome, pop, "age", age_choice, "gender", gender, ptsd, prs,  sep = "_"), file=paste(study, model, effect, bp_outcome, pop, "age", age_choice, "gender", gender, ptsd, prs,".RData", sep="_")) #Save model outputs as an R object
                                  i = i+1
                                  print(i)  
                                  #rm(list = ls(pattern = "gender")) to clear workspace after each loop
                                }, #Trycatch 7 end curly
                                error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } #gender loop end curly
                              
                            }, #Trycatch 6 end curly
                            error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } #gender loop end curly
                          
                        }, #Trycatch 5 end curly
                        error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } #age loop end curly
                    }, #Trycatch 4 end curly
                    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } #main effect/interaction loop end curly
                }, #Trycatch 3 end curly
                error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) } #ptsd loop end curly
            }, #Trycatch 2 end curly
            error=function(e){cat("ERROR :",conditionMessage(e), "\n")})   } #prs loop end curly
        }, #Trycatch 1 end curly
        error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) } #ancestry loop end curly
    }, #Trycatch 1 end curly
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #BP outcome end
  } #end loop
  
} #end function loop