###########  PTSD Physical Health Group - PTSD and BP PRS Project: Step 2 - Linear Regressions Predicting Continuous BP Measures ##########

#Questions
#What to do with studies that didn't have antihtn_use : sensitivity analysis excluding studies that didn't have antihtn-use
#Will PRS files be all together, or separate for SBP, DBP, HTN? I would prefer them merged with standard naming convention

#antihtn strat chi sq run on those that are not NA
#todo: figure out spss to R missing variable 
#todo: make check on if variables are the correct num, chr, factor
#todo: multinom
#Todo: deal with related subjects!
#todo: add in mean and SD and N for all variables analyzed
#todo: for 3 level outcome, possibly report the multiplicative and additive model resutls
#todo: diagnostics

#####Big questions for Jen
#Save model vs. save summary of output? That way we can run diagnostics ourselves?
#dput(summary(lm(cars$speed~cars$dist)),file="summary_lm.txt",control="all")
#This allows to re-import the summary object via
#res=dget("summary_lm.txt")


#Have files saved to a folder on your Desktop called "PGC_PTSD_BP" and set working directory to that folder#
#Be sure to change USERNAME so it reflects what your username#
#Select Windows or Mac path by deleting the "#" in front of the setwd command#
##############
#Windows path#
##############
#setwd("C:/Users/USERNAME/Desktop/PGC_PTSD_BP")
library(MASS)
library(nnet)
library(VGAM)  

##########
#Mac path#
##########
#setwd("Desktop/PGC_PTSD_BP")

#####################
# Set Study PI name #
#####################
#Replace STUDYPI below with last name of Principle Investigator (For example "Sumner")
study <- "StudyPI"

##########################
#Read in merged data file#
##########################
merged <- read.csv("merged.csv")

classed <- htn_outcome_classify(merged)
index_eur = grepl(0, classed$ancestry)
index_afr = grepl(1, classed$ancestry)

setwd("/Users/TinyDragon/github/PolygenicPTSD/data/results")
polygenicPTSD(classed)






#Working Function Loop 
#First create list of PRS variables 
sbp_prs_vars <- colnames(classed)[grepl("sbp_prs", colnames(classed))] #selecting column names for sbp PRS
dbp_prs_vars <- colnames(classed)[grepl("dbp_prs", colnames(classed))] #selecting column names for dbp PRS
htn_prs_vars <- colnames(classed)[grepl("^bp_prs", colnames(classed))]
ptsd_vars <- colnames(classed[grepl("ptsd", colnames(classed))]) #selecting the four ptsd variable names
age_mod <- c("1", "age")

i = 0 
for (bp_outcome in c("SBP_meas", "DBP_meas", "SBP_meas_adj", "DBP_meas_adj", "htn_aha_new_bi", "htn_aha_old_bi", "htn_aha_new", "htn_aha_old")){ #Loop runs through outcomes. Currently just SBP and DBP but will add HTN when ready.
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
          dat = classed[index_afr,] #so if the population is afr, we have the afr subset of data
        }
        if (pop == "index_eur"){
          dat = classed[index_eur,] #subset of eur population
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
                    for (age_choice in c(age_mod)){ #Loop runs through age and age*age (still figuring out how to make good file name for this, currently it is age_1 and age_age)
                      tryCatch({
                        for (gender in c("all", "male", "female")){ #Loop subsets based on gender
                          tryCatch({
                            if (gender =="all"){
                              dat = dat
                            }
                            if (gender =="male"){
                              dat = dat[grep(1, dat$sex),]
                            }
                            if (gender =="female"){
                              dat = dat[grep(0, dat$sex),]
                            }
                            #Add 55 based on nhaynes
                            modelformula = paste(bp_outcome, "~ sex + P1 + P2 + P3 + age*",age_choice, "+", antihtn, ptsd, sign, prs, sep = "")
                            assign(paste(study, model, effect, bp_outcome, pop, "age", age_choice, gender, ptsd, prs,  sep = "_"), paste(model, "(as.formula(modelformula)", family, "data=dat)", sep = ""))
                            #save(paste(study, model, effect, bp_outcome, pop, "age", age_choice, gender, ptsd, prs,  sep = "_"), file=paste(study, model, effect, bp_outcome, pop, "age", age_choice, gender, ptsd, prs,".RData", sep="_")) #Save model outputs as an R object
                            #Add saving output
                            #save(assign...,file=paste("ME",bp_outcome,"age", pop, age_choice, ptsd, prs,".RData",sep="_")) #Save model outputs as an R object
                            #lmOut(paste("ptsd_ME",bp_outcome, prs, sep = ""), file=as.character(paste("ptsdlt_res_summary_ME_", bp_outcome, prs, ".csv", sep="")), ndigit=3, writecsv=T)
                            i = i+1
                            print(i)  
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

