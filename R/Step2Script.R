###########  PTSD Physical Health Group - PTSD and BP PRS Project: Step 2 - Linear Regressions Predicting Continuous BP Measures ##########

#Questions
#What to do with studies that didn't have antihtn_use : sensitivity analysis excluding studies that didn't have antihtn-use
#Will PRS files be all together, or separate for SBP, DBP, HTN? I would prefer them merged with standard naming convention
#**JEN: Are the principle components different for different groups? Different for ancestry only?
#May need to modify naming conventions to have eur_pc1, afr_pc1 etc


#antihtn strat chi sq run on those that are not NA
#todo: figure out spss to R missing variable 
#todo: make check on if variables are the correct num, chr, factor
#todo: multinom
#Todo: add in ancestry data
#Todo: deal with related subjects!
#todo: add in mean and SD and N for all variables analyzed
#todo: SBP and DBP PRS need to be loaded
#todo: for 3 level outcome, possibly report the multiplicative and additive model resutls
#todo: more rigorous specification for phenotype file, error checking all alround
#todo: try code in case models DONT fit
#todo: diagnostics

#####Big questions for Jen
#Save model vs. save summary of output? That way we can run diagnostics ourselves?
#dput(summary(lm(cars$speed~cars$dist)),file="summary_lm.txt",control="all")
#This allows to re-import the summary object via
#res=dget("summary_lm.txt")



###Remember to conduct analyses separately in different ancestry groups in your sample based on "ancestry" variable###

#Have files saved to a folder on your Desktop called "PGC_PTSD_BP" and set working directory to that folder#
#Be sure to change USERNAME so it reflects what your username#
#Select Windows or Mac path by deleting the "#" in front of the setwd command#
##############
#Windows path#
##############
#setwd("C:/Users/USERNAME/Desktop/PGC_PTSD_BP")

##########
#Mac path#
##########
#setwd("Desktop/PGC_PTSD_BP")

##########################
#Read in merged data file#
##########################
merged <- read.csv("merged.csv")

##*New loop for multiple dbp_prs
#First create list of PRS variables 
sbp_prs_vars <- colnames(merged)[grepl("sbp_prs", colnames(merged))] #selecting column names for sbp PRS
dbp_prs_vars <- colnames(merged)[grepl("dbp_prs", colnames(merged))] #selecting column names for dbp PRS
htn_prs_vars <- colnames(merged)[grepl("^bp_prs", colnames(merged))]
ptsd_vars <- colnames(merged[grepl("ptsd", colnames(merged))]) #selecting the four ptsd variable names
age_mod <- c("1", "age")

i = 0 
#Main Loop 
for (bp_outcome in c("SBP_meas", "DBP_meas")){ #Loop runs through outcomes. Currently just SBP and DBP but will add HTN when ready.
  tryCatch({ #Adding this in case there is an error in any regression; it prints the error without stopping the loop or crashing R! This makes it easy to debug.
      if (bp_outcome == "SBP_meas"){ ##If statement to have loop run on correct PRS according to outcome
        prs_vars = sbp_prs_vars
      }
      if (bp_outcome == "DBP_meas"){ #Indicate dbp prs variables if outcome is DBP
        prs_vars = dbp_prs_vars
      }
    for (pop in c("index_afr", "index_eur")){ #Loop subsets to eur/afr population and uses correct PCs for each ancestry
      tryCatch({
            if (pop == "index_afr"){ 
                dat = merged[index_afr,] #so if the population is afr, we have the afr subset of data
            }
            if (pop == "index_eur"){
                dat = merged[index_eur,] #subset of eur population
            }
        ###DO PCs need to be subsetted or is there common PC variables between ethnicities?
    for (prs in c(prs_vars)){ #Loop runs through PRS variables within the correct subset specified above based on outcome
          tryCatch({
            for (ptsd in c(ptsd_vars)){ #Loop runs through PTSD variables
              tryCatch({
                for (sign in c("+", "*")){ #Loop runs through main effect/interaction
                  tryCatch({
                    if (sign == "+"){ #Define model name for file title (Main Effects model)
                      model = "ME"
                    }
                    if (sign == "*"){ #Define model name for file title (Interaction model)
                      model = "Int"
                    }
                for (age_choice in c(age_mod)){ #Loop runs through age and age*age (still figuring out how to make good file name for this, currently it is age_1 and age_age)
                  tryCatch({
                #Add loop for gender- all, male, female?
                #Add 55 based on nhaynes
                    #hypertension and adjustment
                #Add loop for different BP measurement and remove anthtn_use
        modelformula = paste(bp_outcome, "~ sex + P1 + P2 + P3 + age*",age_choice, "+", ptsd, sign, prs, sep = "")
        assign(paste(model, bp_outcome, pop, "age", age_choice, ptsd, prs,  sep = "_"), lm(as.formula(modelformula), data=dat))
        #Add saving output
        #save(assign...,file=paste("ME",bp_outcome,"age", pop, age_choice, ptsd, prs,".RData",sep="_")) #Save model outputs as an R object
        #lmOut(paste("ptsd_ME",bp_outcome, prs, sep = ""), file=as.character(paste("ptsdlt_res_summary_ME_", bp_outcome, prs, ".csv", sep="")), ndigit=3, writecsv=T)
        i = i+1
        print(i)          
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











#Old below

for (bp_outcome in c("SBP_meas", "DBP_meas")){ #Loop runs through outcomes. Currently just SBP and DBP but will add HTN when ready.
  tryCatch({ #Adding this in case there is an error in any regression; it prints the error without stopping the loop or crashing R! This makes it easy to debug.
    if (bp_outcome == "SBP_meas"){ ##If statement to have loop run on correct PRS according to outcome
      prs_vars = sbp_prs_vars
    }
    if (bp_outcome == "DBP_meas"){
      prs_vars = dbp_prs_vars
    }
    for (pop in c("index_all", "index_afr", "index_eur")){ #Loop subsets to eur/afr population and uses correct PCs for each ancestry
      tryCatch({
        if (pop == "index_afr"){ 
          dat = merged[index_afr,] #so if the population is afr, we have the afr subset of data
        }
        if (pop == "index_eur"){
          dat = merged[index_eur,] #subset of eur population
        }
        if (pop == "index_all"){
          dat = merged #using entire population
        }   
        ###DO PCs need to be subsetted or is there common PC variables between ethnicities?
        for (prs in c(prs_vars)){ #Loop runs through PRS variables within the correct subset specified above based on outcome
          tryCatch({
            for (ptsd in c(ptsd_vars)){ #Loop runs through PTSD variables
              tryCatch({
                for (age_choice in c(age_mod)){ #Loop runs through age and age*age (still figuring out how to make good file name for this, currently it is age_1 and age_age)
                  tryCatch({
                    #Main effects model first
                    #Add loop for interaction c("ptsd + prs", "ptsd*prs")
                    #Add loop for ancestry (different PCs)
                    #Add loop for gender
                    #Add loop for different BP measurment and remove anthtn_use
                    modelformula = paste(bp_outcome, "~ sex + age*",age_choice, "+", ptsd, "+", prs, sep = "")
                    assign(paste("ME",bp_outcome,"age", pop, age_choice, ptsd, prs,  sep = "_"), lm(as.formula(modelformula), data=dat))
                    #Add saving output
                    #save(assign...,file=paste("ME",bp_outcome,"age", pop, age_choice, ptsd, prs,".RData",sep="_")) #Save model outputs as an R object
                    #lmOut(paste("ptsd_ME",bp_outcome, prs, sep = ""), file=as.character(paste("ptsdlt_res_summary_ME_", bp_outcome, prs, ".csv", sep="")), ndigit=3, writecsv=T)
                  }, #Trycatch 4 end curly
                  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  } #age loop end curly
              }, #Trycatch 3 end curly
              error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) } #ptsd loop end curly
          }, #Trycatch 2 end curly
          error=function(e){cat("ERROR :",conditionMessage(e), "\n")})   } #prs loop end curly
      }, #Trycatch 1 end curly
      error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) } #ancestry loop end curly
  }, #Trycatch 1 end curly
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) #BP outcome end
} #end loop


for (i in 1:length(prsVars)){  #Loop runs through columns of dpbPrs
  tryCatch({ #Adding this in case there is an error in any regression
    prs <- prsVars[i] #
    for (j in bp_outcome)
    tryCatch({
    #Main effects model first
    assign(paste("pts",bp_outcome, model, prs, sep = ""), lm(bp_outcome ~ ptsd_diag_lt + prs + sex + age + P1 + P2 + P3, data=merged))
    lmOut(paste("ptsdlt_res_ME_", bp_outcome, prs, sep = ""), file=as.character(paste("ptsdlt_res_summary_ME_", prs, ".csv", sep="")), ndigit=3, writecsv=T)
    #Interaction model below
    assign(paste("ptsdlt_res_Int_", prs, sep = ""), lm(bp_outcome ~ ptsd_diag_lt*prs + sex + age + P1 + P2 + P3, data=merged))
    filenameint <- as.character(paste("ptsdlt_res_summary_Int_", bp_outcome, prs, ".csv", sep="")) 
    lmOut(paste("ptsdlt_res_Int_", prs, sep = ""), file=filenameint, ndigit=3, writecsv=T)
  }, #Trycatch end curly
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  }, #Trycatch end curly
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
} #end loop


#Trying Adam's way

#Function details:
#The reason I am using the paste function in formula is to convert generic variable names to the actual input strings. 
#Otherwise the model would look for a variable named 'outcome' in the data structure and not find it
#By using paste, 'outcome' gets correctly converted to the user's outcome variable name

updater_analyzer <- function(prs, formula_statement, outcome, ptsd, dataset)
{
  
  main_fx <- update(formula_statement, paste(outcome, "~ . +", ptsd, "+", prs)) #Main effects model
  interaction_v1 <- update(formula_statement, paste(outcome, "~ . + " , ptsd, "*", prs)) #Basic interaction model
  interaction_v2 <- update(formula_statement, paste(outcome, "~ . + ",  " scale(" ,ptsd, ",scale=F,center=T)", "*" , "scale(", prs, ",scale=F,center=T)")) #centered interactions
  interaction_v3 <- update(formula_statement, paste(outcome, "~ . * ",  "(scale(", ptsd, ",scale=F,center=T) + scale(", prs,",scale=F,center=T))", "+","(scale(", ptsd, ",scale=F,center=T) * scale(" , prs,",scale=F,center=T))")) #Keller recommended interaction models require all cov x PTSD and cov x PRS interactions
  
  m1 <- summary(lm(main_fx, data=dataset))
  m2 <- summary(lm(interaction_v1, data=dataset))
  m3 <- summary(lm(interaction_v2, data=dataset))
  m4 <- summary(lm(interaction_v3, data=dataset))
  
  output <- list(m1,m2,m3,m4) #Output all models into a list
}

#bp_meas
bp_meas = dat$sbp_meas
#I'll specify the 'baseline' models here (i.e. No PRS or PTSD covariates included)
#for modelloop
base_model <- formula(sbp_meas ~ sex + age + P1 + P2 + P3, data=dat)
covar_model <- formula(bp_meas ~ sex + age + P1 + P2 + P3 + as.factor(educ) + trauma_count_lt) 

#test
model <- lapply(prs_columns, updater_analyzer, formula_statement = base_model, outcome = dat$sbp_meas, ptsd=dat$ptsd_diag_lt, dataset=dat)

main_fx <- update(base_model, paste("sbp_meas", "~ . +", "ptsd_diag_lt", "+", "X0.01")) #Main effects model
m1 <- summary(lm(main_fx, data=dat))


model <- lapply(prs_columns, updater_analyzer, formula_statement = modelloop, outcome = bp_meas, ptsd=ptsdloop, dataset=dat)

save(model,file=paste(study,bp_meas,modelloop,ptsdloop,prs_col,i,".RData",sep="_")) #Save model outputs as an R object



#Interaction model with additional covariates#

ptsdlt_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs0001 + ptsdlt_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_dbp0001int_adj_res)
lmOut(ptsdlt_dbp0001int_adj_res, file="ptsdlt_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs001 + ptsdlt_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_dbp001int_adj_res)
lmOut(ptsdlt_dbp001int_adj_res, file="ptsdlt_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs01 + ptsdlt_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_dbp01int_adj_res)
lmOut(ptsdlt_dbp01int_adj_res, file="ptsdlt_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs05 + ptsdlt_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_dbp05int_adj_res)
lmOut(ptsdlt_dbp05int_adj_res, file="ptsdlt_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs10 + ptsdlt_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_dbp10int_adj_res)
lmOut(ptsdlt_dbp10int_adj_res, file="ptsdlt_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs50 + ptsdlt_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_dbp50int_adj_res)
lmOut(ptsdlt_dbp50int_adj_res, file="ptsdlt_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdlt_sbp0001ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp0001ME_res)
lmOut(ptsdlt_sbp0001ME_res, file="ptsdlt_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp001ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp001ME_res)
lmOut(ptsdlt_sbp001ME_res, file="ptsdlt_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp01ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp01ME_res)
lmOut(ptsdlt_sbp01ME_res, file="ptsdlt_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp05ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp05ME_res)
lmOut(ptsdlt_sbp05ME_res, file="ptsdlt_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp10ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp10ME_res)
lmOut(ptsdlt_sbp10ME_res, file="ptsdlt_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp50ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp50ME_res)
lmOut(ptsdlt_sbp50ME_res, file="ptsdlt_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged$ptsdlt_sbp0001_i <-merged$ptsd_diag_lt*merged$sbp_prs0001

ptsdlt_sbp0001int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs0001 + ptsdlt_sbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp0001int_res)
lmOut(ptsdlt_sbp0001int_res, file="ptsdlt_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_sbp001_i <-merged$ptsd_diag_lt*merged$sbp_prs001

ptsdlt_sbp001int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs001 + ptsdlt_sbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp001int_res)
lmOut(ptsdlt_sbp001int_res, file="ptsdlt_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_sbp01_i <-merged$ptsd_diag_lt*merged$sbp_prs01

ptsdlt_sbp01int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs01 + ptsdlt_sbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp01int_res)
lmOut(ptsdlt_sbp01int_res, file="ptsdlt_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_sbp05_i <-merged$ptsd_diag_lt*merged$sbp_prs05

ptsdlt_sbp05int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs05 + ptsdlt_sbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp05int_res)
lmOut(ptsdlt_sbp05int_res, file="ptsdlt_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_sbp10_i <-merged$ptsd_diag_lt*merged$sbp_prs10

ptsdlt_sbp10int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs10 + ptsdlt_sbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp10int_res)
lmOut(ptsdlt_sbp10int_res, file="ptsdlt_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_sbp50_i <-merged$ptsd_diag_lt*merged$sbp_prs50

ptsdlt_sbp50int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs50 + ptsdlt_sbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_sbp50int_res)
lmOut(ptsdlt_sbp50int_res, file="ptsdlt_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdlt_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs0001 + ptsdlt_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_sbp0001int_adj_res)
lmOut(ptsdlt_sbp0001int_adj_res, file="ptsdlt_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs001 + ptsdlt_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_sbp001int_adj_res)
lmOut(ptsdlt_sbp001int_adj_res, file="ptsdlt_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs01 + ptsdlt_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_sbp01int_adj_res)
lmOut(ptsdlt_sbp01int_adj_res, file="ptsdlt_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs05 + ptsdlt_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_sbp05int_adj_res)
lmOut(ptsdlt_sbp05int_adj_res, file="ptsdlt_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs10 + ptsdlt_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_sbp10int_adj_res)
lmOut(ptsdlt_sbp10int_adj_res, file="ptsdlt_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs50 + ptsdlt_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_sbp50int_adj_res)
lmOut(ptsdlt_sbp50int_adj_res, file="ptsdlt_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables###

#Main effects model# 

ptsdlt_map0001ME_res <- lm(map ~ ptsd_diag_lt + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map0001ME_res)
lmOut(ptsdlt_map0001ME_res, file="ptsdlt_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map001ME_res <- lm(map ~ ptsd_diag_lt + bp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map001ME_res)
lmOut(ptsdlt_map001ME_res, file="ptsdlt_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map01ME_res <- lm(map ~ ptsd_diag_lt + bp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map01ME_res)
lmOut(ptsdlt_map01ME_res, file="ptsdlt_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map05ME_res <- lm(map ~ ptsd_diag_lt + bp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map05ME_res)
lmOut(ptsdlt_map05ME_res, file="ptsdlt_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map10ME_res <- lm(map ~ ptsd_diag_lt + bp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map10ME_res)
lmOut(ptsdlt_map10ME_res, file="ptsdlt_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map50ME_res <- lm(map ~ ptsd_diag_lt + bp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map50ME_res)
lmOut(ptsdlt_map50ME_res, file="ptsdlt_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged$ptsdlt_map0001_i <-merged$ptsd_diag_lt*merged$bp_prs0001

ptsdlt_map0001int_res <- lm(map ~ ptsd_diag_lt + bp_prs0001 + ptsdlt_map0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map0001int_res)
lmOut(ptsdlt_map0001int_res, file="ptsdlt_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_map001_i <-merged$ptsd_diag_lt*merged$bp_prs001

ptsdlt_map001int_res <- lm(map ~ ptsd_diag_lt + bp_prs001 + ptsdlt_map001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map001int_res)
lmOut(ptsdlt_map001int_res, file="ptsdlt_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_map01_i <-merged$ptsd_diag_lt*merged$bp_prs01

ptsdlt_map01int_res <- lm(map ~ ptsd_diag_lt + bp_prs01 + ptsdlt_map01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map01int_res)
lmOut(ptsdlt_map01int_res, file="ptsdlt_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_map05_i <-merged$ptsd_diag_lt*merged$bp_prs05

ptsdlt_map05int_res <- lm(map ~ ptsd_diag_lt + bp_prs05 + ptsdlt_map05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map05int_res)
lmOut(ptsdlt_map05int_res, file="ptsdlt_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_map10_i <-merged$ptsd_diag_lt*merged$bp_prs10

ptsdlt_map10int_res <- lm(map ~ ptsd_diag_lt + bp_prs10 + ptsdlt_map10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map10int_res)
lmOut(ptsdlt_map10int_res, file="ptsdlt_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdlt_map50_i <-merged$ptsd_diag_lt*merged$bp_prs50

ptsdlt_map50int_res <- lm(map ~ ptsd_diag_lt + bp_prs50 + ptsdlt_map50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdlt_map50int_res)
lmOut(ptsdlt_map50int_res, file="ptsdlt_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdlt_map0001int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs0001 + ptsdlt_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_map0001int_adj_res)
lmOut(ptsdlt_map0001int_adj_res, file="ptsdlt_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map001int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs001 + ptsdlt_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_map001int_adj_res)
lmOut(ptsdlt_map001int_adj_res, file="ptsdlt_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map01int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs01 + ptsdlt_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_map01int_adj_res)
lmOut(ptsdlt_map01int_adj_res, file="ptsdlt_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map05int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs05 + ptsdlt_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_map05int_adj_res)
lmOut(ptsdlt_map05int_adj_res, file="ptsdlt_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map10int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs10 + ptsdlt_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_map10int_adj_res)
lmOut(ptsdlt_map10int_adj_res, file="ptsdlt_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map50int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs50 + ptsdlt_map50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdlt_map50int_adj_res)
lmOut(ptsdlt_map50int_adj_res, file="ptsdlt_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Step 2b - Lifetime measure of PTSD severity###

###Outcome: Diastolic BP, dbp_prs variables###

#Main effects model# 

ptsdltsx_dbp0001ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp0001ME_res)
lmOut(ptsdltsx_dbp0001ME_res, file="ptsdltsx_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp001ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp001ME_res)
lmOut(ptsdltsx_dbp001ME_res, file="ptsdltsx_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp01ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp01ME_res)
lmOut(ptsdltsx_dbp01ME_res, file="ptsdltsx_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp05ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp05ME_res)
lmOut(ptsdltsx_dbp05ME_res, file="ptsdltsx_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp10ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp10ME_res)
lmOut(ptsdltsx_dbp10ME_res, file="ptsdltsx_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp50ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp50ME_res)
lmOut(ptsdltsx_dbp50ME_res, file="ptsdltsx_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$dbp_prs0001_c <- merged$dbp_prs0001 - mean(merged$dbp_prs0001, na.rm=T)

merged$ptsdltsx_dbp0001_i <-merged$ptsdltsx_c*merged$dbp_prs0001_c

ptsdltsx_dbp0001int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs0001 + ptsdltsx_dbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp0001int_res)
lmOut(ptsdltsx_dbp0001int_res, file="ptsdltsx_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$dbp_prs001_c <- merged$dbp_prs001 - mean(merged$dbp_prs001, na.rm=T)

merged$ptsdltsx_dbp001_i <-merged$ptsdltsx_c*merged$dbp_prs001_c

ptsdltsx_dbp001int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs001 + ptsdltsx_dbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp001int_res)
lmOut(ptsdltsx_dbp001int_res, file="ptsdltsx_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$dbp_prs01_c <- merged$dbp_prs01 - mean(merged$dbp_prs01, na.rm=T)

merged$ptsdltsx_dbp01_i <-merged$ptsdltsx_c*merged$dbp_prs01_c

ptsdltsx_dbp01int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs01 + ptsdltsx_dbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp01int_res)
lmOut(ptsdltsx_dbp01int_res, file="ptsdltsx_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$dbp_prs05_c <- merged$dbp_prs05 - mean(merged$dbp_prs05, na.rm=T)

merged$ptsdltsx_dbp05_i <-merged$ptsdltsx_c*merged$dbp_prs05_c

ptsdltsx_dbp05int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs05 + ptsdltsx_dbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp05int_res)
lmOut(ptsdltsx_dbp05int_res, file="ptsdltsx_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$dbp_prs10_c <- merged$dbp_prs10 - mean(merged$dbp_prs10, na.rm=T)

merged$ptsdltsx_dbp10_i <-merged$ptsdltsx_c*merged$dbp_prs10_c

ptsdltsx_dbp10int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs10 + ptsdltsx_dbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp10int_res)
lmOut(ptsdltsx_dbp10int_res, file="ptsdltsx_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$dbp_prs50_c <- merged$dbp_prs50 - mean(merged$dbp_prs50, na.rm=T)

merged$ptsdltsx_dbp50_i <-merged$ptsdltsx_c*merged$dbp_prs50_c

ptsdltsx_dbp50int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs50 + ptsdltsx_dbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_dbp50int_res)
lmOut(ptsdltsx_dbp50int_res, file="ptsdltsx_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdltsx_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs0001 + ptsdltsx_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_dbp0001int_adj_res)
lmOut(ptsdltsx_dbp0001int_adj_res, file="ptsdltsx_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs001 + ptsdltsx_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_dbp001int_adj_res)
lmOut(ptsdltsx_dbp001int_adj_res, file="ptsdltsx_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs01 + ptsdltsx_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_dbp01int_adj_res)
lmOut(ptsdltsx_dbp01int_adj_res, file="ptsdltsx_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs05 + ptsdltsx_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_dbp05int_adj_res)
lmOut(ptsdltsx_dbp05int_adj_res, file="ptsdltsx_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs10 + ptsdltsx_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_dbp10int_adj_res)
lmOut(ptsdltsx_dbp10int_adj_res, file="ptsdltsx_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs50 + ptsdltsx_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_dbp50int_adj_res)
lmOut(ptsdltsx_dbp50int_adj_res, file="ptsdltsx_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdltsx_sbp0001ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp0001ME_res)
lmOut(ptsdltsx_sbp0001ME_res, file="ptsdltsx_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp001ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp001ME_res)
lmOut(ptsdltsx_sbp001ME_res, file="ptsdltsx_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp01ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp01ME_res)
lmOut(ptsdltsx_sbp01ME_res, file="ptsdltsx_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp05ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp05ME_res)
lmOut(ptsdltsx_sbp05ME_res, file="ptsdltsx_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp10ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp10ME_res)
lmOut(ptsdltsx_sbp10ME_res, file="ptsdltsx_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp50ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp50ME_res)
lmOut(ptsdltsx_sbp50ME_res, file="ptsdltsx_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$sbp_prs0001_c <- merged$sbp_prs0001 - mean(merged$sbp_prs0001, na.rm=T)

merged$ptsdltsx_sbp0001_i <-merged$ptsdltsx_c*merged$sbp_prs0001_c

ptsdltsx_sbp0001int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs0001 + ptsdltsx_sbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp0001int_res)
lmOut(ptsdltsx_sbp0001int_res, file="ptsdltsx_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$sbp_prs001_c <- merged$sbp_prs001 - mean(merged$sbp_prs001, na.rm=T)

merged$ptsdltsx_sbp001_i <-merged$ptsdltsx_c*merged$sbp_prs001_c

ptsdltsx_sbp001int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs001 + ptsdltsx_sbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp001int_res)
lmOut(ptsdltsx_sbp001int_res, file="ptsdltsx_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$sbp_prs01_c <- merged$sbp_prs01 - mean(merged$sbp_prs01, na.rm=T)

merged$ptsdltsx_sbp01_i <-merged$ptsdltsx_c*merged$sbp_prs01_c

ptsdltsx_sbp01int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs01 + ptsdltsx_sbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp01int_res)
lmOut(ptsdltsx_sbp01int_res, file="ptsdltsx_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$sbp_prs05_c <- merged$sbp_prs05 - mean(merged$sbp_prs05, na.rm=T)

merged$ptsdltsx_sbp05_i <-merged$ptsdltsx_c*merged$sbp_prs05_c

ptsdltsx_sbp05int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs05 + ptsdltsx_sbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp05int_res)
lmOut(ptsdltsx_sbp05int_res, file="ptsdltsx_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$sbp_prs10_c <- merged$sbp_prs10 - mean(merged$sbp_prs10, na.rm=T)

merged$ptsdltsx_sbp10_i <-merged$ptsdltsx_c*merged$sbp_prs10_c

ptsdltsx_sbp10int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs10 + ptsdltsx_sbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp10int_res)
lmOut(ptsdltsx_sbp10int_res, file="ptsdltsx_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$sbp_prs50_c <- merged$sbp_prs50 - mean(merged$sbp_prs50, na.rm=T)

merged$ptsdltsx_sbp50_i <-merged$ptsdltsx_c*merged$sbp_prs50_c

ptsdltsx_sbp50int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs50 + ptsdltsx_sbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_sbp50int_res)
lmOut(ptsdltsx_sbp50int_res, file="ptsdltsx_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdltsx_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs0001 + ptsdltsx_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_sbp0001int_adj_res)
lmOut(ptsdltsx_sbp0001int_adj_res, file="ptsdltsx_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs001 + ptsdltsx_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_sbp001int_adj_res)
lmOut(ptsdltsx_sbp001int_adj_res, file="ptsdltsx_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs01 + ptsdltsx_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_sbp01int_adj_res)
lmOut(ptsdltsx_sbp01int_adj_res, file="ptsdltsx_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs05 + ptsdltsx_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_sbp05int_adj_res)
lmOut(ptsdltsx_sbp05int_adj_res, file="ptsdltsx_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs10 + ptsdltsx_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_sbp10int_adj_res)
lmOut(ptsdltsx_sbp10int_adj_res, file="ptsdltsx_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs50 + ptsdltsx_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_sbp50int_adj_res)
lmOut(ptsdltsx_sbp50int_adj_res, file="ptsdltsx_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables### 

#Main effects model# 

ptsdltsx_map0001ME_res <- lm(map ~ ptsd_sx_lt + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map0001ME_res)
lmOut(ptsdltsx_map0001ME_res, file="ptsdltsx_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map001ME_res <- lm(map ~ ptsd_sx_lt + bp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map001ME_res)
lmOut(ptsdltsx_map001ME_res, file="ptsdltsx_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map01ME_res <- lm(map ~ ptsd_sx_lt + bp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map01ME_res)
lmOut(ptsdltsx_map01ME_res, file="ptsdltsx_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map05ME_res <- lm(map ~ ptsd_sx_lt + bp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map05ME_res)
lmOut(ptsdltsx_map05ME_res, file="ptsdltsx_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map10ME_res <- lm(map ~ ptsd_sx_lt + bp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map10ME_res)
lmOut(ptsdltsx_map10ME_res, file="ptsdltsx_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map50ME_res <- lm(map ~ ptsd_sx_lt + bp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map50ME_res)
lmOut(ptsdltsx_map50ME_res, file="ptsdltsx_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$bp_prs0001_c <- merged$bp_prs0001 - mean(merged$bp_prs0001, na.rm=T)

merged$ptsdltsx_map0001_i <-merged$ptsdltsx_c*merged$bp_prs0001_c

ptsdltsx_map0001int_res <- lm(map ~ ptsd_sx_lt + bp_prs0001 + ptsdltsx_map0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map0001int_res)
lmOut(ptsdltsx_map0001int_res, file="ptsdltsx_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$bp_prs001_c <- merged$bp_prs001 - mean(merged$bp_prs001, na.rm=T)

merged$ptsdltsx_map001_i <-merged$ptsdltsx_c*merged$bp_prs001_c

ptsdltsx_map001int_res <- lm(map ~ ptsd_sx_lt + bp_prs001 + ptsdltsx_map001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map001int_res)
lmOut(ptsdltsx_map001int_res, file="ptsdltsx_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$bp_prs01_c <- merged$bp_prs01 - mean(merged$bp_prs01, na.rm=T)

merged$ptsdltsx_map01_i <-merged$ptsdltsx_c*merged$bp_prs01_c

ptsdltsx_map01int_res <- lm(map ~ ptsd_sx_lt + bp_prs01 + ptsdltsx_map01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map01int_res)
lmOut(ptsdltsx_map01int_res, file="ptsdltsx_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$bp_prs05_c <- merged$bp_prs05 - mean(merged$bp_prs05, na.rm=T)

merged$ptsdltsx_map05_i <-merged$ptsdltsx_c*merged$bp_prs05_c

ptsdltsx_map05int_res <- lm(map ~ ptsd_sx_lt + bp_prs05 + ptsdltsx_map05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map05int_res)
lmOut(ptsdltsx_map05int_res, file="ptsdltsx_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$bp_prs10_c <- merged$bp_prs10 - mean(merged$bp_prs10, na.rm=T)

merged$ptsdltsx_map10_i <-merged$ptsdltsx_c*merged$bp_prs10_c

ptsdltsx_map10int_res <- lm(map ~ ptsd_sx_lt + bp_prs10 + ptsdltsx_map10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map10int_res)
lmOut(ptsdltsx_map10int_res, file="ptsdltsx_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdltsx_c <- merged$ptsd_sx_lt - mean(merged$ptsd_sx_lt, na.rm=T)
merged$bp_prs50_c <- merged$bp_prs50 - mean(merged$bp_prs50, na.rm=T)

merged$ptsdltsx_map50_i <-merged$ptsdltsx_c*merged$bp_prs50_c

ptsdltsx_map50int_res <- lm(map ~ ptsd_sx_lt + bp_prs50 + ptsdltsx_map50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdltsx_map50int_res)
lmOut(ptsdltsx_map50int_res, file="ptsdltsx_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdltsx_map0001int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs0001 + ptsdltsx_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_map0001int_adj_res)
lmOut(ptsdltsx_map0001int_adj_res, file="ptsdltsx_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map001int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs001 + ptsdltsx_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_map001int_adj_res)
lmOut(ptsdltsx_map001int_adj_res, file="ptsdltsx_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map01int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs01 + ptsdltsx_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_map01int_adj_res)
lmOut(ptsdltsx_map01int_adj_res, file="ptsdltsx_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map05int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs05 + ptsdltsx_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_map05int_adj_res)
lmOut(ptsdltsx_map05int_adj_res, file="ptsdltsx_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map10int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs10 + ptsdltsx_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_map10int_adj_res)
lmOut(ptsdltsx_map10int_adj_res, file="ptsdltsx_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map50int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs50 + ptsdltsx_map50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdltsx_map50int_adj_res)
lmOut(ptsdltsx_map50int_adj_res, file="ptsdltsx_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Step 2c - Current measure of PTSD diagnosis###

###Outcome: Diastolic BP, dbp_prs variables###

#Main effects model# 

ptsdcur_dbp0001ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp0001ME_res)
lmOut(ptsdcur_dbp0001ME_res, file="ptsdcur_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp001ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp001ME_res)
lmOut(ptsdcur_dbp001ME_res, file="ptsdcur_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp01ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp01ME_res)
lmOut(ptsdcur_dbp01ME_res, file="ptsdcur_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp05ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp05ME_res)
lmOut(ptsdcur_dbp05ME_res, file="ptsdcur_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp10ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp10ME_res)
lmOut(ptsdcur_dbp10ME_res, file="ptsdcur_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp50ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp50ME_res)
lmOut(ptsdcur_dbp50ME_res, file="ptsdcur_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged$ptsdcur_dbp0001_i <-merged$ptsd_diag_cur*merged$dbp_prs0001

ptsdcur_dbp0001int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs0001 + ptsdcur_dbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp0001int_res)
lmOut(ptsdcur_dbp0001int_res, file="ptsdcur_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_dbp001_i <-merged$ptsd_diag_cur*merged$dbp_prs001

ptsdcur_dbp001int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs001 + ptsdcur_dbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp001int_res)
lmOut(ptsdcur_dbp001int_res, file="ptsdcur_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_dbp01_i <-merged$ptsd_diag_cur*merged$dbp_prs01

ptsdcur_dbp01int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs01 + ptsdcur_dbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp01int_res)
lmOut(ptsdcur_dbp01int_res, file="ptsdcur_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_dbp05_i <-merged$ptsd_diag_cur*merged$dbp_prs05

ptsdcur_dbp05int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs05 + ptsdcur_dbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp05int_res)
lmOut(ptsdcur_dbp05int_res, file="ptsdcur_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_dbp10_i <-merged$ptsd_diag_cur*merged$dbp_prs10

ptsdcur_dbp10int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs10 + ptsdcur_dbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp10int_res)
lmOut(ptsdcur_dbp10int_res, file="ptsdcur_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_dbp50_i <-merged$ptsd_diag_cur*merged$dbp_prs50

ptsdcur_dbp50int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs50 + ptsdcur_dbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_dbp50int_res)
lmOut(ptsdcur_dbp50int_res, file="ptsdcur_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcur_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs0001 + ptsdcur_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_dbp0001int_adj_res)
lmOut(ptsdcur_dbp0001int_adj_res, file="ptsdcur_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs001 + ptsdcur_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_dbp001int_adj_res)
lmOut(ptsdcur_dbp001int_adj_res, file="ptsdcur_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs01 + ptsdcur_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_dbp01int_adj_res)
lmOut(ptsdcur_dbp01int_adj_res, file="ptsdcur_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs05 + ptsdcur_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_dbp05int_adj_res)
lmOut(ptsdcur_dbp05int_adj_res, file="ptsdcur_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs10 + ptsdcur_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_dbp10int_adj_res)
lmOut(ptsdcur_dbp10int_adj_res, file="ptsdcur_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs50 + ptsdcur_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_dbp50int_adj_res)
lmOut(ptsdcur_dbp50int_adj_res, file="ptsdcur_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdcur_sbp0001ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp0001ME_res)
lmOut(ptsdcur_sbp0001ME_res, file="ptsdcur_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp001ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp001ME_res)
lmOut(ptsdcur_sbp001ME_res, file="ptsdcur_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp01ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp01ME_res)
lmOut(ptsdcur_sbp01ME_res, file="ptsdcur_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp05ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp05ME_res)
lmOut(ptsdcur_sbp05ME_res, file="ptsdcur_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp10ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp10ME_res)
lmOut(ptsdcur_sbp10ME_res, file="ptsdcur_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp50ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp50ME_res)
lmOut(ptsdcur_sbp50ME_res, file="ptsdcur_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged$ptsdcur_sbp0001_i <-merged$ptsd_diag_cur*merged$sbp_prs0001

ptsdcur_sbp0001int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs0001 + ptsdcur_sbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp0001int_res)
lmOut(ptsdcur_sbp0001int_res, file="ptsdcur_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_sbp001_i <-merged$ptsd_diag_cur*merged$sbp_prs001

ptsdcur_sbp001int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs001 + ptsdcur_sbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp001int_res)
lmOut(ptsdcur_sbp001int_res, file="ptsdcur_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_sbp01_i <-merged$ptsd_diag_cur*merged$sbp_prs01

ptsdcur_sbp01int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs01 + ptsdcur_sbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp01int_res)
lmOut(ptsdcur_sbp01int_res, file="ptsdcur_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_sbp05_i <-merged$ptsd_diag_cur*merged$sbp_prs05

ptsdcur_sbp05int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs05 + ptsdcur_sbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp05int_res)
lmOut(ptsdcur_sbp05int_res, file="ptsdcur_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_sbp10_i <-merged$ptsd_diag_cur*merged$sbp_prs10

ptsdcur_sbp10int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs10 + ptsdcur_sbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp10int_res)
lmOut(ptsdcur_sbp10int_res, file="ptsdcur_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_sbp50_i <-merged$ptsd_diag_cur*merged$sbp_prs50

ptsdcur_sbp50int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs50 + ptsdcur_sbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_sbp50int_res)
lmOut(ptsdcur_sbp50int_res, file="ptsdcur_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcur_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs0001 + ptsdcur_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_sbp0001int_adj_res)
lmOut(ptsdcur_sbp0001int_adj_res, file="ptsdcur_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs001 + ptsdcur_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_sbp001int_adj_res)
lmOut(ptsdcur_sbp001int_adj_res, file="ptsdcur_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs01 + ptsdcur_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_sbp01int_adj_res)
lmOut(ptsdcur_sbp01int_adj_res, file="ptsdcur_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs05 + ptsdcur_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_sbp05int_adj_res)
lmOut(ptsdcur_sbp05int_adj_res, file="ptsdcur_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs10 + ptsdcur_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_sbp10int_adj_res)
lmOut(ptsdcur_sbp10int_adj_res, file="ptsdcur_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs50 + ptsdcur_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_sbp50int_adj_res)
lmOut(ptsdcur_sbp50int_adj_res, file="ptsdcur_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables###

#Main effects model# 

ptsdcur_map0001ME_res <- lm(map ~ ptsd_diag_cur + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map0001ME_res)
lmOut(ptsdcur_map0001ME_res, file="ptsdcur_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map001ME_res <- lm(map ~ ptsd_diag_cur + bp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map001ME_res)
lmOut(ptsdcur_map001ME_res, file="ptsdcur_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map01ME_res <- lm(map ~ ptsd_diag_cur + bp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map01ME_res)
lmOut(ptsdcur_map01ME_res, file="ptsdcur_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map05ME_res <- lm(map ~ ptsd_diag_cur + bp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map05ME_res)
lmOut(ptsdcur_map05ME_res, file="ptsdcur_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map10ME_res <- lm(map ~ ptsd_diag_cur + bp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map10ME_res)
lmOut(ptsdcur_map10ME_res, file="ptsdcur_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map50ME_res <- lm(map ~ ptsd_diag_cur + bp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map50ME_res)
lmOut(ptsdcur_map50ME_res, file="ptsdcur_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged$ptsdcur_map0001_i <-merged$ptsd_diag_cur*merged$bp_prs0001

ptsdcur_map0001int_res <- lm(map ~ ptsd_diag_cur + bp_prs0001 + ptsdcur_map0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map0001int_res)
lmOut(ptsdcur_map0001int_res, file="ptsdcur_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_map001_i <-merged$ptsd_diag_cur*merged$bp_prs001

ptsdcur_map001int_res <- lm(map ~ ptsd_diag_cur + bp_prs001 + ptsdcur_map001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map001int_res)
lmOut(ptsdcur_map001int_res, file="ptsdcur_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_map01_i <-merged$ptsd_diag_cur*merged$bp_prs01

ptsdcur_map01int_res <- lm(map ~ ptsd_diag_cur + bp_prs01 + ptsdcur_map01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map01int_res)
lmOut(ptsdcur_map01int_res, file="ptsdcur_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_map05_i <-merged$ptsd_diag_cur*merged$bp_prs05

ptsdcur_map05int_res <- lm(map ~ ptsd_diag_cur + bp_prs05 + ptsdcur_map05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map05int_res)
lmOut(ptsdcur_map05int_res, file="ptsdcur_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_map10_i <-merged$ptsd_diag_cur*merged$bp_prs10

ptsdcur_map10int_res <- lm(map ~ ptsd_diag_cur + bp_prs10 + ptsdcur_map10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map10int_res)
lmOut(ptsdcur_map10int_res, file="ptsdcur_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcur_map50_i <-merged$ptsd_diag_cur*merged$bp_prs50

ptsdcur_map50int_res <- lm(map ~ ptsd_diag_cur + bp_prs50 + ptsdcur_map50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcur_map50int_res)
lmOut(ptsdcur_map50int_res, file="ptsdcur_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcur_map0001int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs0001 + ptsdcur_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_map0001int_adj_res)
lmOut(ptsdcur_map0001int_adj_res, file="ptsdcur_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map001int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs001 + ptsdcur_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_map001int_adj_res)
lmOut(ptsdcur_map001int_adj_res, file="ptsdcur_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map01int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs01 + ptsdcur_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_map01int_adj_res)
lmOut(ptsdcur_map01int_adj_res, file="ptsdcur_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map05int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs05 + ptsdcur_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_map05int_adj_res)
lmOut(ptsdcur_map05int_adj_res, file="ptsdcur_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map10int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs10 + ptsdcur_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_map10int_adj_res)
lmOut(ptsdcur_map10int_adj_res, file="ptsdcur_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map50int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs50 + ptsdcur_map50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcur_map50int_adj_res)
lmOut(ptsdcur_map50int_adj_res, file="ptsdcur_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)

#Step 2d - Current measure of PTSD severity#

###Outcome: Diastolic BP, dbp_prs variables###

#Main effects model# 

ptsdcursx_dbp0001ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp0001ME_res)
lmOut(ptsdcursx_dbp0001ME_res, file="ptsdcursx_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp001ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp001ME_res)
lmOut(ptsdcursx_dbp001ME_res, file="ptsdcursx_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp01ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp01ME_res)
lmOut(ptsdcursx_dbp01ME_res, file="ptsdcursx_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp05ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp05ME_res)
lmOut(ptsdcursx_dbp05ME_res, file="ptsdcursx_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp10ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp10ME_res)
lmOut(ptsdcursx_dbp10ME_res, file="ptsdcursx_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp50ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp50ME_res)
lmOut(ptsdcursx_dbp50ME_res, file="ptsdcursx_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$dbp_prs0001_c <- merged$dbp_prs0001 - mean(merged$dbp_prs0001, na.rm=T)

merged$ptsdcursx_dbp0001_i <-merged$ptsdcursx_c*merged$dbp_prs0001_c

ptsdcursx_dbp0001int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs0001 + ptsdcursx_dbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp0001int_res)
lmOut(ptsdcursx_dbp0001int_res, file="ptsdcursx_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$dbp_prs001_c <- merged$dbp_prs001 - mean(merged$dbp_prs001, na.rm=T)

merged$ptsdcursx_dbp001_i <-merged$ptsdcursx_c*merged$dbp_prs001_c

ptsdcursx_dbp001int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs001 + ptsdcursx_dbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp001int_res)
lmOut(ptsdcursx_dbp001int_res, file="ptsdcursx_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$dbp_prs01_c <- merged$dbp_prs01 - mean(merged$dbp_prs01, na.rm=T)

merged$ptsdcursx_dbp01_i <-merged$ptsdcursx_c*merged$dbp_prs01_c

ptsdcursx_dbp01int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs01 + ptsdcursx_dbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp01int_res)
lmOut(ptsdcursx_dbp01int_res, file="ptsdcursx_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$dbp_prs05_c <- merged$dbp_prs05 - mean(merged$dbp_prs05, na.rm=T)

merged$ptsdcursx_dbp05_i <-merged$ptsdcursx_c*merged$dbp_prs05_c

ptsdcursx_dbp05int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs05 + ptsdcursx_dbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp05int_res)
lmOut(ptsdcursx_dbp05int_res, file="ptsdcursx_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$dbp_prs10_c <- merged$dbp_prs10 - mean(merged$dbp_prs10, na.rm=T)

merged$ptsdcursx_dbp10_i <-merged$ptsdcursx_c*merged$dbp_prs10_c

ptsdcursx_dbp10int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs10 + ptsdcursx_dbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp10int_res)
lmOut(ptsdcursx_dbp10int_res, file="ptsdcursx_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$dbp_prs50_c <- merged$dbp_prs50 - mean(merged$dbp_prs50, na.rm=T)

merged$ptsdcursx_dbp50_i <-merged$ptsdcursx_c*merged$dbp_prs50_c

ptsdcursx_dbp50int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs50 + ptsdcursx_dbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_dbp50int_res)
lmOut(ptsdcursx_dbp50int_res, file="ptsdcursx_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcursx_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs0001 + ptsdcursx_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_dbp0001int_adj_res)
lmOut(ptsdcursx_dbp0001int_adj_res, file="ptsdcursx_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs001 + ptsdcursx_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_dbp001int_adj_res)
lmOut(ptsdcursx_dbp001int_adj_res, file="ptsdcursx_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs01 + ptsdcursx_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_dbp01int_adj_res)
lmOut(ptsdcursx_dbp01int_adj_res, file="ptsdcursx_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs05 + ptsdcursx_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_dbp05int_adj_res)
lmOut(ptsdcursx_dbp05int_adj_res, file="ptsdcursx_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs10 + ptsdcursx_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_dbp10int_adj_res)
lmOut(ptsdcursx_dbp10int_adj_res, file="ptsdcursx_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs50 + ptsdcursx_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_dbp50int_adj_res)
lmOut(ptsdcursx_dbp50int_adj_res, file="ptsdcursx_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdcursx_sbp0001ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp0001ME_res)
lmOut(ptsdcursx_sbp0001ME_res, file="ptsdcursx_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp001ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp001ME_res)
lmOut(ptsdcursx_sbp001ME_res, file="ptsdcursx_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp01ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp01ME_res)
lmOut(ptsdcursx_sbp01ME_res, file="ptsdcursx_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp05ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp05ME_res)
lmOut(ptsdcursx_sbp05ME_res, file="ptsdcursx_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp10ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp10ME_res)
lmOut(ptsdcursx_sbp10ME_res, file="ptsdcursx_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp50ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp50ME_res)
lmOut(ptsdcursx_sbp50ME_res, file="ptsdcursx_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$sbp_prs0001_c <- merged$sbp_prs0001 - mean(merged$sbp_prs0001, na.rm=T)

merged$ptsdcursx_sbp0001_i <-merged$ptsdcursx_c*merged$sbp_prs0001_c

ptsdcursx_sbp0001int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs0001 + ptsdcursx_sbp0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp0001int_res)
lmOut(ptsdcursx_sbp0001int_res, file="ptsdcursx_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$sbp_prs001_c <- merged$sbp_prs001 - mean(merged$sbp_prs001, na.rm=T)

merged$ptsdcursx_sbp001_i <-merged$ptsdcursx_c*merged$sbp_prs001_c

ptsdcursx_sbp001int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs001 + ptsdcursx_sbp001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp001int_res)
lmOut(ptsdcursx_sbp001int_res, file="ptsdcursx_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$sbp_prs01_c <- merged$sbp_prs01 - mean(merged$sbp_prs01, na.rm=T)

merged$ptsdcursx_sbp01_i <-merged$ptsdcursx_c*merged$sbp_prs01_c

ptsdcursx_sbp01int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs01 + ptsdcursx_sbp01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp01int_res)
lmOut(ptsdcursx_sbp01int_res, file="ptsdcursx_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$sbp_prs05_c <- merged$sbp_prs05 - mean(merged$sbp_prs05, na.rm=T)

merged$ptsdcursx_sbp05_i <-merged$ptsdcursx_c*merged$sbp_prs05_c

ptsdcursx_sbp05int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs05 + ptsdcursx_sbp05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp05int_res)
lmOut(ptsdcursx_sbp05int_res, file="ptsdcursx_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$sbp_prs10_c <- merged$sbp_prs10 - mean(merged$sbp_prs10, na.rm=T)

merged$ptsdcursx_sbp10_i <-merged$ptsdcursx_c*merged$sbp_prs10_c

ptsdcursx_sbp10int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs10 + ptsdcursx_sbp10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp10int_res)
lmOut(ptsdcursx_sbp10int_res, file="ptsdcursx_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$sbp_prs50_c <- merged$sbp_prs50 - mean(merged$sbp_prs50, na.rm=T)

merged$ptsdcursx_sbp50_i <-merged$ptsdcursx_c*merged$sbp_prs50_c

ptsdcursx_sbp50int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs50 + ptsdcursx_sbp50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_sbp50int_res)
lmOut(ptsdcursx_sbp50int_res, file="ptsdcursx_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcursx_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs0001 + ptsdcursx_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_sbp0001int_adj_res)
lmOut(ptsdcursx_sbp0001int_adj_res, file="ptsdcursx_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs001 + ptsdcursx_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_sbp001int_adj_res)
lmOut(ptsdcursx_sbp001int_adj_res, file="ptsdcursx_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs01 + ptsdcursx_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_sbp01int_adj_res)
lmOut(ptsdcursx_sbp01int_adj_res, file="ptsdcursx_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs05 + ptsdcursx_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_sbp05int_adj_res)
lmOut(ptsdcursx_sbp05int_adj_res, file="ptsdcursx_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs10 + ptsdcursx_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_sbp10int_adj_res)
lmOut(ptsdcursx_sbp10int_adj_res, file="ptsdcursx_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs50 + ptsdcursx_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_sbp50int_adj_res)
lmOut(ptsdcursx_sbp50int_adj_res, file="ptsdcursx_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables### 

#Main effects model# 

ptsdcursx_map0001ME_res <- lm(map ~ ptsd_sx_cur + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map0001ME_res)
lmOut(ptsdcursx_map0001ME_res, file="ptsdcursx_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map001ME_res <- lm(map ~ ptsd_sx_cur + bp_prs001 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map001ME_res)
lmOut(ptsdcursx_map001ME_res, file="ptsdcursx_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map01ME_res <- lm(map ~ ptsd_sx_cur + bp_prs01 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map01ME_res)
lmOut(ptsdcursx_map01ME_res, file="ptsdcursx_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map05ME_res <- lm(map ~ ptsd_sx_cur + bp_prs05 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map05ME_res)
lmOut(ptsdcursx_map05ME_res, file="ptsdcursx_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map10ME_res <- lm(map ~ ptsd_sx_cur + bp_prs10 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map10ME_res)
lmOut(ptsdcursx_map10ME_res, file="ptsdcursx_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map50ME_res <- lm(map ~ ptsd_sx_cur + bp_prs50 + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map50ME_res)
lmOut(ptsdcursx_map50ME_res, file="ptsdcursx_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$bp_prs0001_c <- merged$bp_prs0001 - mean(merged$bp_prs0001, na.rm=T)

merged$ptsdcursx_map0001_i <-merged$ptsdcursx_c*merged$bp_prs0001_c

ptsdcursx_map0001int_res <- lm(map ~ ptsd_sx_cur + bp_prs0001 + ptsdcursx_map0001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map0001int_res)
lmOut(ptsdcursx_map0001int_res, file="ptsdcursx_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$bp_prs001_c <- merged$bp_prs001 - mean(merged$bp_prs001, na.rm=T)

merged$ptsdcursx_map001_i <-merged$ptsdcursx_c*merged$bp_prs001_c

ptsdcursx_map001int_res <- lm(map ~ ptsd_sx_cur + bp_prs001 + ptsdcursx_map001_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map001int_res)
lmOut(ptsdcursx_map001int_res, file="ptsdcursx_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$bp_prs01_c <- merged$bp_prs01 - mean(merged$bp_prs01, na.rm=T)

merged$ptsdcursx_map01_i <-merged$ptsdcursx_c*merged$bp_prs01_c

ptsdcursx_map01int_res <- lm(map ~ ptsd_sx_cur + bp_prs01 + ptsdcursx_map01_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map01int_res)
lmOut(ptsdcursx_map01int_res, file="ptsdcursx_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$bp_prs05_c <- merged$bp_prs05 - mean(merged$bp_prs05, na.rm=T)

merged$ptsdcursx_map05_i <-merged$ptsdcursx_c*merged$bp_prs05_c

ptsdcursx_map05int_res <- lm(map ~ ptsd_sx_cur + bp_prs05 + ptsdcursx_map05_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map05int_res)
lmOut(ptsdcursx_map05int_res, file="ptsdcursx_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$bp_prs10_c <- merged$bp_prs10 - mean(merged$bp_prs10, na.rm=T)

merged$ptsdcursx_map10_i <-merged$ptsdcursx_c*merged$bp_prs10_c

ptsdcursx_map10int_res <- lm(map ~ ptsd_sx_cur + bp_prs10 + ptsdcursx_map10_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map10int_res)
lmOut(ptsdcursx_map10int_res, file="ptsdcursx_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged$ptsdcursx_c <- merged$ptsd_sx_cur - mean(merged$ptsd_sx_cur, na.rm=T)
merged$bp_prs50_c <- merged$bp_prs50 - mean(merged$bp_prs50, na.rm=T)

merged$ptsdcursx_map50_i <-merged$ptsdcursx_c*merged$bp_prs50_c

ptsdcursx_map50int_res <- lm(map ~ ptsd_sx_cur + bp_prs50 + ptsdcursx_map50_i + sex + age + P1 + P2 + P3, data=merged)
summary(ptsdcursx_map50int_res)
lmOut(ptsdcursx_map50int_res, file="ptsdcursx_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcursx_map0001int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs0001 + ptsdcursx_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_map0001int_adj_res)
lmOut(ptsdcursx_map0001int_adj_res, file="ptsdcursx_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map001int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs001 + ptsdcursx_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_map001int_adj_res)
lmOut(ptsdcursx_map001int_adj_res, file="ptsdcursx_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map01int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs01 + ptsdcursx_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_map01int_adj_res)
lmOut(ptsdcursx_map01int_adj_res, file="ptsdcursx_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map05int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs05 + ptsdcursx_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_map05int_adj_res)
lmOut(ptsdcursx_map05int_adj_res, file="ptsdcursx_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map10int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs10 + ptsdcursx_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_map10int_adj_res)
lmOut(ptsdcursx_map10int_adj_res, file="ptsdcursx_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map50int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs50 + ptsdcursx_map50_i + sex + age + P1 + P2 + P3 + as.factor(as.factor(educ)) + antihtn_use + trauma_count_lt, data=merged)
summary(ptsdcursx_map50int_adj_res)
lmOut(ptsdcursx_map50int_adj_res, file="ptsdcursx_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)



#Below seemed to be pulled from https://gist.github.com/EconometricsBySimulation/6274532

lmOut <- function(res, file="test.csv", ndigit=3, writecsv=T) {
  # If summary has not been run on the model then run summary
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  
  co <- res$coefficients
  nvar <- nrow(co)
  ncol <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4
  
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncol+1)
  
  G[1,1] <- toString(res$call)
  
  # Save rownames and colnames
  G[(nstats+1):(nvar+nstats),1] <- rownames(co)
  G[nstats, 2:(ncol+1)] <- colnames(co)
  
  # Save Coefficients
  G[(nstats+1):(nvar+nstats), 2:(ncol+1)] <- formatter(co)
  
  # Save F-stat
  G[1,2] <- paste0("F(",f[2],",",f[3],")")
  G[2,2] <- formatter(f[1])
  
  # Save F-p value
  G[1,3] <- "Prob > P"
  G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))
  
  # Save R2
  G[1,4] <- "R-Squared"
  G[2,4] <- formatter(res$r.squared)
  
  # Save Adj-R2
  G[1,5] <- "Adj-R2"
  G[2,5] <- formatter(res$adj.r.squared)
  
  print(G)
  if (writecsv) write.csv(G, file=file, row.names=F)
}

lmOut(res)

# First let's generate some fake binary response data (from yesterday's post).
Nobs <- 10^4
X <- cbind(cons=1, X1=rnorm(Nobs),X2=rnorm(Nobs),X3=rnorm(Nobs),u=rnorm(Nobs))
B <- c(B0=-.2, B1=-.1,B2=0,B3=-.2,u=5)
Y <- X%*%B
SData <- as.data.frame(cbind(Y, X))

# Great, we have generated our data.  
myres <- lm(Y ~ X1 + X2 + X3, data=SData)

lmOut(myres, file="my-results.csv")


