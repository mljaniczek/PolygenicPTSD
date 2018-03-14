
library(tidyverse)
library(dplyr)
#############################
#Simulated mergeda for testing #
#############################
library(plyr)
prsfile= "/Users/TinyDragon/github/PolygenicPTSD/data/test.SBP.all.score"
bpprs_pcs <- read.table(prsfile, header=T,stringsAsFactors=F)
bpprs_pcs <- rename(bpprs_pcs, c("X0.01" = "dbp_prs01", "X0.02" = "dbp_prs02", "X0.03" = "sbp_prs03", "X0.04" = "sbp_prs04", "X0.05" = "bp_prs05", "X0.06" = "bp_prs06"))


#We'll note the names of all PRS columns that aren't FID and IID (cols 1 and 2), we're gonna loop over those
#prs_columns <- names(bpprs_pcs)[-c(1:2)]

phenofile <- "/Users/TinyDragon/github/PolygenicPTSD/data/example_bpdata.txt" #File name for phenotype mergeda
pheno <- read.table(phenofile, header=T,stringsAsFactors=F)
pheno <- rename(pheno, c("sbp_meas" = "SBP_meas", "dbp_meas" = "DBP_meas"))

#Mutating to simulate all relevant variables we might need
pheno <- mutate(pheno, 
                 ptsd_diag_lt = sample(c(0,1), replace=TRUE, size=dim(pheno)[1]),
                 ptsd_sx_lt = sample(c(0:10), replace=TRUE, size=dim(pheno)[1]),
                 ptsd_diag_cur = sample(c(0,1), replace=TRUE, size=dim(pheno)[1]),
                 ptsd_sx_cur = sample(c(0:10), replace=TRUE, size=dim(pheno)[1]),
                 P1 = rnorm(dim(pheno)[1]),
                 P2 = rnorm(dim(pheno)[1]),
                 P3 = rnorm(dim(pheno)[1]),
                 afrP1 = rnorm(dim(pheno)[1]),
                 afrP2 = rnorm(dim(pheno)[1]),
                 afrP3 = rnorm(dim(pheno)[1]),
                 eurP1 = rnorm(dim(pheno)[1]),
                 eurP2 = rnorm(dim(pheno)[1]),
                 eurP3 = rnorm(dim(pheno)[1]),        
                 BP = rnorm(dim(pheno)[1]),
                 sex = sample(c(0,1), replace=TRUE, size=dim(pheno)[1]),
                 age = runif(dim(pheno)[1],18,50),
                 educ = sample(c(0, 1, 2, 3), replace = TRUE, size = dim(pheno)[1]),
                 cohort = sample(c(0, 1), replace = TRUE, size = dim(pheno)[1]),
                 antihtn_use = sample(c(0,1), replace = TRUE, size = dim(pheno)[1], prob = c(.8, .2)),
                 antihtn_type = sample(c(0,1, 2, 3, 4, 5, 6), replace = TRUE, size = dim(pheno)[1]),
                 bp_meth = sample(c(0, 1, 2), replace = TRUE, size = dim(pheno)[1]),
                 htn_meth = sample(c(0, 2, 3), replace = TRUE, size = dim(pheno)[1]),
                 bmi = runif(dim(pheno)[1],42,150)/runif(dim(pheno)[1],1.5,1.9), #random weight (kg)/random heigh (meters)
                 bestpop = sample(c("eur", "afr", "aam"), replace = TRUE, size = dim(pheno)[1]),
                 htn_selfr = sample(c(0, 1, 2), replace=TRUE, size=dim(pheno)[1])
                 )

merged <- merge(bpprs_pcs[,1:8], pheno, by=c("FID","IID")) #Just using 3 PRS columns to make it simpler mergedaframe for now

##*Margie to follow-up with Adam about PRS naming convention

polygenicPTSD <- function(data_entry){
  #First create list of PRS variables 
  sbp_prs_vars <- colnames(data_entry)[grepl("sbp_prs", colnames(data_entry))] #selecting column names for sbp PRS
  dbp_prs_vars <- colnames(data_entry)[grepl("dbp_prs", colnames(data_entry))] #selecting column names for dbp PRS
  htn_prs_vars <- colnames(data_entry)[grepl("^bp_prs", colnames(data_entry))]
  ptsd_vars <- colnames(data_entry[grepl("ptsd", colnames(data_entry))]) #selecting the four ptsd variable names
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
  
} #end function loop


#Function for creating regression output .csv files#
#From Jen #

lmOut <- function(res, file="test.csv", ndigit=3, writecsv=T) {
  # If summary has not been run on the model then run summary
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  
  co <- res$coefficients
  nvar <- nrow(co)
  ncol <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4 #**? What is this? should it be programmable above?
  
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncol+1)
  
  G[1,1] <- toString(res$call) #**? I imagine some of the data will be numeric and some string, may want a data frame instead of matrix?
  
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






