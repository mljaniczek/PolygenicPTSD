###########  PTSD Physical Health Group - PTSD and BP PRS Project: Step 2 - Linear Regressions Predicting Continuous BP Measures ##########

###Remember to conduct analyses separately in different ancestry groups in your sample based on "ancestry" variable###

#Have files saved to a folder on your Desktop called "PGC_PTSD_BP" and set working directory to that folder#
#Be sure to change USERNAME so it reflects what your username#
#Select Windows or Mac path by deleting the "#" in front of the setwd command#

#Windows path#
#setwd("C:/Users/USERNAME/Desktop/PGC_PTSD_BP")

#Mac path#
#setwd("Desktop/PGC_PTSD_BP")

#Function for creating regression output .csv files#

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

#Read in merged data file#

merged.data <- read.csv("merged.data.csv")

###Step 2a - Lifetime measure of PTSD diagnosis### 

###Outcome: Diastolic BP, dbp_prs variables###

##*New loop for multiple dbp_prs
#First create list of PRS variables 
prsVars <- colnames(merged.data)[grepl("dbp_prs", colnames(merged.data))] #confirm that all prs levels have "prs" and no other variables have it
#dpbPrs <- merged.data[dbpVars] if we need subset of columns with prs

#Main Effects Loop
for (i in 1:length(prsVars)){  #Loop runs through columns of dpbPrs
  tryCatch({ #Adding this in case there is an error in any regression
    level <- prsVars[i] #
    #Main effects model first
    assign(paste("ptsdlt_res_ME_", level, sep = ""), lm(DBP_meas ~ ptsd_diag_lt + level + sex + age + P1 + P2 + P3, data=merged.data))
    filename <- as.character(paste("ptsdlt_res_summary_ME_", level, ".csv", sep="")) 
    lmOut(paste("ptsdlt_res_ME_", level, sep = ""), file=filename, ndigit=3, writecsv=T)
    #Interaction model below
    assign(paste("ptsdlt_res_Int_", level, sep = ""), lm(DBP_meas ~ ptsd_diag_lt*level + sex + age + P1 + P2 + P3, data=merged.data))
    filenameint <- as.character(paste("ptsdlt_res_summary_Int_", level, ".csv", sep="")) 
    lmOut(paste("ptsdlt_res_Int_", level, sep = ""), file=filenameint, ndigit=3, writecsv=T)
  }, #Trycatch end curly
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
} #end loop

#Interaction loop
for (i in 1:length(prsVars)){  #Loop runs through columns of dpbPrs
  tryCatch({ #Adding this in case there is an error in any regression
    level <- prsVars[i]
    assign(paste("ptsdlt_res_Int_", level, sep = ""), lm(DBP_meas ~ ptsd_diag_lt*level + sex + age + P1 + P2 + P3, data=merged.data))
    #summary(ptsdlt_dbp0001ME_res) #*Do we need this in the loop?
    filenameint <- as.character(paste("ptsdlt_res_summary_Int_", level, ".csv", sep="")) 
    lmOut(paste("ptsdlt_res_Int_", level, sep = ""), file=filenameint, ndigit=3, writecsv=T)
  }, #Trycatch end curly
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
} #end loop


#Old Code

#Main effects model# 
ptsdlt_dbp0001ME_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp0001ME_res)
lmOut(ptsdlt_dbp0001ME_res, file="ptsdlt_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp001ME_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp001ME_res)
lmOut(ptsdlt_dbp001ME_res, file="ptsdlt_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp01ME_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp01ME_res)
lmOut(ptsdlt_dbp01ME_res, file="ptsdlt_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp05ME_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp05ME_res)
lmOut(ptsdlt_dbp05ME_res, file="ptsdlt_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp10ME_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp10ME_res)
lmOut(ptsdlt_dbp10ME_res, file="ptsdlt_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp50ME_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp50ME_res)
lmOut(ptsdlt_dbp50ME_res, file="ptsdlt_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged.data$ptsdlt_dbp0001_i <-merged.data$ptsd_diag_lt*merged.data$dbp_prs0001

ptsdlt_dbp0001int_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs0001 + ptsdlt_dbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp0001int_res)
lmOut(ptsdlt_dbp0001int_res, file="ptsdlt_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_dbp001_i <-merged.data$ptsd_diag_lt*merged.data$dbp_prs001

ptsdlt_dbp001int_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs001 + ptsdlt_dbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp001int_res)
lmOut(ptsdlt_dbp001int_res, file="ptsdlt_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_dbp01_i <-merged.data$ptsd_diag_lt*merged.data$dbp_prs01

ptsdlt_dbp01int_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs01 + ptsdlt_dbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp01int_res)
lmOut(ptsdlt_dbp01int_res, file="ptsdlt_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_dbp05_i <-merged.data$ptsd_diag_lt*merged.data$dbp_prs05

ptsdlt_dbp05int_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs05 + ptsdlt_dbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp05int_res)
lmOut(ptsdlt_dbp05int_res, file="ptsdlt_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_dbp10_i <-merged.data$ptsd_diag_lt*merged.data$dbp_prs10

ptsdlt_dbp10int_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs10 + ptsdlt_dbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp10int_res)
lmOut(ptsdlt_dbp10int_res, file="ptsdlt_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_dbp50_i <-merged.data$ptsd_diag_lt*merged.data$dbp_prs50

ptsdlt_dbp50int_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs50 + ptsdlt_dbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_dbp50int_res)
lmOut(ptsdlt_dbp50int_res, file="ptsdlt_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdlt_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs0001 + ptsdlt_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_dbp0001int_adj_res)
lmOut(ptsdlt_dbp0001int_adj_res, file="ptsdlt_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs001 + ptsdlt_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_dbp001int_adj_res)
lmOut(ptsdlt_dbp001int_adj_res, file="ptsdlt_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs01 + ptsdlt_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_dbp01int_adj_res)
lmOut(ptsdlt_dbp01int_adj_res, file="ptsdlt_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs05 + ptsdlt_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_dbp05int_adj_res)
lmOut(ptsdlt_dbp05int_adj_res, file="ptsdlt_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs10 + ptsdlt_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_dbp10int_adj_res)
lmOut(ptsdlt_dbp10int_adj_res, file="ptsdlt_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_diag_lt + dbp_prs50 + ptsdlt_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_dbp50int_adj_res)
lmOut(ptsdlt_dbp50int_adj_res, file="ptsdlt_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdlt_sbp0001ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp0001ME_res)
lmOut(ptsdlt_sbp0001ME_res, file="ptsdlt_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp001ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp001ME_res)
lmOut(ptsdlt_sbp001ME_res, file="ptsdlt_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp01ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp01ME_res)
lmOut(ptsdlt_sbp01ME_res, file="ptsdlt_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp05ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp05ME_res)
lmOut(ptsdlt_sbp05ME_res, file="ptsdlt_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp10ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp10ME_res)
lmOut(ptsdlt_sbp10ME_res, file="ptsdlt_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp50ME_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp50ME_res)
lmOut(ptsdlt_sbp50ME_res, file="ptsdlt_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged.data$ptsdlt_sbp0001_i <-merged.data$ptsd_diag_lt*merged.data$sbp_prs0001

ptsdlt_sbp0001int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs0001 + ptsdlt_sbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp0001int_res)
lmOut(ptsdlt_sbp0001int_res, file="ptsdlt_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_sbp001_i <-merged.data$ptsd_diag_lt*merged.data$sbp_prs001

ptsdlt_sbp001int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs001 + ptsdlt_sbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp001int_res)
lmOut(ptsdlt_sbp001int_res, file="ptsdlt_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_sbp01_i <-merged.data$ptsd_diag_lt*merged.data$sbp_prs01

ptsdlt_sbp01int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs01 + ptsdlt_sbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp01int_res)
lmOut(ptsdlt_sbp01int_res, file="ptsdlt_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_sbp05_i <-merged.data$ptsd_diag_lt*merged.data$sbp_prs05

ptsdlt_sbp05int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs05 + ptsdlt_sbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp05int_res)
lmOut(ptsdlt_sbp05int_res, file="ptsdlt_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_sbp10_i <-merged.data$ptsd_diag_lt*merged.data$sbp_prs10

ptsdlt_sbp10int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs10 + ptsdlt_sbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp10int_res)
lmOut(ptsdlt_sbp10int_res, file="ptsdlt_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_sbp50_i <-merged.data$ptsd_diag_lt*merged.data$sbp_prs50

ptsdlt_sbp50int_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs50 + ptsdlt_sbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_sbp50int_res)
lmOut(ptsdlt_sbp50int_res, file="ptsdlt_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdlt_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs0001 + ptsdlt_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_sbp0001int_adj_res)
lmOut(ptsdlt_sbp0001int_adj_res, file="ptsdlt_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs001 + ptsdlt_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_sbp001int_adj_res)
lmOut(ptsdlt_sbp001int_adj_res, file="ptsdlt_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs01 + ptsdlt_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_sbp01int_adj_res)
lmOut(ptsdlt_sbp01int_adj_res, file="ptsdlt_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs05 + ptsdlt_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_sbp05int_adj_res)
lmOut(ptsdlt_sbp05int_adj_res, file="ptsdlt_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs10 + ptsdlt_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_sbp10int_adj_res)
lmOut(ptsdlt_sbp10int_adj_res, file="ptsdlt_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_diag_lt + sbp_prs50 + ptsdlt_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_sbp50int_adj_res)
lmOut(ptsdlt_sbp50int_adj_res, file="ptsdlt_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables###

#Main effects model# 

ptsdlt_map0001ME_res <- lm(map ~ ptsd_diag_lt + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map0001ME_res)
lmOut(ptsdlt_map0001ME_res, file="ptsdlt_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map001ME_res <- lm(map ~ ptsd_diag_lt + bp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map001ME_res)
lmOut(ptsdlt_map001ME_res, file="ptsdlt_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map01ME_res <- lm(map ~ ptsd_diag_lt + bp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map01ME_res)
lmOut(ptsdlt_map01ME_res, file="ptsdlt_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map05ME_res <- lm(map ~ ptsd_diag_lt + bp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map05ME_res)
lmOut(ptsdlt_map05ME_res, file="ptsdlt_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map10ME_res <- lm(map ~ ptsd_diag_lt + bp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map10ME_res)
lmOut(ptsdlt_map10ME_res, file="ptsdlt_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map50ME_res <- lm(map ~ ptsd_diag_lt + bp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map50ME_res)
lmOut(ptsdlt_map50ME_res, file="ptsdlt_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged.data$ptsdlt_map0001_i <-merged.data$ptsd_diag_lt*merged.data$bp_prs0001

ptsdlt_map0001int_res <- lm(map ~ ptsd_diag_lt + bp_prs0001 + ptsdlt_map0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map0001int_res)
lmOut(ptsdlt_map0001int_res, file="ptsdlt_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_map001_i <-merged.data$ptsd_diag_lt*merged.data$bp_prs001

ptsdlt_map001int_res <- lm(map ~ ptsd_diag_lt + bp_prs001 + ptsdlt_map001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map001int_res)
lmOut(ptsdlt_map001int_res, file="ptsdlt_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_map01_i <-merged.data$ptsd_diag_lt*merged.data$bp_prs01

ptsdlt_map01int_res <- lm(map ~ ptsd_diag_lt + bp_prs01 + ptsdlt_map01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map01int_res)
lmOut(ptsdlt_map01int_res, file="ptsdlt_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_map05_i <-merged.data$ptsd_diag_lt*merged.data$bp_prs05

ptsdlt_map05int_res <- lm(map ~ ptsd_diag_lt + bp_prs05 + ptsdlt_map05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map05int_res)
lmOut(ptsdlt_map05int_res, file="ptsdlt_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_map10_i <-merged.data$ptsd_diag_lt*merged.data$bp_prs10

ptsdlt_map10int_res <- lm(map ~ ptsd_diag_lt + bp_prs10 + ptsdlt_map10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map10int_res)
lmOut(ptsdlt_map10int_res, file="ptsdlt_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdlt_map50_i <-merged.data$ptsd_diag_lt*merged.data$bp_prs50

ptsdlt_map50int_res <- lm(map ~ ptsd_diag_lt + bp_prs50 + ptsdlt_map50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdlt_map50int_res)
lmOut(ptsdlt_map50int_res, file="ptsdlt_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdlt_map0001int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs0001 + ptsdlt_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_map0001int_adj_res)
lmOut(ptsdlt_map0001int_adj_res, file="ptsdlt_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map001int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs001 + ptsdlt_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_map001int_adj_res)
lmOut(ptsdlt_map001int_adj_res, file="ptsdlt_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map01int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs01 + ptsdlt_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_map01int_adj_res)
lmOut(ptsdlt_map01int_adj_res, file="ptsdlt_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map05int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs05 + ptsdlt_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_map05int_adj_res)
lmOut(ptsdlt_map05int_adj_res, file="ptsdlt_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map10int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs10 + ptsdlt_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_map10int_adj_res)
lmOut(ptsdlt_map10int_adj_res, file="ptsdlt_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdlt_map50int_adj_res <- lm(map ~ ptsd_diag_lt + bp_prs50 + ptsdlt_map50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdlt_map50int_adj_res)
lmOut(ptsdlt_map50int_adj_res, file="ptsdlt_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Step 2b - Lifetime measure of PTSD severity###

###Outcome: Diastolic BP, dbp_prs variables###

#Main effects model# 

ptsdltsx_dbp0001ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp0001ME_res)
lmOut(ptsdltsx_dbp0001ME_res, file="ptsdltsx_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp001ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp001ME_res)
lmOut(ptsdltsx_dbp001ME_res, file="ptsdltsx_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp01ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp01ME_res)
lmOut(ptsdltsx_dbp01ME_res, file="ptsdltsx_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp05ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp05ME_res)
lmOut(ptsdltsx_dbp05ME_res, file="ptsdltsx_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp10ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp10ME_res)
lmOut(ptsdltsx_dbp10ME_res, file="ptsdltsx_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp50ME_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp50ME_res)
lmOut(ptsdltsx_dbp50ME_res, file="ptsdltsx_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$dbp_prs0001_c <- merged.data$dbp_prs0001 - mean(merged.data$dbp_prs0001, na.rm=T)

merged.data$ptsdltsx_dbp0001_i <-merged.data$ptsdltsx_c*merged.data$dbp_prs0001_c

ptsdltsx_dbp0001int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs0001 + ptsdltsx_dbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp0001int_res)
lmOut(ptsdltsx_dbp0001int_res, file="ptsdltsx_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$dbp_prs001_c <- merged.data$dbp_prs001 - mean(merged.data$dbp_prs001, na.rm=T)

merged.data$ptsdltsx_dbp001_i <-merged.data$ptsdltsx_c*merged.data$dbp_prs001_c

ptsdltsx_dbp001int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs001 + ptsdltsx_dbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp001int_res)
lmOut(ptsdltsx_dbp001int_res, file="ptsdltsx_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$dbp_prs01_c <- merged.data$dbp_prs01 - mean(merged.data$dbp_prs01, na.rm=T)

merged.data$ptsdltsx_dbp01_i <-merged.data$ptsdltsx_c*merged.data$dbp_prs01_c

ptsdltsx_dbp01int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs01 + ptsdltsx_dbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp01int_res)
lmOut(ptsdltsx_dbp01int_res, file="ptsdltsx_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$dbp_prs05_c <- merged.data$dbp_prs05 - mean(merged.data$dbp_prs05, na.rm=T)

merged.data$ptsdltsx_dbp05_i <-merged.data$ptsdltsx_c*merged.data$dbp_prs05_c

ptsdltsx_dbp05int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs05 + ptsdltsx_dbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp05int_res)
lmOut(ptsdltsx_dbp05int_res, file="ptsdltsx_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$dbp_prs10_c <- merged.data$dbp_prs10 - mean(merged.data$dbp_prs10, na.rm=T)

merged.data$ptsdltsx_dbp10_i <-merged.data$ptsdltsx_c*merged.data$dbp_prs10_c

ptsdltsx_dbp10int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs10 + ptsdltsx_dbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp10int_res)
lmOut(ptsdltsx_dbp10int_res, file="ptsdltsx_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$dbp_prs50_c <- merged.data$dbp_prs50 - mean(merged.data$dbp_prs50, na.rm=T)

merged.data$ptsdltsx_dbp50_i <-merged.data$ptsdltsx_c*merged.data$dbp_prs50_c

ptsdltsx_dbp50int_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs50 + ptsdltsx_dbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_dbp50int_res)
lmOut(ptsdltsx_dbp50int_res, file="ptsdltsx_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdltsx_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs0001 + ptsdltsx_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_dbp0001int_adj_res)
lmOut(ptsdltsx_dbp0001int_adj_res, file="ptsdltsx_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs001 + ptsdltsx_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_dbp001int_adj_res)
lmOut(ptsdltsx_dbp001int_adj_res, file="ptsdltsx_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs01 + ptsdltsx_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_dbp01int_adj_res)
lmOut(ptsdltsx_dbp01int_adj_res, file="ptsdltsx_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs05 + ptsdltsx_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_dbp05int_adj_res)
lmOut(ptsdltsx_dbp05int_adj_res, file="ptsdltsx_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs10 + ptsdltsx_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_dbp10int_adj_res)
lmOut(ptsdltsx_dbp10int_adj_res, file="ptsdltsx_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_sx_lt + dbp_prs50 + ptsdltsx_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_dbp50int_adj_res)
lmOut(ptsdltsx_dbp50int_adj_res, file="ptsdltsx_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdltsx_sbp0001ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp0001ME_res)
lmOut(ptsdltsx_sbp0001ME_res, file="ptsdltsx_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp001ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp001ME_res)
lmOut(ptsdltsx_sbp001ME_res, file="ptsdltsx_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp01ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp01ME_res)
lmOut(ptsdltsx_sbp01ME_res, file="ptsdltsx_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp05ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp05ME_res)
lmOut(ptsdltsx_sbp05ME_res, file="ptsdltsx_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp10ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp10ME_res)
lmOut(ptsdltsx_sbp10ME_res, file="ptsdltsx_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp50ME_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp50ME_res)
lmOut(ptsdltsx_sbp50ME_res, file="ptsdltsx_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$sbp_prs0001_c <- merged.data$sbp_prs0001 - mean(merged.data$sbp_prs0001, na.rm=T)

merged.data$ptsdltsx_sbp0001_i <-merged.data$ptsdltsx_c*merged.data$sbp_prs0001_c

ptsdltsx_sbp0001int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs0001 + ptsdltsx_sbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp0001int_res)
lmOut(ptsdltsx_sbp0001int_res, file="ptsdltsx_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$sbp_prs001_c <- merged.data$sbp_prs001 - mean(merged.data$sbp_prs001, na.rm=T)

merged.data$ptsdltsx_sbp001_i <-merged.data$ptsdltsx_c*merged.data$sbp_prs001_c

ptsdltsx_sbp001int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs001 + ptsdltsx_sbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp001int_res)
lmOut(ptsdltsx_sbp001int_res, file="ptsdltsx_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$sbp_prs01_c <- merged.data$sbp_prs01 - mean(merged.data$sbp_prs01, na.rm=T)

merged.data$ptsdltsx_sbp01_i <-merged.data$ptsdltsx_c*merged.data$sbp_prs01_c

ptsdltsx_sbp01int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs01 + ptsdltsx_sbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp01int_res)
lmOut(ptsdltsx_sbp01int_res, file="ptsdltsx_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$sbp_prs05_c <- merged.data$sbp_prs05 - mean(merged.data$sbp_prs05, na.rm=T)

merged.data$ptsdltsx_sbp05_i <-merged.data$ptsdltsx_c*merged.data$sbp_prs05_c

ptsdltsx_sbp05int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs05 + ptsdltsx_sbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp05int_res)
lmOut(ptsdltsx_sbp05int_res, file="ptsdltsx_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$sbp_prs10_c <- merged.data$sbp_prs10 - mean(merged.data$sbp_prs10, na.rm=T)

merged.data$ptsdltsx_sbp10_i <-merged.data$ptsdltsx_c*merged.data$sbp_prs10_c

ptsdltsx_sbp10int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs10 + ptsdltsx_sbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp10int_res)
lmOut(ptsdltsx_sbp10int_res, file="ptsdltsx_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$sbp_prs50_c <- merged.data$sbp_prs50 - mean(merged.data$sbp_prs50, na.rm=T)

merged.data$ptsdltsx_sbp50_i <-merged.data$ptsdltsx_c*merged.data$sbp_prs50_c

ptsdltsx_sbp50int_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs50 + ptsdltsx_sbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_sbp50int_res)
lmOut(ptsdltsx_sbp50int_res, file="ptsdltsx_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdltsx_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs0001 + ptsdltsx_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_sbp0001int_adj_res)
lmOut(ptsdltsx_sbp0001int_adj_res, file="ptsdltsx_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs001 + ptsdltsx_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_sbp001int_adj_res)
lmOut(ptsdltsx_sbp001int_adj_res, file="ptsdltsx_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs01 + ptsdltsx_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_sbp01int_adj_res)
lmOut(ptsdltsx_sbp01int_adj_res, file="ptsdltsx_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs05 + ptsdltsx_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_sbp05int_adj_res)
lmOut(ptsdltsx_sbp05int_adj_res, file="ptsdltsx_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs10 + ptsdltsx_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_sbp10int_adj_res)
lmOut(ptsdltsx_sbp10int_adj_res, file="ptsdltsx_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_sx_lt + sbp_prs50 + ptsdltsx_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_sbp50int_adj_res)
lmOut(ptsdltsx_sbp50int_adj_res, file="ptsdltsx_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables### 

#Main effects model# 

ptsdltsx_map0001ME_res <- lm(map ~ ptsd_sx_lt + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map0001ME_res)
lmOut(ptsdltsx_map0001ME_res, file="ptsdltsx_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map001ME_res <- lm(map ~ ptsd_sx_lt + bp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map001ME_res)
lmOut(ptsdltsx_map001ME_res, file="ptsdltsx_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map01ME_res <- lm(map ~ ptsd_sx_lt + bp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map01ME_res)
lmOut(ptsdltsx_map01ME_res, file="ptsdltsx_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map05ME_res <- lm(map ~ ptsd_sx_lt + bp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map05ME_res)
lmOut(ptsdltsx_map05ME_res, file="ptsdltsx_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map10ME_res <- lm(map ~ ptsd_sx_lt + bp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map10ME_res)
lmOut(ptsdltsx_map10ME_res, file="ptsdltsx_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map50ME_res <- lm(map ~ ptsd_sx_lt + bp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map50ME_res)
lmOut(ptsdltsx_map50ME_res, file="ptsdltsx_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$bp_prs0001_c <- merged.data$bp_prs0001 - mean(merged.data$bp_prs0001, na.rm=T)

merged.data$ptsdltsx_map0001_i <-merged.data$ptsdltsx_c*merged.data$bp_prs0001_c

ptsdltsx_map0001int_res <- lm(map ~ ptsd_sx_lt + bp_prs0001 + ptsdltsx_map0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map0001int_res)
lmOut(ptsdltsx_map0001int_res, file="ptsdltsx_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$bp_prs001_c <- merged.data$bp_prs001 - mean(merged.data$bp_prs001, na.rm=T)

merged.data$ptsdltsx_map001_i <-merged.data$ptsdltsx_c*merged.data$bp_prs001_c

ptsdltsx_map001int_res <- lm(map ~ ptsd_sx_lt + bp_prs001 + ptsdltsx_map001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map001int_res)
lmOut(ptsdltsx_map001int_res, file="ptsdltsx_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$bp_prs01_c <- merged.data$bp_prs01 - mean(merged.data$bp_prs01, na.rm=T)

merged.data$ptsdltsx_map01_i <-merged.data$ptsdltsx_c*merged.data$bp_prs01_c

ptsdltsx_map01int_res <- lm(map ~ ptsd_sx_lt + bp_prs01 + ptsdltsx_map01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map01int_res)
lmOut(ptsdltsx_map01int_res, file="ptsdltsx_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$bp_prs05_c <- merged.data$bp_prs05 - mean(merged.data$bp_prs05, na.rm=T)

merged.data$ptsdltsx_map05_i <-merged.data$ptsdltsx_c*merged.data$bp_prs05_c

ptsdltsx_map05int_res <- lm(map ~ ptsd_sx_lt + bp_prs05 + ptsdltsx_map05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map05int_res)
lmOut(ptsdltsx_map05int_res, file="ptsdltsx_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$bp_prs10_c <- merged.data$bp_prs10 - mean(merged.data$bp_prs10, na.rm=T)

merged.data$ptsdltsx_map10_i <-merged.data$ptsdltsx_c*merged.data$bp_prs10_c

ptsdltsx_map10int_res <- lm(map ~ ptsd_sx_lt + bp_prs10 + ptsdltsx_map10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map10int_res)
lmOut(ptsdltsx_map10int_res, file="ptsdltsx_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdltsx_c <- merged.data$ptsd_sx_lt - mean(merged.data$ptsd_sx_lt, na.rm=T)
merged.data$bp_prs50_c <- merged.data$bp_prs50 - mean(merged.data$bp_prs50, na.rm=T)

merged.data$ptsdltsx_map50_i <-merged.data$ptsdltsx_c*merged.data$bp_prs50_c

ptsdltsx_map50int_res <- lm(map ~ ptsd_sx_lt + bp_prs50 + ptsdltsx_map50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdltsx_map50int_res)
lmOut(ptsdltsx_map50int_res, file="ptsdltsx_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdltsx_map0001int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs0001 + ptsdltsx_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_map0001int_adj_res)
lmOut(ptsdltsx_map0001int_adj_res, file="ptsdltsx_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map001int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs001 + ptsdltsx_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_map001int_adj_res)
lmOut(ptsdltsx_map001int_adj_res, file="ptsdltsx_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map01int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs01 + ptsdltsx_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_map01int_adj_res)
lmOut(ptsdltsx_map01int_adj_res, file="ptsdltsx_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map05int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs05 + ptsdltsx_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_map05int_adj_res)
lmOut(ptsdltsx_map05int_adj_res, file="ptsdltsx_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map10int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs10 + ptsdltsx_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_map10int_adj_res)
lmOut(ptsdltsx_map10int_adj_res, file="ptsdltsx_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdltsx_map50int_adj_res <- lm(map ~ ptsd_sx_lt + bp_prs50 + ptsdltsx_map50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdltsx_map50int_adj_res)
lmOut(ptsdltsx_map50int_adj_res, file="ptsdltsx_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Step 2c - Current measure of PTSD diagnosis###

###Outcome: Diastolic BP, dbp_prs variables###

#Main effects model# 

ptsdcur_dbp0001ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp0001ME_res)
lmOut(ptsdcur_dbp0001ME_res, file="ptsdcur_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp001ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp001ME_res)
lmOut(ptsdcur_dbp001ME_res, file="ptsdcur_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp01ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp01ME_res)
lmOut(ptsdcur_dbp01ME_res, file="ptsdcur_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp05ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp05ME_res)
lmOut(ptsdcur_dbp05ME_res, file="ptsdcur_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp10ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp10ME_res)
lmOut(ptsdcur_dbp10ME_res, file="ptsdcur_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp50ME_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp50ME_res)
lmOut(ptsdcur_dbp50ME_res, file="ptsdcur_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged.data$ptsdcur_dbp0001_i <-merged.data$ptsd_diag_cur*merged.data$dbp_prs0001

ptsdcur_dbp0001int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs0001 + ptsdcur_dbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp0001int_res)
lmOut(ptsdcur_dbp0001int_res, file="ptsdcur_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_dbp001_i <-merged.data$ptsd_diag_cur*merged.data$dbp_prs001

ptsdcur_dbp001int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs001 + ptsdcur_dbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp001int_res)
lmOut(ptsdcur_dbp001int_res, file="ptsdcur_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_dbp01_i <-merged.data$ptsd_diag_cur*merged.data$dbp_prs01

ptsdcur_dbp01int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs01 + ptsdcur_dbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp01int_res)
lmOut(ptsdcur_dbp01int_res, file="ptsdcur_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_dbp05_i <-merged.data$ptsd_diag_cur*merged.data$dbp_prs05

ptsdcur_dbp05int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs05 + ptsdcur_dbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp05int_res)
lmOut(ptsdcur_dbp05int_res, file="ptsdcur_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_dbp10_i <-merged.data$ptsd_diag_cur*merged.data$dbp_prs10

ptsdcur_dbp10int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs10 + ptsdcur_dbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp10int_res)
lmOut(ptsdcur_dbp10int_res, file="ptsdcur_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_dbp50_i <-merged.data$ptsd_diag_cur*merged.data$dbp_prs50

ptsdcur_dbp50int_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs50 + ptsdcur_dbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_dbp50int_res)
lmOut(ptsdcur_dbp50int_res, file="ptsdcur_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcur_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs0001 + ptsdcur_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_dbp0001int_adj_res)
lmOut(ptsdcur_dbp0001int_adj_res, file="ptsdcur_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs001 + ptsdcur_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_dbp001int_adj_res)
lmOut(ptsdcur_dbp001int_adj_res, file="ptsdcur_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs01 + ptsdcur_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_dbp01int_adj_res)
lmOut(ptsdcur_dbp01int_adj_res, file="ptsdcur_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs05 + ptsdcur_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_dbp05int_adj_res)
lmOut(ptsdcur_dbp05int_adj_res, file="ptsdcur_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs10 + ptsdcur_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_dbp10int_adj_res)
lmOut(ptsdcur_dbp10int_adj_res, file="ptsdcur_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_diag_cur + dbp_prs50 + ptsdcur_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_dbp50int_adj_res)
lmOut(ptsdcur_dbp50int_adj_res, file="ptsdcur_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdcur_sbp0001ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp0001ME_res)
lmOut(ptsdcur_sbp0001ME_res, file="ptsdcur_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp001ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp001ME_res)
lmOut(ptsdcur_sbp001ME_res, file="ptsdcur_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp01ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp01ME_res)
lmOut(ptsdcur_sbp01ME_res, file="ptsdcur_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp05ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp05ME_res)
lmOut(ptsdcur_sbp05ME_res, file="ptsdcur_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp10ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp10ME_res)
lmOut(ptsdcur_sbp10ME_res, file="ptsdcur_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp50ME_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp50ME_res)
lmOut(ptsdcur_sbp50ME_res, file="ptsdcur_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged.data$ptsdcur_sbp0001_i <-merged.data$ptsd_diag_cur*merged.data$sbp_prs0001

ptsdcur_sbp0001int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs0001 + ptsdcur_sbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp0001int_res)
lmOut(ptsdcur_sbp0001int_res, file="ptsdcur_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_sbp001_i <-merged.data$ptsd_diag_cur*merged.data$sbp_prs001

ptsdcur_sbp001int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs001 + ptsdcur_sbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp001int_res)
lmOut(ptsdcur_sbp001int_res, file="ptsdcur_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_sbp01_i <-merged.data$ptsd_diag_cur*merged.data$sbp_prs01

ptsdcur_sbp01int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs01 + ptsdcur_sbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp01int_res)
lmOut(ptsdcur_sbp01int_res, file="ptsdcur_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_sbp05_i <-merged.data$ptsd_diag_cur*merged.data$sbp_prs05

ptsdcur_sbp05int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs05 + ptsdcur_sbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp05int_res)
lmOut(ptsdcur_sbp05int_res, file="ptsdcur_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_sbp10_i <-merged.data$ptsd_diag_cur*merged.data$sbp_prs10

ptsdcur_sbp10int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs10 + ptsdcur_sbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp10int_res)
lmOut(ptsdcur_sbp10int_res, file="ptsdcur_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_sbp50_i <-merged.data$ptsd_diag_cur*merged.data$sbp_prs50

ptsdcur_sbp50int_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs50 + ptsdcur_sbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_sbp50int_res)
lmOut(ptsdcur_sbp50int_res, file="ptsdcur_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcur_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs0001 + ptsdcur_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_sbp0001int_adj_res)
lmOut(ptsdcur_sbp0001int_adj_res, file="ptsdcur_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs001 + ptsdcur_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_sbp001int_adj_res)
lmOut(ptsdcur_sbp001int_adj_res, file="ptsdcur_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs01 + ptsdcur_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_sbp01int_adj_res)
lmOut(ptsdcur_sbp01int_adj_res, file="ptsdcur_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs05 + ptsdcur_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_sbp05int_adj_res)
lmOut(ptsdcur_sbp05int_adj_res, file="ptsdcur_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs10 + ptsdcur_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_sbp10int_adj_res)
lmOut(ptsdcur_sbp10int_adj_res, file="ptsdcur_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_diag_cur + sbp_prs50 + ptsdcur_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_sbp50int_adj_res)
lmOut(ptsdcur_sbp50int_adj_res, file="ptsdcur_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables###

#Main effects model# 

ptsdcur_map0001ME_res <- lm(map ~ ptsd_diag_cur + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map0001ME_res)
lmOut(ptsdcur_map0001ME_res, file="ptsdcur_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map001ME_res <- lm(map ~ ptsd_diag_cur + bp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map001ME_res)
lmOut(ptsdcur_map001ME_res, file="ptsdcur_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map01ME_res <- lm(map ~ ptsd_diag_cur + bp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map01ME_res)
lmOut(ptsdcur_map01ME_res, file="ptsdcur_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map05ME_res <- lm(map ~ ptsd_diag_cur + bp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map05ME_res)
lmOut(ptsdcur_map05ME_res, file="ptsdcur_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map10ME_res <- lm(map ~ ptsd_diag_cur + bp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map10ME_res)
lmOut(ptsdcur_map10ME_res, file="ptsdcur_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map50ME_res <- lm(map ~ ptsd_diag_cur + bp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map50ME_res)
lmOut(ptsdcur_map50ME_res, file="ptsdcur_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#

merged.data$ptsdcur_map0001_i <-merged.data$ptsd_diag_cur*merged.data$bp_prs0001

ptsdcur_map0001int_res <- lm(map ~ ptsd_diag_cur + bp_prs0001 + ptsdcur_map0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map0001int_res)
lmOut(ptsdcur_map0001int_res, file="ptsdcur_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_map001_i <-merged.data$ptsd_diag_cur*merged.data$bp_prs001

ptsdcur_map001int_res <- lm(map ~ ptsd_diag_cur + bp_prs001 + ptsdcur_map001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map001int_res)
lmOut(ptsdcur_map001int_res, file="ptsdcur_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_map01_i <-merged.data$ptsd_diag_cur*merged.data$bp_prs01

ptsdcur_map01int_res <- lm(map ~ ptsd_diag_cur + bp_prs01 + ptsdcur_map01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map01int_res)
lmOut(ptsdcur_map01int_res, file="ptsdcur_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_map05_i <-merged.data$ptsd_diag_cur*merged.data$bp_prs05

ptsdcur_map05int_res <- lm(map ~ ptsd_diag_cur + bp_prs05 + ptsdcur_map05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map05int_res)
lmOut(ptsdcur_map05int_res, file="ptsdcur_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_map10_i <-merged.data$ptsd_diag_cur*merged.data$bp_prs10

ptsdcur_map10int_res <- lm(map ~ ptsd_diag_cur + bp_prs10 + ptsdcur_map10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map10int_res)
lmOut(ptsdcur_map10int_res, file="ptsdcur_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcur_map50_i <-merged.data$ptsd_diag_cur*merged.data$bp_prs50

ptsdcur_map50int_res <- lm(map ~ ptsd_diag_cur + bp_prs50 + ptsdcur_map50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcur_map50int_res)
lmOut(ptsdcur_map50int_res, file="ptsdcur_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcur_map0001int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs0001 + ptsdcur_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_map0001int_adj_res)
lmOut(ptsdcur_map0001int_adj_res, file="ptsdcur_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map001int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs001 + ptsdcur_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_map001int_adj_res)
lmOut(ptsdcur_map001int_adj_res, file="ptsdcur_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map01int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs01 + ptsdcur_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_map01int_adj_res)
lmOut(ptsdcur_map01int_adj_res, file="ptsdcur_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map05int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs05 + ptsdcur_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_map05int_adj_res)
lmOut(ptsdcur_map05int_adj_res, file="ptsdcur_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map10int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs10 + ptsdcur_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_map10int_adj_res)
lmOut(ptsdcur_map10int_adj_res, file="ptsdcur_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcur_map50int_adj_res <- lm(map ~ ptsd_diag_cur + bp_prs50 + ptsdcur_map50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcur_map50int_adj_res)
lmOut(ptsdcur_map50int_adj_res, file="ptsdcur_map50int_adj_res_summary.csv", ndigit=3, writecsv=T)

#Step 2d - Current measure of PTSD severity#

###Outcome: Diastolic BP, dbp_prs variables###

#Main effects model# 

ptsdcursx_dbp0001ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp0001ME_res)
lmOut(ptsdcursx_dbp0001ME_res, file="ptsdcursx_dbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp001ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp001ME_res)
lmOut(ptsdcursx_dbp001ME_res, file="ptsdcursx_dbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp01ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp01ME_res)
lmOut(ptsdcursx_dbp01ME_res, file="ptsdcursx_dbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp05ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp05ME_res)
lmOut(ptsdcursx_dbp05ME_res, file="ptsdcursx_dbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp10ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp10ME_res)
lmOut(ptsdcursx_dbp10ME_res, file="ptsdcursx_dbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp50ME_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp50ME_res)
lmOut(ptsdcursx_dbp50ME_res, file="ptsdcursx_dbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$dbp_prs0001_c <- merged.data$dbp_prs0001 - mean(merged.data$dbp_prs0001, na.rm=T)

merged.data$ptsdcursx_dbp0001_i <-merged.data$ptsdcursx_c*merged.data$dbp_prs0001_c

ptsdcursx_dbp0001int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs0001 + ptsdcursx_dbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp0001int_res)
lmOut(ptsdcursx_dbp0001int_res, file="ptsdcursx_dbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$dbp_prs001_c <- merged.data$dbp_prs001 - mean(merged.data$dbp_prs001, na.rm=T)

merged.data$ptsdcursx_dbp001_i <-merged.data$ptsdcursx_c*merged.data$dbp_prs001_c

ptsdcursx_dbp001int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs001 + ptsdcursx_dbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp001int_res)
lmOut(ptsdcursx_dbp001int_res, file="ptsdcursx_dbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$dbp_prs01_c <- merged.data$dbp_prs01 - mean(merged.data$dbp_prs01, na.rm=T)

merged.data$ptsdcursx_dbp01_i <-merged.data$ptsdcursx_c*merged.data$dbp_prs01_c

ptsdcursx_dbp01int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs01 + ptsdcursx_dbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp01int_res)
lmOut(ptsdcursx_dbp01int_res, file="ptsdcursx_dbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$dbp_prs05_c <- merged.data$dbp_prs05 - mean(merged.data$dbp_prs05, na.rm=T)

merged.data$ptsdcursx_dbp05_i <-merged.data$ptsdcursx_c*merged.data$dbp_prs05_c

ptsdcursx_dbp05int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs05 + ptsdcursx_dbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp05int_res)
lmOut(ptsdcursx_dbp05int_res, file="ptsdcursx_dbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$dbp_prs10_c <- merged.data$dbp_prs10 - mean(merged.data$dbp_prs10, na.rm=T)

merged.data$ptsdcursx_dbp10_i <-merged.data$ptsdcursx_c*merged.data$dbp_prs10_c

ptsdcursx_dbp10int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs10 + ptsdcursx_dbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp10int_res)
lmOut(ptsdcursx_dbp10int_res, file="ptsdcursx_dbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$dbp_prs50_c <- merged.data$dbp_prs50 - mean(merged.data$dbp_prs50, na.rm=T)

merged.data$ptsdcursx_dbp50_i <-merged.data$ptsdcursx_c*merged.data$dbp_prs50_c

ptsdcursx_dbp50int_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs50 + ptsdcursx_dbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_dbp50int_res)
lmOut(ptsdcursx_dbp50int_res, file="ptsdcursx_dbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcursx_dbp0001int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs0001 + ptsdcursx_dbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_dbp0001int_adj_res)
lmOut(ptsdcursx_dbp0001int_adj_res, file="ptsdcursx_dbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp001int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs001 + ptsdcursx_dbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_dbp001int_adj_res)
lmOut(ptsdcursx_dbp001int_adj_res, file="ptsdcursx_dbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp01int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs01 + ptsdcursx_dbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_dbp01int_adj_res)
lmOut(ptsdcursx_dbp01int_adj_res, file="ptsdcursx_dbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp05int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs05 + ptsdcursx_dbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_dbp05int_adj_res)
lmOut(ptsdcursx_dbp05int_adj_res, file="ptsdcursx_dbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp10int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs10 + ptsdcursx_dbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_dbp10int_adj_res)
lmOut(ptsdcursx_dbp10int_adj_res, file="ptsdcursx_dbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_dbp50int_adj_res <- lm(DBP_meas ~ ptsd_sx_cur + dbp_prs50 + ptsdcursx_dbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_dbp50int_adj_res)
lmOut(ptsdcursx_dbp50int_adj_res, file="ptsdcursx_dbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: Systolic BP, sbp_prs variables###

#Main effects model# 

ptsdcursx_sbp0001ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp0001ME_res)
lmOut(ptsdcursx_sbp0001ME_res, file="ptsdcursx_sbp0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp001ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp001ME_res)
lmOut(ptsdcursx_sbp001ME_res, file="ptsdcursx_sbp001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp01ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp01ME_res)
lmOut(ptsdcursx_sbp01ME_res, file="ptsdcursx_sbp01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp05ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp05ME_res)
lmOut(ptsdcursx_sbp05ME_res, file="ptsdcursx_sbp05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp10ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp10ME_res)
lmOut(ptsdcursx_sbp10ME_res, file="ptsdcursx_sbp10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp50ME_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp50ME_res)
lmOut(ptsdcursx_sbp50ME_res, file="ptsdcursx_sbp50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$sbp_prs0001_c <- merged.data$sbp_prs0001 - mean(merged.data$sbp_prs0001, na.rm=T)

merged.data$ptsdcursx_sbp0001_i <-merged.data$ptsdcursx_c*merged.data$sbp_prs0001_c

ptsdcursx_sbp0001int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs0001 + ptsdcursx_sbp0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp0001int_res)
lmOut(ptsdcursx_sbp0001int_res, file="ptsdcursx_sbp0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$sbp_prs001_c <- merged.data$sbp_prs001 - mean(merged.data$sbp_prs001, na.rm=T)

merged.data$ptsdcursx_sbp001_i <-merged.data$ptsdcursx_c*merged.data$sbp_prs001_c

ptsdcursx_sbp001int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs001 + ptsdcursx_sbp001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp001int_res)
lmOut(ptsdcursx_sbp001int_res, file="ptsdcursx_sbp001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$sbp_prs01_c <- merged.data$sbp_prs01 - mean(merged.data$sbp_prs01, na.rm=T)

merged.data$ptsdcursx_sbp01_i <-merged.data$ptsdcursx_c*merged.data$sbp_prs01_c

ptsdcursx_sbp01int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs01 + ptsdcursx_sbp01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp01int_res)
lmOut(ptsdcursx_sbp01int_res, file="ptsdcursx_sbp01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$sbp_prs05_c <- merged.data$sbp_prs05 - mean(merged.data$sbp_prs05, na.rm=T)

merged.data$ptsdcursx_sbp05_i <-merged.data$ptsdcursx_c*merged.data$sbp_prs05_c

ptsdcursx_sbp05int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs05 + ptsdcursx_sbp05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp05int_res)
lmOut(ptsdcursx_sbp05int_res, file="ptsdcursx_sbp05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$sbp_prs10_c <- merged.data$sbp_prs10 - mean(merged.data$sbp_prs10, na.rm=T)

merged.data$ptsdcursx_sbp10_i <-merged.data$ptsdcursx_c*merged.data$sbp_prs10_c

ptsdcursx_sbp10int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs10 + ptsdcursx_sbp10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp10int_res)
lmOut(ptsdcursx_sbp10int_res, file="ptsdcursx_sbp10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$sbp_prs50_c <- merged.data$sbp_prs50 - mean(merged.data$sbp_prs50, na.rm=T)

merged.data$ptsdcursx_sbp50_i <-merged.data$ptsdcursx_c*merged.data$sbp_prs50_c

ptsdcursx_sbp50int_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs50 + ptsdcursx_sbp50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_sbp50int_res)
lmOut(ptsdcursx_sbp50int_res, file="ptsdcursx_sbp50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcursx_sbp0001int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs0001 + ptsdcursx_sbp0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_sbp0001int_adj_res)
lmOut(ptsdcursx_sbp0001int_adj_res, file="ptsdcursx_sbp0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp001int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs001 + ptsdcursx_sbp001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_sbp001int_adj_res)
lmOut(ptsdcursx_sbp001int_adj_res, file="ptsdcursx_sbp001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp01int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs01 + ptsdcursx_sbp01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_sbp01int_adj_res)
lmOut(ptsdcursx_sbp01int_adj_res, file="ptsdcursx_sbp01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp05int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs05 + ptsdcursx_sbp05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_sbp05int_adj_res)
lmOut(ptsdcursx_sbp05int_adj_res, file="ptsdcursx_sbp05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp10int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs10 + ptsdcursx_sbp10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_sbp10int_adj_res)
lmOut(ptsdcursx_sbp10int_adj_res, file="ptsdcursx_sbp10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_sbp50int_adj_res <- lm(SBP_meas ~ ptsd_sx_cur + sbp_prs50 + ptsdcursx_sbp50_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_sbp50int_adj_res)
lmOut(ptsdcursx_sbp50int_adj_res, file="ptsdcursx_sbp50int_adj_res_summary.csv", ndigit=3, writecsv=T)

###Outcome: MAP, bp_prs variables### 

#Main effects model# 

ptsdcursx_map0001ME_res <- lm(map ~ ptsd_sx_cur + bp_prs0001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map0001ME_res)
lmOut(ptsdcursx_map0001ME_res, file="ptsdcursx_map0001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map001ME_res <- lm(map ~ ptsd_sx_cur + bp_prs001 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map001ME_res)
lmOut(ptsdcursx_map001ME_res, file="ptsdcursx_map001ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map01ME_res <- lm(map ~ ptsd_sx_cur + bp_prs01 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map01ME_res)
lmOut(ptsdcursx_map01ME_res, file="ptsdcursx_map01ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map05ME_res <- lm(map ~ ptsd_sx_cur + bp_prs05 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map05ME_res)
lmOut(ptsdcursx_map05ME_res, file="ptsdcursx_map05ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map10ME_res <- lm(map ~ ptsd_sx_cur + bp_prs10 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map10ME_res)
lmOut(ptsdcursx_map10ME_res, file="ptsdcursx_map10ME_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map50ME_res <- lm(map ~ ptsd_sx_cur + bp_prs50 + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map50ME_res)
lmOut(ptsdcursx_map50ME_res, file="ptsdcursx_map50ME_res_summary.csv", ndigit=3, writecsv=T)

#Interaction term and model#
#Center variables#

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$bp_prs0001_c <- merged.data$bp_prs0001 - mean(merged.data$bp_prs0001, na.rm=T)

merged.data$ptsdcursx_map0001_i <-merged.data$ptsdcursx_c*merged.data$bp_prs0001_c

ptsdcursx_map0001int_res <- lm(map ~ ptsd_sx_cur + bp_prs0001 + ptsdcursx_map0001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map0001int_res)
lmOut(ptsdcursx_map0001int_res, file="ptsdcursx_map0001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$bp_prs001_c <- merged.data$bp_prs001 - mean(merged.data$bp_prs001, na.rm=T)

merged.data$ptsdcursx_map001_i <-merged.data$ptsdcursx_c*merged.data$bp_prs001_c

ptsdcursx_map001int_res <- lm(map ~ ptsd_sx_cur + bp_prs001 + ptsdcursx_map001_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map001int_res)
lmOut(ptsdcursx_map001int_res, file="ptsdcursx_map001int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$bp_prs01_c <- merged.data$bp_prs01 - mean(merged.data$bp_prs01, na.rm=T)

merged.data$ptsdcursx_map01_i <-merged.data$ptsdcursx_c*merged.data$bp_prs01_c

ptsdcursx_map01int_res <- lm(map ~ ptsd_sx_cur + bp_prs01 + ptsdcursx_map01_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map01int_res)
lmOut(ptsdcursx_map01int_res, file="ptsdcursx_map01int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$bp_prs05_c <- merged.data$bp_prs05 - mean(merged.data$bp_prs05, na.rm=T)

merged.data$ptsdcursx_map05_i <-merged.data$ptsdcursx_c*merged.data$bp_prs05_c

ptsdcursx_map05int_res <- lm(map ~ ptsd_sx_cur + bp_prs05 + ptsdcursx_map05_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map05int_res)
lmOut(ptsdcursx_map05int_res, file="ptsdcursx_map05int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$bp_prs10_c <- merged.data$bp_prs10 - mean(merged.data$bp_prs10, na.rm=T)

merged.data$ptsdcursx_map10_i <-merged.data$ptsdcursx_c*merged.data$bp_prs10_c

ptsdcursx_map10int_res <- lm(map ~ ptsd_sx_cur + bp_prs10 + ptsdcursx_map10_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map10int_res)
lmOut(ptsdcursx_map10int_res, file="ptsdcursx_map10int_res_summary.csv", ndigit=3, writecsv=T)

merged.data$ptsdcursx_c <- merged.data$ptsd_sx_cur - mean(merged.data$ptsd_sx_cur, na.rm=T)
merged.data$bp_prs50_c <- merged.data$bp_prs50 - mean(merged.data$bp_prs50, na.rm=T)

merged.data$ptsdcursx_map50_i <-merged.data$ptsdcursx_c*merged.data$bp_prs50_c

ptsdcursx_map50int_res <- lm(map ~ ptsd_sx_cur + bp_prs50 + ptsdcursx_map50_i + sex + age + P1 + P2 + P3, data=merged.data)
summary(ptsdcursx_map50int_res)
lmOut(ptsdcursx_map50int_res, file="ptsdcursx_map50int_res_summary.csv", ndigit=3, writecsv=T)

#Interaction model with additional covariates#

ptsdcursx_map0001int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs0001 + ptsdcursx_map0001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_map0001int_adj_res)
lmOut(ptsdcursx_map0001int_adj_res, file="ptsdcursx_map0001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map001int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs001 + ptsdcursx_map001_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_map001int_adj_res)
lmOut(ptsdcursx_map001int_adj_res, file="ptsdcursx_map001int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map01int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs01 + ptsdcursx_map01_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_map01int_adj_res)
lmOut(ptsdcursx_map01int_adj_res, file="ptsdcursx_map01int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map05int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs05 + ptsdcursx_map05_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_map05int_adj_res)
lmOut(ptsdcursx_map05int_adj_res, file="ptsdcursx_map05int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map10int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs10 + ptsdcursx_map10_i + sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt, data=merged.data)
summary(ptsdcursx_map10int_adj_res)
lmOut(ptsdcursx_map10int_adj_res, file="ptsdcursx_map10int_adj_res_summary.csv", ndigit=3, writecsv=T)

ptsdcursx_map50int_adj_res <- lm(map ~ ptsd_sx_cur + bp_prs50 + ptsdcursx_map50_i + sex + age + P1 + P2 + P3 + as.factor(as.factor(educ)) + antihtn_use + trauma_count_lt, data=merged.data)
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


