
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






