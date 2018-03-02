
library(dplyr)
library(vglm)
#todo: multinom
#Todo: add in ancestry data
#Todo: deal with related subjects!
#todo: add in mean and SD and N for all variables analyzed
#todo: SBP and DBP PRS need to be loaded
#todo: for 3 level outcome, possibly report the multiplicative and additive model resutls
#todo: more rigorous specification for phenotype file, error checking all alround
#todo: try code in case models DONT fit

#Notes:
#Right now the script is set up to run one BP trait. In this example, it's SBP.
#Minor modification could allow it to run DBP, namely one needs to select a different PRS input file


#Write in study name here
studyname="mrs"

#Write in trait under investigation
trait='sbp' 

#File name for PRS data
prsfile= "/Users/TinyDragon/github/PolygenicPTSD/data/test.SBP.all.score"
prs <- read.table(prsfile, header=T,stringsAsFactors=F)

#We'll note the names of all PRS columns that aren't FID and IID (cols 1 and 2), we're gonna loop over those
##*JEN: will we have this kind of file with just PRS and IDs?
prs_columns <- names(prs)[-c(1:2)]

phenofile <- "/Users/TinyDragon/github/PolygenicPTSD/data/example_bpdata.txt" #File name for phenotype data
pheno <- read.table(phenofile, header=T,stringsAsFactors=F)

#PRS need to be joined to the phenotype data. 

#Merge PRS and phenotypes. Take only the overlapping set of samples (i.e. inner join)
#Note: data is meant to be joined on FID and IID keys, since that matches genotypes
dat <- merge(pheno,prs,by=c("FID","IID"))


#To go through all PRS, we use lapply

#Since I havent loaded all covars into my phenotype file, as a test,simulated dummy values for missing covariates
dat$ptsd_diag_lt <-sample(c(0,1), replace=TRUE, size=dim(dat)[1])
dat$ptsd_sx_lt <- sample(c(0:10), replace=TRUE, size=dim(dat)[1])
dat$ptsd_diag_cur <- sample(c(0,1), replace=TRUE, size=dim(dat)[1])
dat$ptsd_sx_cur <- sample(c(0:10), replace=TRUE, size=dim(dat)[1])
dat$P1 <- rnorm(dim(dat)[1])
dat$P2 <- rnorm(dim(dat)[1])
dat$P3 <- rnorm(dim(dat)[1])
dat$BP <- rnorm(dim(dat)[1])
dat$sex <- round(runif(dim(dat)[1],0,1))
dat$age <- runif(dim(dat)[1],18,50)
#simulated SBP in case you want to play around: dat$sbp_meas <- scale(dat$ptsd_diag_lt)*.1 + scale(dat$X0.650000)*.1  + scale(dat$ptsd_diag_lt)*scale(dat$X0.650000)*.1  + scale(dat$sex)/30 + scale(dat$age)/30  + scale(dat$P1)/50 + scale(dat$P2)/50+ scale(dat$ptsd_diag_lt)*scale(dat$P1)/50 + scale(dat$X0.650000)*scale(dat$sex)/50 + rnorm(dim(dat)[1],sd=2.3)
#summary(lm(sbp_meas ~ ptsd_diag_lt * X0.650000 + sex + age + P1 + P2 + ptsd_diag_lt *P1  + X0.650000*P1 +  X0.650000*sex,data=dat))

summary(lm(sbp_meas ~ X1,data=dat))
###Function to update formulas and run models
#Rather than copy and paste permutations of a model many times, I'm using a function where the model statement can be updated via a function
#This function can then be run in a loop, where only select variables change (e.g. PTSD variable, baseline model), taking up only a few lines of text.

#Function input description
#prs: name of PRS column in data, e.g. X1.00000
#formula_statement: baseline formula, e.g. formula(bp_meas ~ sex + age + P1 + P2 + P3)
#outcome: outcome variable name, e.g. sbp_meas
#ptsd: ptsd variable name
#dataset: input dataset


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

#I'll specify the 'baseline' models here (i.e. No PRS or PTSD covariates included)
base_model <- formula(bp_meas ~ sex + age + P1 + P2 + P3)
covar_model <- formula(bp_meas ~ sex + age + P1 + P2 + P3 + as.factor(educ) + antihtn_use + trauma_count_lt) 


#Now loop over the baseline models, PTSD variables, and whatever else
#The lapply function is a loop used to produce models for all levels of PRS
i=1
for (modelloop in c(base_model,covar_model)) #Loop over baseline models 
{
 for (ptsdloop in c("ptsd_diag_lt", "ptsd_dx_lt","ptsd_diag_cur", "ptsd_dx_cur")) #Loop over all ptsd dx variables
 {
  #Save model outputs into a variable called model
  model <- lapply(prs_columns, updater_analyzer, formula_statement = modelloop, outcome = "sbp_meas", ptsd=ptsdloop, dataset=dat)
  
  save(model,file=paste(study,trait,modelloop,ptsdloop,i,".RData",sep="_")) #Save model outputs as an R object
 }
 i=i+1
}

#Elements can be acessed e.g.
#This gives a summary of the Nth PRS
N=1
t1[[N]]  #e.g. for the 1st PRS, give summary info for all models
#This gives a summary, for the Nth PRS, of either model 1, 2,3,4
mod=3
t1[[N]][[mod]] #e.g. for the 1st PRS, give summary info for the 3rd model
t1[[N]][[4]] #e.g. for the 1st PRS, give summary info for the 3rd model
