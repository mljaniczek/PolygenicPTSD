###########  PTSD Physical Health Group - PTSD and BP PRS Project: Step 1 - #########################
# 1a Merging Files,                                                     #############################
# 1b Creating Categorical Variables for BP and ancestry,                #############################
# 1c Testing for significant difference in Old vs New BP guidelines stratified by ancestry ##########
# 1d Make summary table and save output
# Author: Margaret Hannum
# Last updated: 2/11/2018
#####################################################################################################
#Have files saved to a folder on your Desktop called "PGC_PTSD_BP" and set working directory to that folder#
#Be sure to change USERNAME so it reflects what username#
#Select Windows or Mac path by deleting the "#" in front of the setwd command#

#Windows path#
#setwd("C:/Users/USERNAME/Desktop/PGC_PTSD_BP")

#Mac path#
#setwd("Desktop/PGC_PTSD_BP")

###############################
#Step 1a: Load and merge data #
###############################

#Read in file with IID, FID, BP PRS, ancestry PCs, and ancestry from Caroline and Adam. Change STUDYPI in file names below to your study PI last name#

##*What will the column names for the PRS look like? In your guide they are like "dbp_prs001" but in the sample file Adam sent they are "XO.01"
##Confirm with adam colnames for everything

bpprs_pcs <- read.csv("STUDYPI_prs_pcs.csv")

#Read in your study file with trauma, PTSD, BP, and demographic data. Change STUDYPI in file names below to your study PI last name. Ensure that id=Participant ID#

pheno <- read.csv("STUDYPI_PTSD_BP_demo.csv")

#Merge data files by participant ID#

merged <- merge(bpprs_pcs, pheno, by="FID","IID")


#Do we want to add this ourselves or just ensure in the instructions that the groups will add missing variables with NA values?
 

###############################################################################################
#Step 1b: Hypertension classification stratified by ancestry (old and new guidelines) #
##############################################################################################
#OLD Hypertensive status based on continuous measured BP variables or endorsing antihypertensive medication use - old AHA cutoffs#
#0=normotensive, 1=pre-hypertensive, 2=hypertensive#

#NEW Hypertensive status based on continuous measured BP variables or endorsing antihypertensive medication use - new AHA cutoffs#
#0=normotensive, 1=pre-hypertensive/elevated, 2=hypertensive#
library(dplyr)
merged = mutate(merged, 
                htn_aha_old =
                  ifelse((SBP_meas<140 | DBP_meas<90) & antihtn_use !=1, 1,  
                          ifelse(SBP_meas<120 & DBP_meas<80, 0,
                                 ifelse(SBP_meas>=140 | DBP_meas>=90 | antihtn_use== 1, 2, NA))),
                htn_aha_new = 
                  ifelse(SBP_meas<130 & DBP_meas<80 & antihtn_use !=1, 1, 
                         ifelse(SBP_meas<120 & DBP_meas<80, 0,
                                ifelse(SBP_meas>=130 | DBP_meas>=80 | antihtn_use==1, 2, NA))))

#Visually check to make sure the two new variables htn_aha_old and htn_aha_new were added appropriately

#Also create binary variable in case there are not enough in the pre-hypertensive group
###Below, 0 = non-hypertensive or pre-hypertensive, 1 = hypertensive
merged = mutate(merged, 
                htn_aha_old_bi =
                  ifelse((SBP_meas<140 | DBP_meas<90) & antihtn_use != 1, 0,
                         ifelse(SBP_meas>=140 | DBP_meas>=90 | antihtn_use==1, 1, NA)),
                htn_aha_new_bi = 
                  ifelse(SBP_meas<130 & DBP_meas<80 & antihtn_use != 1, 0,
                         ifelse((SBP_meas>=130 | DBP_meas>=80) | antihtn_use==1, 1, NA)))

###
#Add BP variables adjusted for if antihtn_use = 1
# SBP + 10 and DBP + 5 if antihtn_use = 1

merged = mutate(merged, 
                SBP_meas_adj =
                  ifelse(antihtn_use == 1, SBP_meas + 10, NA),
                DBP_meas_adj = 
                  ifelse(antihtn_use ==1, DBP_meas + 5, NA))




#Now categorize ancestries
#Ancestry variable is called 'bestpop'. 
#European ancestry subjects are labeled as 'eur', Africans are 'afr', African Americans (admixed between European and African) are 'aam'. 
#Combine the afr and aam samples together for analyses
#Code 0 = eur, 1 = afr & aam

merged = mutate(merged, 
                ancestry =
                  ifelse(bestpop == "eur", 0,
                         ifelse(bestpop == "afr" | bestpop == "aam", 1, NA)))

#create indices for subset
index_eur = grepl(0, merged$ancestry)
index_afr = grepl(1, merged$ancestry)


#########################################################################################
#Step 1c: Create contingency tables stratified on ancestry, and test if assignments are #
#significantly different between old and new classification #############################
#########################################################################################
stratSumEur <- count(merged[index_eur,], c('htn_aha_new', 'htn_aha_old')) #Used index to make contingency table of merged data
eurChi <- chisq.test(stratSumEur) #Chi-square test on stratified data, comparing new to old classes of hypertension

stratSumAfr<- count(merged[index_afr,], c('htn_aha_new', 'htn_aha_old'))
afrChi <- chisq.test(stratSumAfr)

#Now tests below after collapsing normotensive and pre-hypertensive (in case there were not enough samples in pre-hypertensive)
stratSumEurBi<- count(merged[index_eur,], c('htn_aha_new_bi', 'htn_aha_old_bi')) #Used index to make contingency table of merged data
eurChiBi <- chisq.test(stratSumEurBi) #Chi-square test on stratified data, comparing new to old classes of hypertension

stratSumAfrBi<- count(merged[index_afr,], c('htn_aha_new_bi', 'htn_aha_old_bi'))
afrChiBi <- chisq.test(stratSumAfrBi)

#################################
#Step 1d                        #
#Make summary table; save output#
#################################
chiSummary <- data.frame(ChiSq = c(eurChi$statistic, afrChi$statistic, eurChiBi$statistic, afrChiBi$statistic), 
                                DF = c(eurChi$parameter, afrChi$parameter, eurChiBi$parameter, afrChiBi$parameter),
                                PValue = c(eurChi$p.value, afrChi$p.value, eurChiBi$p.value, afrChiBi$p.value),
                         row.names = c("European 3-class", "African 3-class", "European 2-class", "African 2-class"))

#Save summary table of labeled chi-square test p-values
#Replace STUDYPI with your PI's name
#Check working directory you are in by get()
#Change working directory if necessary, if you would like to save files elsewhere
#setwd("<INSERTLOCATION>")
write.csv(chiSummary, "STUDYPI_bpClassAncestryStratChisqSum.csv")
write.csv(stratSumEur, "STUDYPI_EurBPClassCount")
write.csv(stratSumAfr, "STUDYPI_AfrBPClassCount")
write.csv(stratSumEurBi, "STUDYPI_EurBPClassBinaryCount")
write.csv(stratSumAfrBi, "STUDYPI_AfrBPClassBinaryCount")





