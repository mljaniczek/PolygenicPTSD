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

#Read in file with FID, IID, SBP_meas, and DBP_meas, and antihtn_use from your site. 
#Change STUDYPI in file names below to your study PI last name#
bpsite <- read.csv("STUDYPI_bp.csv")

#Read in file with FID, IID, and Ancestry information (bestpop) provided from Adam#
#Change STUDYPI in file names below to your study PI last name#
pop <- read.csv("STUDYPI_population.csv")

#Merge data files by participant IID and family ID FID
bp_pop <- merge(bpsite, pop, by= c("IID", "FID"))

###############################################################################################
#Step 1b: Hypertension classification stratified by ancestry (old and new guidelines) #
##############################################################################################
#OLD Hypertensive status based on continuous measured BP variables or endorsing antihypertensive medication use - old AHA cutoffs#
#0=normotensive, 1=pre-hypertensive, 2=hypertensive#

#NEW Hypertensive status based on continuous measured BP variables or endorsing antihypertensive medication use - new AHA cutoffs#
#0=normotensive, 1=pre-hypertensive/elevated, 2=hypertensive#
library(dplyr)
bp_pop = mutate(bp_pop, 
                htn_aha_old =
                  ifelse(SBP_meas<120 & DBP_meas<80, 0,
                          ifelse((SBP_meas>=120 & SBP_meas<=139) | (DBP_meas>=80 & DBP_meas<=89), 1,
                                 ifelse(SBP_meas>=140 | DBP_meas>=90 | antihtn_use=1, 2, NA))),
                htn_aha_new = 
                  ifelse(SBP_meas<120 & DBP_meas<80, 0,
                         ifelse((SBP_meas>=120 & SBP_meas<=129 & DBP_meas<80), 1,
                                ifelse(SBP_meas>=130 | DBP_meas>=80 | antihtn_use=1, 2, NA))))
##Margie -- is there an extra "(" in the third line/second ifelse of the htn_aha_new or does it need another ")" ?


#Visually check to make sure the two new variables htn_aha_old and htn_aha_new were added appropriately

#Also create binary variable in case there are not enough in the pre-hypertensive group
###Below, 0 = non-hypertensive or pre-hypertensive, 1 = hypertensive
bp_pop = mutate(bp_pop, 
                htn_aha_old_bi =
                  ifelse(SBP_meas<=139 | DBP_meas<=89, 0,
                         ifelse(SBP_meas>=140 | DBP_meas>=90 | antihtn_use=1, 1, NA)),
                htn_aha_new_bi = 
                  ifelse(SBP_meas<=129 & DBP_meas<80, 0,
                         ifelse(SBP_meas>=130 | DBP_meas>=80 | antihtn_use=1, 1, NA)))

#Margie - I grouped pre-hypertensives with normotensives here. Can you check to make sure that the code looks right? Also I made it such that hypertensive is equal to 1, not 2

                            
#Now categorize ancestries
#Ancestry variable is called 'bestpop'. 
#European ancestry subjects are labeled as 'eur', Africans are 'afr', African Americans (admixed between European and African) are 'aam'. 
#Combine the afr and aam samples together for analyses
#Code 0 = eur, 1 = afr & aam

bp_pop = mutate(bp_pop, 
                ancestry =
                  ifelse(bestpop == "eur", 0,
                         ifelse(bestpop == "afr" | bestpop == "aam", 1, NA)))

#create indices for subset
index_eur = grepl(0, bp_pop$ancestry)
index_afr = grepl(1, bp_pop$ancestry)


#########################################################################################
#Step 1c: Create contingency tables stratified on ancestry, and test if assignments are #
#significantly different between old and new classification #############################
#########################################################################################
stratSumEur <- count(bp_pop[index_eur,], c('htn_aha_new', 'htn_aha_old')) #Used index to make contingency table of bp_pop data
eurChi <- chisq.test(stratSumEur) #Chi-square test on stratified data, comparing new to old classes of hypertension

stratSumAfr<- count(bp_pop[index_afr,], c('htn_aha_new', 'htn_aha_old'))
afrChi <- chisq.test(stratSumAfr)

#Now tests below after collapsing normotensive and pre-hypertensive (in case there were not enough samples in pre-hypertensive)
stratSumEurBi<- count(bp_pop[index_eur,], c('htn_aha_new_bi', 'htn_aha_old_bi')) #Used index to make contingency table of bp_pop data
eurChiBi <- chisq.test(stratSumEurBi) #Chi-square test on stratified data, comparing new to old classes of hypertension

stratSumAfrBi<- count(bp_pop[index_afr,], c('htn_aha_new_bi', 'htn_aha_old_bi'))
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
write.csv(chiSummary, "STUDYPI_bpClassAncestryStratChisqSum.csv")
write.csv(stratSumEur, "STUDYPI_EurBPClassCount")
write.csv(stratSumAfr, "STUDYPI_AfrBPClassCount")
write.csv(stratSumEurBi, "STUDYPI_EurBPClassBinaryCount")
write.csv(stratSumAfrBi, "STUDYPI_AfrBPClassBinaryCount")





