#################
# ACES_Baseline.R
# Author: Katherine SF Damme
# Code Updated by: Robert Toms
#################

# The purpose of this code is to aggregate baseline ACEs data for the ABCD dataset, according to the ACEs items/categories established by the CDC.
# Categories from CDC Guidance: https://www.cdc.gov/violenceprevention/aces/about.html
# CDC Guidance based on previous research: Felitti, V. J., Anda, R. F., Nordenberg, D., Williamson, D. F., Spitz, A. M., Edwards, V., Koss, M. P., & Marks, J. S. (1998). Relationship of childhood abuse and household dysfunction to many of the leading causes of death in adults. The Adverse Childhood Experiences (ACE) Study. American journal of preventive medicine, 14(4), 245–258. https://doi.org/10.1016/s0749-3797(98)00017-8

# 3 ACEs Categories: https://www.cdc.gov/violenceprevention/aces/about.html
# Abuse 
#  - Emotional
#  - Physical
#  - Sexual
# Household Challenges 
#  - Mother treated violently
#  - Substance Abuse
#  - Mental Illness
#  - Parental Separation or Divorce
#  - Incarcerated Household Member
# Neglect
#  - Emotional
#  - Physical


# import libraries
library(tidyverse)
library(readr)
library (dplyr)
datapath <- '/path/to/ABCD_5.0/core/'
derivative_data <- '/path/to/derivative_data/5.0/'


####################
# IN THE BELOW CODE BLOCK
# Abuse (PTSD)
####################

# import ptsd data, keep useful columns
abcd_p_ksad_ptsd <- read_csv(paste0(datapath, "/mental-health/mh_p_ksads_ptsd.csv"))
abcd_p_ksad_ptsd_ACES<-abcd_p_ksad_ptsd%>%
  select(src_subject_id,eventname,
         ksads_ptsd_raw_762_p,  # Shot, stabbed, or beaten brutally by a grown up in the home
         ksads_ptsd_raw_763_p,  # Beaten to the point of having bruises by a grown up in the home
         ksads_ptsd_raw_767_p,  # A grown up in the home touched your child in their privates, had your child touch their privates, or did other sexual things to your child
         ksads_ptsd_raw_768_p,  # An adult outside your family touched your child in their privates, had your child touch their privates or did other sexual things to your child
         ksads_ptsd_raw_766_p,  # Witness the grownups in the home push, shove or hit one another
         ksads_ptsd_raw_765_p,  # A family member threatened to kill your child
         ksads_ptsd_raw_764_p ) # A non-family member threatened to kill your child

#Emotional Abuse - 2 yers  baseline and year 2 full data other years partial 
# ksads_ptsd_raw_764_p -  A non-family member threatened to kill your child
# ksads_ptsd_raw_765_p -  A family member threatened to kill your child
abcd_p_ksad_ptsd_ACES$EmotionalAbuseTotal<-abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_764_p+abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_765_p
abcd_p_ksad_ptsd_ACES$EmotionalAbuse1Y0N[abcd_p_ksad_ptsd_ACES$EmotionalAbuseTotal==0]<-0
abcd_p_ksad_ptsd_ACES$EmotionalAbuse1Y0N[abcd_p_ksad_ptsd_ACES$EmotionalAbuseTotal>=1]<-1
table(abcd_p_ksad_ptsd_ACES$EmotionalAbuseTotal, abcd_p_ksad_ptsd_ACES$EmotionalAbuse1Y0N )
table(abcd_p_ksad_ptsd_ACES$EmotionalAbuseTotal, abcd_p_ksad_ptsd_ACES$eventname )


#Physical Abuse  - 2 years  baseline and year 2 full data other years partial 
# ksads_ptsd_raw_762_p - Shot, stabbed, or beaten brutally by a grown up in the home
# ksads_ptsd_raw_763_p - Beaten to the point of having bruises by a grown up in the home
abcd_p_ksad_ptsd_ACES$PhysicalAbuseTotal<-abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_762_p+abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_763_p
abcd_p_ksad_ptsd_ACES$PhysicalAbuse1Y0N[abcd_p_ksad_ptsd_ACES$PhysicalAbuseTotal==0]<-0
abcd_p_ksad_ptsd_ACES$PhysicalAbuse1Y0N[abcd_p_ksad_ptsd_ACES$PhysicalAbuseTotal>=1]<-1
table(abcd_p_ksad_ptsd_ACES$PhysicalAbuseTotal, abcd_p_ksad_ptsd_ACES$PhysicalAbuse1Y0N )
table(abcd_p_ksad_ptsd_ACES$PhysicalAbuseTotal, abcd_p_ksad_ptsd_ACES$eventname )


#Sexual Abuse - 2 yers  baseline and year 2 full data other years partial 
# ksads_ptsd_raw_767_p - A grown up in the home touched your child in their privates, had your child touch their privates, or did other sexual things to your child
# ksads_ptsd_raw_768_p - An adult outside your family touched your child in their privates, had your child touch their privates or did other sexual things to your child
abcd_p_ksad_ptsd_ACES$SexualAbuseTotal<-abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_767_p+abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_768_p
abcd_p_ksad_ptsd_ACES$SexualAbuse1Y0N[abcd_p_ksad_ptsd_ACES$SexualAbuseTotal==0]<-0
abcd_p_ksad_ptsd_ACES$SexualAbuse1Y0N[abcd_p_ksad_ptsd_ACES$SexualAbuseTotal>=1]<-1
table(abcd_p_ksad_ptsd_ACES$SexualAbuseTotal, abcd_p_ksad_ptsd_ACES$SexualAbuse1Y0N )
table(abcd_p_ksad_ptsd_ACES$SexualAbuseTotal, abcd_p_ksad_ptsd_ACES$eventname )


####################
# IN THE BELOW CODE BLOCK
# Household Challenges
# Household Violence, Substance Use, Parental Suicide, Parental Depression, Criminal System Involvement
####################

# Household violence 1/3 - three years of data
# ksads_ptsd_raw_766_p - Witness the grownups in the home push, shove or hit one another
abcd_p_ksad_ptsd_ACES$ksads_ptsd_raw_766_p->abcd_p_ksad_ptsd_ACES$WitnessViolenceHousehold
table(abcd_p_ksad_ptsd_ACES$WitnessViolenceHousehold)
table(abcd_p_ksad_ptsd_ACES$WitnessViolenceHousehold, abcd_p_ksad_ptsd_ACES$eventname)

# Done with ptsd spreadsheet, save key columns to merge later
abcd_ACES<-abcd_p_ksad_ptsd_ACES%>%select("src_subject_id","eventname","WitnessViolenceHousehold","SexualAbuse1Y0N", "EmotionalAbuse1Y0N","PhysicalAbuse1Y0N")


#household violence 2/3 - number of time points 5
abcd_p_familyenvironment <- read_csv(paste0(datapath, "/culture-environment/ce_p_fes.csv"))
abcd_p_familyenvironmentACES <- abcd_p_familyenvironment%>%
  select(src_subject_id,eventname, 
         fam_enviro6_p, # Family members sometimes hit each other. [Parent] 1 = True, 0 = False, 999 = Don't know, 777 = Decline to answer
         fam_enviro3_p) # Family members sometimes get so angry they throw things. [Parent] 1 = True, 0 = False, 999 = Don't know, 777 = Decline to answer

abcd_p_familyenvironmentACES$FamilyEnviron_Parent_ViolenceTotal<-abcd_p_familyenvironmentACES$fam_enviro6_p+abcd_p_familyenvironmentACES$fam_enviro3_p
abcd_p_familyenvironmentACES$FamilyEnviron_Parent_Violence1Y0N[abcd_p_familyenvironmentACES$FamilyEnviron_Parent_ViolenceTotal=="2"]<-"1"
abcd_p_familyenvironmentACES$FamilyEnviron_Parent_Violence1Y0N[abcd_p_familyenvironmentACES$FamilyEnviron_Parent_ViolenceTotal=="1"]<-"1"
abcd_p_familyenvironmentACES$FamilyEnviron_Parent_Violence1Y0N[abcd_p_familyenvironmentACES$FamilyEnviron_Parent_ViolenceTotal=="0"]<-"0"
table(abcd_p_familyenvironmentACES$FamilyEnviron_Parent_Violence1Y0N,abcd_p_familyenvironmentACES$FamilyEnviron_Parent_ViolenceTotal )
table(abcd_p_familyenvironmentACES$FamilyEnviron_Parent_Violence1Y0N,abcd_p_familyenvironmentACES$eventname )

# Done with parent fes spreadsheet, merge with existing key columns
abcd_ACES_2<-merge(abcd_ACES,abcd_p_familyenvironmentACES[,c("src_subject_id","eventname","FamilyEnviron_Parent_Violence1Y0N")] , by =c("src_subject_id", "eventname"), all=TRUE)


#household violence 3/3 - number of timepoints 5 
abcd_y_familyenvironment <- read_csv(paste0(datapath, "/culture-environment/ce_y_fes.csv"))
abcd_y_familyenvironmentACES <- abcd_y_familyenvironment%>%
  select(src_subject_id,eventname, 
         fes_youth_q6, # Family members sometimes hit each other. [Youth] 1 = True, 0 = False, 777 = Decline to answer
         fes_youth_q3) # Family members sometimes get so angry they throw things. [Youth] 1 = True, 0 = False, 777 = Decline to answer

abcd_y_familyenvironmentACES$FamilyEnviron_Youth_ViolenceTotal<-abcd_y_familyenvironmentACES$fes_youth_q6+abcd_y_familyenvironmentACES$fes_youth_q3
abcd_y_familyenvironmentACES$FamilyEnviron_Youth_Violence1Y0N[abcd_y_familyenvironmentACES$FamilyEnviron_Youth_ViolenceTotal=="2"]<-"1"
abcd_y_familyenvironmentACES$FamilyEnviron_Youth_Violence1Y0N[abcd_y_familyenvironmentACES$FamilyEnviron_Youth_ViolenceTotal=="1"]<-"1"
abcd_y_familyenvironmentACES$FamilyEnviron_Youth_Violence1Y0N[abcd_y_familyenvironmentACES$FamilyEnviron_Youth_ViolenceTotal=="0"]<-"0"
table(abcd_y_familyenvironmentACES$FamilyEnviron_Youth_Violence1Y0N,abcd_y_familyenvironmentACES$FamilyEnviron_Youth_ViolenceTotal)
table(abcd_y_familyenvironmentACES$FamilyEnviron_Youth_Violence1Y0N,abcd_y_familyenvironmentACES$eventname)

# Done with youth fes spreadsheet, merge with existing key columns
abcd_ACES_3<-merge(abcd_ACES_2,abcd_y_familyenvironmentACES[,c("src_subject_id","eventname","FamilyEnviron_Youth_Violence1Y0N")] , by =c("src_subject_id", "eventname"), all=TRUE)

##### Substance Use, Suicide/Depression, Criminal System Involvement/Incarceration
abcd_p_familyhistory <- read_csv(paste0(datapath, "mental-health/mh_p_fhx.csv"))
abcd_p_familyhistory_ACES<-abcd_p_familyhistory%>%
  select(src_subject_id,eventname,
         famhx_4_p,                  # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Has any blood relative of your child ever had any problems due to alcohol, such as: Marital separation or divorce; laid off or fired from work; arrests or DUIs; alcohol harmed their health; In an alcohol treatment program; suspended or expelled from school 2 or more times; isolated self from family, caused arguments or were drunk a lot.
         famhx_4d_p___1,             # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological mother [Multi-select]: Marital
         famhx_4d_p___2,             # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological mother [Multi-select]: Work
         famhx_4d_p___3,             # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological mother [Multi-select]: Arrests/DUI
         famhx_4d_p___4,             # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological mother [Multi-select]: Alcohol treatment program
         famhx_4d_p___5,             # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological mother [Multi-select]: School
         famhx_4d_p___6,             # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological mother [Multi-select]: Isolated self, arguments, drunk a lot
         famhx4a_p___1,              # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological father [Multi-select]: Marital
         famhx4a_p___2,              # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological father [Multi-select]: Work
         famhx4a_p___3,              # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological father [Multi-select]: Arrests/DUI
         famhx4a_p___4,              # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological father [Multi-select]: Alcohol treatment program
         famhx4a_p___5,              # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological father [Multi-select]: School
         famhx4a_p___6,              # TRUE = 1, FALSE = 0; Problems due to alcohol - Biological father [Multi-select]: Isolated self, arguments, drunk a lot
         fam_history_13_yes_no,      # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Has any blood relative of your child ever attempted or committed suicide?
         fam_history_q13a_suicide,   # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Attempted or committed suicide - Biological father
         fam_history_q13d_suicide,   # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Attempted or committed suicide - Biological mother
         fam_history_6_yes_no,       # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Has any blood relative of your child ever suffered from depression, that is, have they felt so low for a period of at least two weeks that they hardly ate or slept or couldn't work or do whatever they usually do?
         fam_history_q6a_depression, # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Depression - Biological father
         fam_history_q6d_depression, # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Depression - Biological mother
         fam_history_9_yes_no,       # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Has any blood relative of your child been the kind of person who never holds a job for long, or gets into fights, or gets into trouble with the police from time to time, or had any trouble with the law as a child or an adult?
         fam_history_q9a_trouble,    # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Difficulty holding a job or getting in trouble - Biological father
         fam_history_q9d_trouble)    # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Difficulty holding a job or getting in trouble - Biological mother

#colnames(abcd_p_familyhistory_ACES) <- c(src_subject_id,eventname,famhx_alcolholproblems_p,famhx_biomother_alcp_maritialpx,famhx_biomother_alcp_work,famhx_biomother_alcp_arrestDUI,famhx_biomother_alcp_health,famhx_biomother_alcp_school,famhx_biomother_alcp_social,amhx_biofather_alcp_maritialpx,famhx_biofather_alcp_work,famhx_biofather_alcp_arrestDUI,famhx_biofather_alcp_health,famhx_biofather_alcp_school,famhx_biofather_alcp_social, fam_history_suicide_yes_no,fam_history_biofather_suicide,fam_history_biomother_suicide, fam_history_depression_yes_no,fam_history_biofather_depression, fam_history_biomother_depression, fam_history_criminaltrouble_yes_no, fam_history_biofather_crimetrouble, fam_history_biomother_trouble)

##### Substance Use Problems - Baseline only
# famhx_4d_p___1, Problems due to alcohol - Biological mother [Multi-select]: Marital
# famhx_4d_p___2, Problems due to alcohol - Biological mother [Multi-select]: Work
# famhx_4d_p___3, Problems due to alcohol - Biological mother [Multi-select]: Arrests/DUI
# famhx_4d_p___4, Problems due to alcohol - Biological mother [Multi-select]: Alcohol treatment program
# famhx_4d_p___5, Problems due to alcohol - Biological mother [Multi-select]: School
# famhx_4d_p___6, Problems due to alcohol - Biological mother [Multi-select]: Isolated self, arguments, drunk a lot
# famhx4a_p___1, Problems due to alcohol - Biological father [Multi-select]: Marital
# famhx4a_p___2, Problems due to alcohol - Biological father [Multi-select]: Work
# famhx4a_p___3, Problems due to alcohol - Biological father [Multi-select]: Arrests/DUI
# famhx4a_p___4, Problems due to alcohol - Biological father [Multi-select]: Alcohol treatment program
# famhx4a_p___5, Problems due to alcohol - Biological father [Multi-select]: School
# famhx4a_p___6, Problems due to alcohol - Biological father [Multi-select]: Isolated self, arguments, drunk a lot
abcd_p_familyhistory_ACES$ParentalAlcoholProblemsTotal<-abcd_p_familyhistory_ACES$famhx_4d_p___1+abcd_p_familyhistory_ACES$famhx_4d_p___2+abcd_p_familyhistory_ACES$famhx_4d_p___3+abcd_p_familyhistory_ACES$famhx_4d_p___4+abcd_p_familyhistory_ACES$famhx_4d_p___5+abcd_p_familyhistory_ACES$famhx_4d_p___6+abcd_p_familyhistory_ACES$famhx4a_p___1+abcd_p_familyhistory_ACES$famhx4a_p___2+abcd_p_familyhistory_ACES$famhx4a_p___3+abcd_p_familyhistory_ACES$famhx4a_p___4+abcd_p_familyhistory_ACES$famhx4a_p___5+abcd_p_familyhistory_ACES$famhx4a_p___6
abcd_p_familyhistory_ACES$c[abcd_p_familyhistory_ACES$ParentalAlcoholProblemsTotal==0]<-0
abcd_p_familyhistory_ACES$ParentalAlcoholProblems1Y0N[abcd_p_familyhistory_ACES$ParentalAlcoholProblemsTotal>=1]<-1
#sanity check 
table(abcd_p_familyhistory_ACES$ParentalAlcoholProblemsTotal, abcd_p_familyhistory_ACES$ParentalAlcoholProblems1Y0N )
table(abcd_p_familyhistory_ACES$eventname, abcd_p_familyhistory_ACES$ParentalAlcoholProblems1Y0N )

##### Parental Suicide - Baseline only
# fam_history_q13a_suicide, Attempted or committed suicide - Biological father
# fam_history_q13d_suicide, Attempted or committed suicide - Biological mother
abcd_p_familyhistory_ACES$fam_history_q13a_suicide[abcd_p_familyhistory_ACES$fam_history_q13a_suicide==999]<-NA
abcd_p_familyhistory_ACES$fam_history_q13d_suicide[abcd_p_familyhistory_ACES$fam_history_q13d_suicide==999]<-NA
abcd_p_familyhistory_ACES$ParentalSuicideTotal<-abcd_p_familyhistory_ACES$fam_history_q13a_suicide+abcd_p_familyhistory_ACES$fam_history_q13d_suicide
abcd_p_familyhistory_ACES$ParentalSuicide1Y0N[abcd_p_familyhistory_ACES$ParentalSuicideTotal==0]<-0
abcd_p_familyhistory_ACES$ParentalSuicide1Y0N[abcd_p_familyhistory_ACES$ParentalSuicideTotal>=1]<-1
#sanity check 
table(abcd_p_familyhistory_ACES$ParentalSuicide1Y0N, abcd_p_familyhistory_ACES$ParentalSuicideTotal )
table(abcd_p_familyhistory_ACES$ParentalSuicide1Y0N, abcd_p_familyhistory_ACES$eventname )


##### Parental Depression -  Baseline only
# fam_history_q6a_depression, Depression - Biological father
# fam_history_q6d_depression, Depression - Biological mother
abcd_p_familyhistory_ACES$fam_history_q6a_depression[abcd_p_familyhistory_ACES$fam_history_q6a_depression==999]<-NA
abcd_p_familyhistory_ACES$fam_history_q6d_depression[abcd_p_familyhistory_ACES$fam_history_q6d_depression==999]<-NA
abcd_p_familyhistory_ACES$ParentalMDDTotal<-abcd_p_familyhistory_ACES$fam_history_q6a_depression+abcd_p_familyhistory_ACES$fam_history_q6d_depression
abcd_p_familyhistory_ACES$ParentalMDD1Y0N[abcd_p_familyhistory_ACES$ParentalMDDTotal==0]<-0
abcd_p_familyhistory_ACES$ParentalMDD1Y0N[abcd_p_familyhistory_ACES$ParentalMDDTotal>=1]<-1
#sanity check 
table(abcd_p_familyhistory_ACES$ParentalMDDTotal, abcd_p_familyhistory_ACES$ParentalMDD1Y0N )
table(abcd_p_familyhistory_ACES$eventname, abcd_p_familyhistory_ACES$ParentalMDD1Y0N )


##### Criminal System Involvement - Baseline only
# fam_history_q9a_trouble,    # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Difficulty holding a job or getting in trouble - Biological father
# fam_history_q9d_trouble)    # Yes = 1, No = 0, Don't Know = 999, Decline to Answer = 777; Difficulty holding a job or getting in trouble - Biological mother
abcd_p_familyhistory_ACES$fam_history_q9a_trouble[abcd_p_familyhistory_ACES$fam_history_q9a_trouble==999]<-NA
abcd_p_familyhistory_ACES$fam_history_q9d_trouble[abcd_p_familyhistory_ACES$fam_history_q9d_trouble==999]<-NA
abcd_p_familyhistory_ACES$ParentalCriminalTotal<-abcd_p_familyhistory_ACES$fam_history_q9a_trouble+abcd_p_familyhistory_ACES$fam_history_q9d_trouble
abcd_p_familyhistory_ACES$ParentalCriminal1Y0N[abcd_p_familyhistory_ACES$ParentalCriminalTotal==0]<-0
abcd_p_familyhistory_ACES$ParentalCriminal1Y0N[abcd_p_familyhistory_ACES$ParentalCriminalTotal>=1]<-1
table(abcd_p_familyhistory_ACES$ParentalCriminalTotal, abcd_p_familyhistory_ACES$ParentalCriminal1Y0N )


# Done with fhx spreadsheet, merge with existing key columns
abcd_ACES_4<-merge(abcd_ACES_3,abcd_p_familyhistory_ACES[,c("src_subject_id","eventname","ParentalCriminal1Y0N","ParentalMDD1Y0N","ParentalSuicide1Y0N","ParentalAlcoholProblems1Y0N")] , by =c("src_subject_id", "eventname"), all=TRUE)

####################
# IN THE BELOW CODE BLOCK
# Neglect
# Physical, Emotional
####################

# Physical Neglect - five timepoints
abcd_y_parentalmonitoring <- read_csv(paste0(datapath, "/culture-environment/ce_y_pm.csv"))
abcd_y_parentalmonitoring_ACES<-abcd_y_parentalmonitoring%>%
  select(src_subject_id,eventname,
         parent_monitor_q1_y, # How often do your parents/guardians know where you are? 1 = Never, 2 = Almost Never, 3 = Sometimes, 4 = Often, 5 = Always or Almost Always, 777 = Decline to answer
         parent_monitor_q3_y) # If you are at home when your parents or guardians are not, how often do you know how to get in touch with them? 1 = Never, 2 = Almost Never, 3 = Sometimes, 4 = Often, 5 = Always or Almost Always, 777 = Decline to answer

abcd_y_parentalmonitoring_ACES$ParentalKnowWhereYouAre[abcd_y_parentalmonitoring_ACES$parent_monitor_q1_y=="3"]<-"0"
abcd_y_parentalmonitoring_ACES$ParentalKnowWhereYouAre[abcd_y_parentalmonitoring_ACES$parent_monitor_q1_y>="3"]<-"0"
abcd_y_parentalmonitoring_ACES$ParentalKnowWhereYouAre[abcd_y_parentalmonitoring_ACES$parent_monitor_q1_y<="3"]<-"1"
abcd_y_parentalmonitoring_ACES$YouKnowWhereParentsAre[abcd_y_parentalmonitoring_ACES$parent_monitor_q3_y=="3"]<-"0"
abcd_y_parentalmonitoring_ACES$YouKnowWhereParentsAre[abcd_y_parentalmonitoring_ACES$parent_monitor_q3_y>="3"]<-"0"
abcd_y_parentalmonitoring_ACES$YouKnowWhereParentsAre[abcd_y_parentalmonitoring_ACES$parent_monitor_q3_y<="3"]<-"1"
table(abcd_y_parentalmonitoring_ACES$ParentalKnowWhereYouAre)
table(abcd_y_parentalmonitoring_ACES$YouKnowWhereParentsAre)
table(abcd_y_parentalmonitoring_ACES$ParentalKnowWhereYouAre,abcd_y_parentalmonitoring_ACES$eventname)
table(abcd_y_parentalmonitoring_ACES$YouKnowWhereParentsAre,abcd_y_parentalmonitoring_ACES$eventname)

# Done with parental monitoring spreadsheet, merge with existing key columns
abcd_ACES_5<-merge(abcd_ACES_4,abcd_y_parentalmonitoring_ACES[,c("src_subject_id","eventname","YouKnowWhereParentsAre","ParentalKnowWhereYouAre")] , by =c("src_subject_id", "eventname"), all=TRUE)

#emotional neglect - 4 timepoints missing year 2 
abcd_y_ParentalAcceptance <- read_csv(paste0(datapath, "/culture-environment/ce_y_crpbi.csv"))
abcd_y_ParentalAcceptance_ACES<-abcd_y_ParentalAcceptance%>%
  select(src_subject_id,eventname,
         crpbi_parent4_y) # (Caregiver 1) Believes in showing their love for me. 1 = Not like them, 2 = Somewhat like them, 3 = A lot like them

abcd_y_ParentalAcceptance_ACES$ParentalEmotionalNeglectProxy[abcd_y_ParentalAcceptance_ACES$crpbi_parent4_y=="1"]<-"1"
abcd_y_ParentalAcceptance_ACES$ParentalEmotionalNeglectProxy[abcd_y_ParentalAcceptance_ACES$crpbi_parent4_y=="2"]<-"0"
abcd_y_ParentalAcceptance_ACES$ParentalEmotionalNeglectProxy[abcd_y_ParentalAcceptance_ACES$crpbi_parent4_y=="3"]<-"0"
table(abcd_y_ParentalAcceptance_ACES$ParentalEmotionalNeglectProxy,abcd_y_ParentalAcceptance_ACES$crpbi_parent4_y )
table(abcd_y_ParentalAcceptance_ACES$ParentalEmotionalNeglectProxy,abcd_y_ParentalAcceptance_ACES$eventname )

# Done with parental acceptance spreadsheet, merge with existing key columns
abcd_ACES_6<-merge(abcd_ACES_5,abcd_y_ParentalAcceptance_ACES[,c("src_subject_id","eventname","ParentalEmotionalNeglectProxy")] , by =c("src_subject_id", "eventname"), all=TRUE)

####################
# IN THE BELOW CODE BLOCK
# Merge in Demographic Data
####################

# Abuse 
#  - Emotional
#  - Physical
#  - Sexual
# Household Challenges 
#  - Mother treated violently
#  - Substance Abuse
#  - Mental Illness
#  - Parental Separation or Divorce
#  - Incarcerated Household Member
# Neglect
#  - Emotional
#  - Physical

#### ACEs Categories and Sum Determinants ####
# Filter for Baseline only then combine and make ACEs totals 
# household mental illness - ParentalMDD1Y0N; ParentalSuicide1Y0N
# household substance use - ParentalAlcoholProblems1Y0N
# divorce/separation - parent_divorcedseparateddead
# incarceration of household member - ParentalCriminal1Y0N
# emotional neglect -  ParentalEmotionalNeglectProxy
# physical neglect -  ParentalKnowWhereYouAre, YouKnowWhereParentsAre
# sexual abuse - SexualAbuse1Y0N
# physical abuse- PhysicalAbuse1Y0N
# emotional abuse - EmotionalAbuse1Y0N
# Household Violence YN <-YN<- total =FamilyEnviron_Youth_ViolenceTotal+FamilyEnviron_Parent_ViolenceTotal+WitnessViolenceHousehold
#

# Add Demographics Data
abcd_p_demo <- read_csv(paste0(datapath, "/abcd-general/abcd_p_demo.csv"))
data4 <- abcd_p_demo%>%select(src_subject_id,eventname,
                              demo_comb_income_v2, # What is your TOTAL COMBINED FAMILY INCOME for the past 12 months? This should include income (before taxes and deductions) from all sources, wages, rent from properties, social security, disability and/or veteran's benefits, unemployment benefits, workman's compensation, help from relative (include child payments and alimony), and so on. 1= Less than $5,000; 2=$5,000 through $11,999; 3=$12,000 through $15,999; 4=$16,000 through $24,999; 5=$25,000 through $34,999; 6=$35,000 through $49,999; 7=$50,000 through $74,999; 8= $75,000 through $99,999; 9=$100,000 through $199,999; 10=$200,000 and greater. 999 = Don't know ; 777 = Refuse to answer
                              race_ethnicity, # Race Ethnicity (Child) - 1 = White; 2 = Black; 3 = Hispanic; 4 = Asian; 5 = Other
                              demo_sex_v2, # What sex was the child assigned at birth, on the original birth certificate? 1 = Male, 2 = Female, 3 = Intersex Male, 4 = Intersex Female, 999 = Don't Know, 777 = Refuse to Answer
                              demo_prnt_marital_v2) # Are you now married, widowed, divorced, separated, never married or living with a partner? 1 = Married; 2 = Widowed; 3 = Divorced; 4 = Separated; 5 = Never married; 6 = Living with partner; 777 = Refused to answer

data4$demo_prnt_marital_v2_hrcode[data4$demo_prnt_marital_v2 == "1" ] <- "Married"
data4$demo_prnt_marital_v2_hrcode[data4$demo_prnt_marital_v2 == "2" ] <- "Widowed"
data4$demo_prnt_marital_v2_hrcode[data4$demo_prnt_marital_v2 == "3" ] <- "Divorced"
data4$demo_prnt_marital_v2_hrcode[data4$demo_prnt_marital_v2 == "4" ] <- "Separated"
data4$demo_prnt_marital_v2_hrcode[data4$demo_prnt_marital_v2 == "5" ] <- "Never Married"

data4$parent_divorcedseparateddead[data4$demo_prnt_marital_v2_hrcode == "Married" ] <- "0"
data4$parent_divorcedseparateddead[data4$demo_prnt_marital_v2_hrcode == "Widowed" ] <- "1"
data4$parent_divorcedseparateddead[data4$demo_prnt_marital_v2_hrcode == "Divorced" ] <- "1"
data4$parent_divorcedseparateddead[data4$demo_prnt_marital_v2_hrcode == "Separated" ] <- "1"
data4$parent_divorcedseparateddead[data4$demo_prnt_marital_v2_hrcode == "Never Married" ] <- "1"

#TOTAL COMBINED FAMILY INCOME for the past 12 months; 1= Less than $5,000; 2=$5,000 through $11,999; 3=$12,000 through $15,999; 4=$16,000 through $24,999; 
# 5=$25,000 through $34,999; 6=$35,000 through $49,999; 7=$50,000 through $74,999; 8= $75,000 through $99,999; 9=$100,000 through $199,999; 10=$200,000 and greater. 
# 999 = Don't know No lo s‚àö√â¬¨¬© ; 777 = Refuse to answer No deseo responder | If Separated/Divorced, please average the two household incomes. Si es Separado(a) / Divorciado(a), por favor promedie los dos ingresos familiares
data4$demo_comb_income_v2<-as.factor(data4$demo_comb_income_v2)
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "1" ] <- "Less than $5,000"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "2" ] <- "$5,000 through $11,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "3" ] <- "$12,000 through $15,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "4" ] <- "$16,000 through $24,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "5" ] <- "$25,000 through $34,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "6" ] <- "$35,000 through $49,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "7" ] <- "$50,000 through $74,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "8" ] <- "$75,000 through $99,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "9" ] <- "$100,000 through $199,999"
data4$demo_comb_income_hrcode[data4$demo_comb_income_v2 == "10" ] <- "$200,000 and over"

#Race Ethnicity (Child) recode 1 = White; 2 = Black; 3 = Hispanic; 4 = Asian; 5 = Other
data4$race_ethnicity<-as.factor(data4$race_ethnicity)
data4$race_ethnicity_hrcode[data4$race_ethnicity == "1" ] <- "White"
data4$race_ethnicity_hrcode[data4$race_ethnicity == "2" ] <- "Black"
data4$race_ethnicity_hrcode[data4$race_ethnicity == "3" ] <- "Hispanic"
data4$race_ethnicity_hrcode[data4$race_ethnicity == "4" ] <- "Asian"
data4$race_ethnicity_hrcode[data4$race_ethnicity == "4" ] <- "Other"

#sex at birth recode 1 male 2 female 3 intersex male 4 intersex female 999 Don't know 777 refuse to answer
data4$demo_sex_v2<-as.factor(data4$demo_sex_v2)
data4$demo_sex_hrcode[data4$demo_sex_v2 == "1" ] <- "Male"
data4$demo_sex_hrcode[data4$demo_sex_v2 == "2" ] <- "Female"
data4$demo_sex_hrcode[data4$demo_sex_v2 == "3" ] <- "Intersex Male"

# Done with demographics spreadsheet, merge with existing key columns
abcd_ACES_7<-merge(data4[,c("src_subject_id","eventname","demo_sex_hrcode","demo_comb_income_hrcode", "race_ethnicity_hrcode", "parent_divorcedseparateddead" )], abcd_ACES_6 , by =c("src_subject_id", "eventname"), all=TRUE)


####################
# IN THE BELOW CODE BLOCK
# Add Age and Family ID
####################

abcd_y_lt <- read_csv(paste0(datapath, "/abcd-general/abcd_y_lt.csv"))
# subject ID # eventname # Study site # Family ID #Participant's age in month at start of the event
data<-abcd_y_lt%>%
  select(src_subject_id,eventname, 
         site_id_l,     # Site ID
         rel_family_id, # Family ID
         interview_age) # age at interview in MONTHS

# Done with longitudinal variables spreadsheet, merge with existing key columns
abcd_ACES_Final<-merge(data[,c("src_subject_id","eventname", "rel_family_id", "interview_age")], abcd_ACES_7, by =c("src_subject_id","eventname"), all=TRUE)

####################
# IN THE BELOW CODE BLOCK
# Combine ACEs Categories and Sum Determinants
####################

#### ACEs Categories and Sum Determinants ####
# Abuse 
#  - Emotional - EmotionalAbuse1Y0N
#  - Physical - PhysicalAbuse1Y0N
#  - Sexual - SexualAbuse1Y0N
# Household Challenges 
#  - Mother treated violently - Household Violence YN <-YN<- total =FamilyEnviron_Youth_ViolenceTotal+FamilyEnviron_Parent_ViolenceTotal+WitnessViolenceHousehold
#  - Substance Abuse - ParentalAlcoholProblems1Y0N
#  - Mental Illness - ParentalMDD1Y0N; ParentalSuicide1Y0N
#  - Parental Separation or Divorce - parent_divorcedseparateddead
#  - Incarcerated Household Member - ParentalCriminal1Y0N
# Neglect
#  - Emotional -  ParentalEmotionalNeglectProxy
#  - Physical -  ParentalKnowWhereYouAre, YouKnowWhereParentsAre


# Filter for Baseline only then combine and make ACEs totals 
# household mental illness - ParentalMDD1Y0N; ParentalSuicide1Y0N
# household substance use - ParentalAlcoholProblems1Y0N
# divorce/separation - parent_divorcedseparateddead
# incarceration of household member - ParentalCriminal1Y0N
# emotional neglect -  ParentalEmotionalNeglectProxy
# physical neglect -  ParentalKnowWhereYouAre, YouKnowWhereParentsAre
# sexual abuse - SexualAbuse1Y0N
# physical abuse- PhysicalAbuse1Y0N
# emotional abuse - EmotionalAbuse1Y0N
# Household Violence YN <-YN<- total =FamilyEnviron_Youth_ViolenceTotal+FamilyEnviron_Parent_ViolenceTotal+WitnessViolenceHousehold

#### Calculating ACEs - Total and Groups ####

#### ACEs Categories and Sum Determinants ####
# Filter for Baseline only
abcd_ACES_Final%>%filter(abcd_ACES_Final$eventname=="baseline_year_1_arm_1")->ACEs_baseline

# Combine ACEs Categories
# household mental illness - ParentalMDD1Y0N; ParentalSuicide1Y0N
ACEs_baseline$HouseholdMentalIllness<-ACEs_baseline$ParentalMDD1Y0N+ACEs_baseline$ParentalSuicide1Y0N
ACEs_baseline$HouseholdMentalIllness[ACEs_baseline$HouseholdMentalIllness==0]<-0
ACEs_baseline$HouseholdMentalIllness[ACEs_baseline$HouseholdMentalIllness==2]<-1
ACEs_baseline$HouseholdMentalIllness[ACEs_baseline$HouseholdMentalIllness==1]<-1
table(ACEs_baseline$HouseholdMentalIllness)
as.numeric(ACEs_baseline$HouseholdMentalIllness)->ACEs_baseline$HouseholdMentalIllness

# physical neglect -  ParentalKnowWhereYouAre, YouKnowWhereParentsAre
as.numeric(ACEs_baseline$ParentalKnowWhereYouAre)->ACEs_baseline$ParentalKnowWhereYouAre
as.numeric(ACEs_baseline$YouKnowWhereParentsAre)->ACEs_baseline$YouKnowWhereParentsAre

ACEs_baseline$PhysicalNeglect<-ACEs_baseline$ParentalKnowWhereYouAre+ACEs_baseline$YouKnowWhereParentsAre
ACEs_baseline$PhysicalNeglect[ACEs_baseline$PhysicalNeglect==0]<-0
ACEs_baseline$PhysicalNeglect[ACEs_baseline$PhysicalNeglect==2]<-1
ACEs_baseline$PhysicalNeglect[ACEs_baseline$PhysicalNeglect==1]<-1
table(ACEs_baseline$PhysicalNeglect)
as.numeric(ACEs_baseline$PhysicalNeglect)->ACEs_baseline$PhysicalNeglect


# Household Violence YN <-YN<- total =FamilyEnviron_Youth_ViolenceTotal+FamilyEnviron_Parent_ViolenceTotal+WitnessViolenceHousehold
as.numeric(ACEs_baseline$FamilyEnviron_Youth_Violence1Y0N)->ACEs_baseline$FamilyEnviron_Youth_Violence1Y0N
as.numeric(ACEs_baseline$FamilyEnviron_Parent_Violence1Y0N)->ACEs_baseline$FamilyEnviron_Parent_Violence1Y0N
as.numeric(ACEs_baseline$WitnessViolenceHousehold)->ACEs_baseline$WitnessViolenceHousehold

ACEs_baseline$HouseholdViolence<-ACEs_baseline$FamilyEnviron_Youth_Violence1Y0N+ACEs_baseline$FamilyEnviron_Parent_Violence1Y0N+ACEs_baseline$WitnessViolenceHousehold
ACEs_baseline$HouseholdViolence[ACEs_baseline$HouseholdViolence==0]<-0
ACEs_baseline$HouseholdViolence[ACEs_baseline$HouseholdViolence==2]<-1
ACEs_baseline$HouseholdViolence[ACEs_baseline$HouseholdViolence==1]<-1
ACEs_baseline$HouseholdViolence[ACEs_baseline$HouseholdViolence==3]<-1
table(ACEs_baseline$HouseholdViolence)
as.numeric(ACEs_baseline$HouseholdViolence)->ACEs_baseline$HouseholdViolence

### Already Yes/No Flags
# household substance use - ParentalAlcoholProblems1Y0N
# divorce/separation - parent_divorcedseparateddead
# incarceration of household member - ParentalCriminal1Y0N
# emotional neglect -  ParentalEmotionalNeglectProxy
# sexual abuse - SexualAbuse1Y0N
# physical abuse- PhysicalAbuse1Y0N
# emotional abuse - EmotionalAbuse1Y0N

as.numeric(ACEs_baseline$parent_divorcedseparateddead)->ACEs_baseline$parent_divorcedseparateddead
as.numeric(ACEs_baseline$ParentalEmotionalNeglectProxy)->ACEs_baseline$ParentalEmotionalNeglectProxy

# Sum Determinants
ACEs_baseline$ACEs_Total<-rowSums(ACEs_baseline[,c("HouseholdViolence","PhysicalNeglect","HouseholdMentalIllness","ParentalAlcoholProblems1Y0N","parent_divorcedseparateddead", "ParentalCriminal1Y0N","ParentalEmotionalNeglectProxy","SexualAbuse1Y0N","PhysicalAbuse1Y0N","EmotionalAbuse1Y0N")], na.rm=TRUE)
ACEs_baseline$ACEs_Category[ACEs_baseline$ACEs_Total==0]<-"0"
ACEs_baseline$ACEs_Category[ACEs_baseline$ACEs_Total==1]<-"1 to 3"
ACEs_baseline$ACEs_Category[ACEs_baseline$ACEs_Total==2]<-"1 to 3"
ACEs_baseline$ACEs_Category[ACEs_baseline$ACEs_Total==3]<-"1 to 3"
ACEs_baseline$ACEs_Category[ACEs_baseline$ACEs_Total==4]<-"4+"

ACEs_baseline$Abuse_ACEs<-rowSums(ACEs_baseline[,c("PhysicalAbuse1Y0N","HouseholdViolence","EmotionalAbuse1Y0N","SexualAbuse1Y0N")])
ACEs_baseline$Neglect_ACEs<-rowSums(ACEs_baseline[,c("PhysicalNeglect","ParentalEmotionalNeglectProxy")])


####################
# IN THE BELOW CODE BLOCK
# Write to CSV
####################

write_csv(ACEs_baseline, paste0(derivative_data, "ACES_baseline_output.csv"))

# make dataframe of just subjects, totals, and human-readable categories
ACEs_baseline%>%select(src_subject_id,ACEs_Total,ACEs_Category)->ACEs_baseline_minimum

write_csv(ACEs_baseline_minimum, paste0(derivative_data, "ACES_baseline_mini_output.csv"))
