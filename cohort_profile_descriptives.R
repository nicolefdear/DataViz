######################################################################
# PROGRAM NAME: 		   Cohort profile descriptives     
# AUTHOR:              Nicole Dear
# DATE WRITTEN:        22Jan2024
# REVIEWED BY:		
# DATE REVIEWED:		
# WRITTEN FOR:         AFRICOS
# PURPOSE:             
# OVERVIEW:                                                       
# INPUT DATA:          data from Fauci: /group/Stat/NEW/RV329/Dataout_V5_CP/Data_freeze/20240901
# OUTPUT DATA:           
# RELIES ON:             
# RELIED ON BY:          
# SPECIAL INSTRUCTIONS:             
# MODIFICATION HISTORY:  
# DATE	MODIFIER	DESCRIPTION/REASON
######################################################################


## SETUP -----

# global settings
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)  # view more columns

# load packages
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(textclean)
library(ggpubr)
library(stringr)
library(tidyverse)
library(lubridate)
library(dvmisc)
library(table1)
library(arsenal)
library(kableExtra)
library(data.table)

# set working directory
setwd("G:/DCAC/DCAC_PEPFAR/RV329/Data In/090124_freeze")

# load data
hivstat0 <- read_sas("hivstat0.sas7bdat")
names(hivstat0) <- tolower(names(hivstat0))

subjq_dm0 <- read_sas("subjq_dm0.sas7bdat")
names(subjq_dm0) <- tolower(names(subjq_dm0))

subjq_a0 <- read_sas("subjq_a0.sas7bdat")
names(subjq_a0) <- tolower(names(subjq_a0))

v_load0 <- read_sas("v_load0.sas7bdat")
names(v_load0) <- tolower(names(v_load0))

lymph0 <- read_sas("lymph0.sas7bdat")
names(lymph0) <- tolower(names(lymph0))

stat_chg0 <- read_sas("stat_chg0.sas7bdat")
names(stat_chg0) <- tolower(names(stat_chg0))

arvcode0 <- read_sas("arvcode0.sas7bdat")
names(arvcode0) <- tolower(names(arvcode0))


## DATA CLEANING -----

hivstat1 <- hivstat0 %>%
  select(subjid, visit, visitdt, gender, age, dobdtn, hivflag, hivstat, progid, diagdtn, art_sdtn, dur_art, dur_hiv, enroldtn) %>%
  filter(visit<90) %>% 
  # remove participants with duplicate visits with wrong date
  filter(!(subjid=="A01-0382" & visit==17 & visitdt=='2023-11-23')) %>%
  filter(!(subjid=="C01-0064" & visit==2 & visitdt=='2023-10-25')) %>%
  filter(!(subjid=="C01-0067" & visit==2 & visitdt=='2023-11-15')) %>%
  filter(!(subjid=="C01-0139" & visit==9 & visitdt=='2023-09-20')) %>%
  filter(!(subjid=="D01-0022" & visit==13 & visitdt=='2023-10-17')) %>%
  filter(!(subjid=="D01-0226" & visit==15 & visitdt=='2023-10-18')) %>%
  filter(!(subjid=="D01-0277" & visit==1 & visitdt=='2023-12-07'))

hivstat1$visit[hivstat1$subjid=="C01-0044" & hivstat1$visit==19 & hivstat1$visitdt=='2023-10-31'] <- 20
hivstat1$visit[hivstat1$subjid=="C01-0116" & hivstat1$visit==18 & hivstat1$visitdt=='2023-08-03'] <- 19
hivstat1$visit[hivstat1$subjid=="D01-0405" & hivstat1$visit==16 & hivstat1$visitdt=='2022-10-26'] <- 14
hivstat1$visit[hivstat1$subjid=="D02-0008" & hivstat1$visit==12 & hivstat1$visitdt=='2023-10-16'] <- 13

hivstat1 <- hivstat1 %>%
  distinct(subjid, visit, .keep_all = TRUE)

# check for dups by subjid and visit
duptest1 <- hivstat1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest1$dupe)

subjq_dm1 <- subjq_dm0 %>%
  filter(visit<90) %>% 
  # remove participants with duplicate visits with wrong date
  filter(!(subjid=="D01-0045" & visit==18 & visitdt=='2023-01-25')) %>%
  filter(!(subjid=="D01-0555" & visit==3 & visitdt=='2023-03-01')) %>% 
  distinct(subjid, visit, .keep_all = TRUE) %>% 
  select(subjid, visit, educat, eductxt, employed, hhincome, hcurrtyp, hcurrtxt, hholdur)

# check for dups by subjid and visit
duptest2 <- subjq_dm1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest2$dupe)

subjq_a1 <- subjq_a0 %>%
  select(subjid, visit, visitdt, sequence, hivinfct, hivintxt, hivinf, takearv, pillnum, pillnr, missarv) %>% 
  filter(visit<90 & sequence==1) %>%
  # convert unk/no response values to missing
  mutate(missarv = na_if(missarv, 7)) %>%
  mutate(hivintxt_new = strip(hivintxt, lower.case = FALSE, apostrophe.remove=TRUE)) %>% 
  # remove participants with duplicate visits with wrong date
  filter(!(subjid=="D01-0045" & visit==18 & visitdt=='2023-01-25')) %>%
  filter(!(subjid=="D01-0371" & visit==15 & visitdt=='2022-02-21')) %>% 
  filter(!(subjid=="D02-0010" & visit==10 & visitdt=='2023-03-22')) %>% 
  distinct(subjid, visit, .keep_all = TRUE)

# check for dups by subjid and visit
duptest3 <- subjq_a1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest3$dupe)

v_load1 <- v_load0 %>%
  select(subjid, visit, visitdt, sequence, drawper, drawdt, vl_circ, vlcopy, nodetect) %>%
  # include only rows with draw at study visit
  filter(drawper==1 & visit<90) %>%
  # remove duplicate rows (for D02-0124, this row with visit=2 is a dup of visit 7
  # and for B01-0033, this row is a dup where the other visit 7 row has non-missing vlcopy)
  filter(!(subjid=="B01-0033" & visit==7 & is.na(vlcopy))) %>%
  filter(!(subjid=="D02-0124" & visitdt=="2022-08-19" & visit==2)) %>%
  filter(!(subjid=="D01-0022" & visitdt=="2023-10-17" & visit==13)) %>%
  filter(!(subjid=="D01-0277" & visitdt=="2023-12-07" & visit==1)) %>%
  # remove rows that have same subjid, visit, and vlcopy
  distinct(subjid, visit, vlcopy, .keep_all = TRUE) %>%
  # variable to check if visit values are repeated within a subjid
  group_by(subjid, visit) %>%
  mutate(dupflag = if_else(n()>1, 1, 0)) %>%
  # remove rows from same visit with sequence greater than 1
  filter(!(dupflag==1 & sequence!=1)) %>% 
  # clean vl variable
  mutate(vl_clean = case_when(nodetect==1 & is.na(vlcopy) ~ "undetectable",
                              vl_circ==1 ~ paste0("<", vlcopy),
                              vl_circ==2 | is.na(vl_circ) ~ as.character(vlcopy),
                              vl_circ==3 ~ paste0(">", vlcopy))) %>% 
  mutate(vls1000 = case_when(nodetect==1 | vlcopy<1000 ~ 1,
                             vlcopy>=1000 ~ 0,
                             .default = NA)) %>% 
  mutate(vls200 = case_when(nodetect==1 | vlcopy<200 ~ 1,
                             vlcopy>=200 ~ 0,
                             .default = NA)) %>% 
  mutate(vls50 = case_when(nodetect==1 | vlcopy<50 ~ 1,
                             vlcopy>=50 ~ 0,
                             .default = NA))
  

v_load1$visit[v_load1$subjid=="D01-0405" & v_load1$visit==16 & v_load1$visitdt=='2022-10-26'] <- 14
v_load1$visit[v_load1$subjid=="D02-0008" & v_load1$visit==12 & v_load1$visitdt=='2023-10-16'] <- 13

# check for dups by subjid and visit
duptest3 <- v_load1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest3$dupe)

lymph1 <- lymph0 %>%
  select(subjid, visit, visitdt, sequence, drawper, drawdt, cd3_4_n) %>%
  # include only rows with draw at study visit
  filter(drawper==1 & visit<90 & !is.na(cd3_4_n)) %>% 
  # drop rows with incorrect visit #s/dates
  filter(!(subjid=="A01-0023" & visitdt=="2022-11-29" & visit==2)) %>%
  filter(!(subjid=="A01-0024" & visitdt=="2022-11-08" & visit==2)) %>%
  filter(!(subjid=="A01-0030" & visitdt=="2022-11-29" & visit==2)) %>%
  filter(!(subjid=="A01-0032" & visitdt=="2022-11-21" & visit==2)) %>%
  filter(!(subjid=="A01-0036" & visitdt=="2022-12-12" & visit==2)) %>%
  filter(!(subjid=="A01-0037" & visitdt=="2022-11-01" & visit==2)) %>%
  filter(!(subjid=="A01-0042" & visitdt=="2022-11-15" & visit==2)) %>%
  filter(!(subjid=="A01-0044" & visitdt=="2022-11-16" & visit==2)) %>%
  filter(!(subjid=="A01-0047" & visitdt=="2022-11-15" & visit==2)) %>%
  filter(!(subjid=="A01-0048" & visitdt=="2022-12-05" & visit==2)) %>%
  filter(!(subjid=="A01-0051" & visitdt=="2022-11-15" & visit==2)) %>%
  filter(!(subjid=="A01-0071" & visitdt=="2022-11-01" & visit==2)) %>%
  filter(!(subjid=="A01-0081" & visitdt=="2022-12-01" & visit==2)) %>%
  filter(!(subjid=="A01-0087" & visitdt=="2022-11-28" & visit==2)) %>%
  filter(!(subjid=="A01-0089" & visitdt=="2022-11-28" & visit==2)) %>%
  filter(!(subjid=="A01-0090" & visitdt=="2022-11-30" & visit==2)) %>%
  filter(!(subjid=="A01-0093" & visitdt=="2022-12-01" & visit==2)) %>%
  filter(!(subjid=="A01-0094" & visitdt=="2022-12-12" & visit==2)) %>%
  filter(!(subjid=="A01-0096" & visitdt=="2022-12-06" & visit==2)) %>%
  filter(!(subjid=="A01-0099" & visitdt=="2022-12-14" & visit==2)) %>%
  filter(!(subjid=="A01-0107" & visitdt=="2023-01-04" & visit==2)) %>%
  filter(!(subjid=="A01-0125" & visitdt=="2023-01-18" & visit==2)) %>%
  filter(!(subjid=="C01-0139" & visitdt=="2023-09-20" & visit==9)) %>%
  filter(!(subjid=="D01-0022" & visitdt=="2023-10-17" & visit==13)) %>%
  filter(!(subjid=="D01-0058" & visitdt=="2023-08-24" & visit==10)) %>%
  filter(!(subjid=="D01-0277" & visitdt=="2023-12-07" & visit==1)) %>%
  # variable to check if visit values are repeated within a subjid
  group_by(subjid, visit) %>%
  mutate(dupflag = if_else(n()>1, 1, 0)) %>%
  # remove rows from same visit with sequence greater than 1
  filter(!(dupflag==1 & sequence!=1)) %>% 
  # create categorical version of CD4 count
  mutate(cd4cat = case_when(cd3_4_n<250 ~ 1,
                            cd3_4_n>=250 & cd3_4_n<500 ~ 2,
                            cd3_4_n>=500 ~ 3,
                            .default = NA))

lymph1$visit[lymph1$subjid=="C01-0044" & lymph1$visit==19 & lymph1$visitdt=='2023-10-31'] <- 20
lymph1$visit[lymph1$subjid=="D01-0405" & lymph1$visit==16 & lymph1$visitdt=='2022-10-26'] <- 14
lymph1$visit[lymph1$subjid=="D02-0008" & lymph1$visit==12 & lymph1$visitdt=='2023-10-16'] <- 13

lymph1 <- lymph1 %>% 
  distinct(subjid, visit, .keep_all = TRUE)

# ART data
arvcode1 <- arvcode0 %>%
  select(subjid, visit, medicat, arv_code, startdtcn, astartdtcn) %>%
  filter(visit<90)

# check for dups by subjid and visit
duptest4 <- arvcode1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest4$dupe)


## MERGE DATA -----

data0 <- hivstat1 %>%
  full_join(subjq_dm1, by = c("subjid", "visit")) %>%
  full_join(subjq_a1, by = c("subjid", "visit")) %>%
  full_join(v_load1, by = c("subjid", "visit")) %>% 
  full_join(lymph1, by = c("subjid", "visit")) %>%
  full_join(arvcode1, by = c("subjid", "visit")) %>%
  mutate(visitdt = if_else(!is.na(visitdt.x), visitdt.x, visitdt.y, NA)) %>% 
  arrange(subjid, visitdt)


## ADDITIONAL DATA CLEANING ON COMBINED DATASET -----

# code free text responses for 'other' education
educat1 <- c("1", "ONE", "2", "TWO", "PRIMARY")
educat3 <- c("3", "FOUR", "SEVEN", "8", "EIGHT", "SECONDARY")
educat5 <- c("COLLAGE", "COLLEGE", "UNIVERSITY")
educat6 <- c("POST GRADUATE")

data0$flag_educat1 <- ifelse(grepl(paste(educat1, collapse='|'), data0$eductxt), 1, 0)
data0$flag_educat3 <- ifelse(grepl(paste(educat3, collapse='|'), data0$eductxt), 1, 0)
data0$flag_educat5 <- ifelse(grepl(paste(educat5, collapse='|'), data0$eductxt), 1, 0)
data0$flag_educat6 <- ifelse(grepl(paste(educat6, collapse='|'), data0$eductxt), 1, 0)

# code free text responses for route of HIV transmission
freetext <- as.data.frame(table(data0$hivintxt_new))

# define patterns to search for
mtct_text <- c('AT BIRTH', 'BORN WITH', 'BREAST MILK', 'BREST FEEDING', 'DURING DELIVERY', 'MOTHER TO CHILD', 'MOTHER TOCHILD', 
               'FROM THE PARENTS', 'PMTCT', 'MTCT', 'VERRTICAL TRANSMISSION', 'VERTICAL')
unk_text <- c('CAN NOT RECALL', 'CANNOT IDENTIFY HOW HE ACQUIRED', 'NOT SURE', 'DO NOT KNOW', 'DOES NOT KNOW', 'DOESNT KNOW',
              'DONT KNOW', 'DONT REMEMBER', 'NOT KNOW', 'NOT KNOWN', 'NOT KNOW', 'CANNOT TELL HOW SHE WAS INFECTED', 'UNKNOWN')

data0$flag_mtct <- ifelse(grepl(paste(mtct_text, collapse='|'), data0$hivintxt), 1, 0)
data0$flag_unk <- ifelse(grepl(paste(unk_text, collapse='|'), data0$hivintxt), 1, 0)

# create new variables and convert to factor
data1 <- data0 %>%
  
  # demographics
  mutate(gender = factor(gender,
                         levels = c(1,2),
                         labels = c("Male", "Female"))) %>%
  mutate(hivflag = factor(hivflag,
                          levels = c(1,2),
                          labels = c("PLWH", "PLWoH"))) %>% 
  mutate(hivstat = factor(hivstat,
                          levels = c(1,2),
                          labels = c("PLWH", "PLWoH"))) %>%
  mutate(education = case_when(educat==0 | educat==1 | (educat==90 & flag_educat1==1) ~ 0,
                               educat==2 | educat==3 | (educat==90 & flag_educat3==1) ~ 1,
                               educat>3 | (educat==90 & flag_educat5==1) | 
                                 (educat==90 & flag_educat6==1) ~ 2,
                               .default = NA)) %>% 
  mutate(education = factor(education,
                            levels = c(0,1,2),
                            labels = c("None or some primary", "Primary or some secondary", "Secondary and above"))) %>%
  mutate(country = case_when(progid==1 ~ 1,
                             progid==2 | progid==3 ~ 2,
                             progid==4 ~ 3,
                             progid==5 ~ 4,
                             .default = NA)) %>%
  mutate(progid = factor(progid,
                         levels = c(1,2,3,4,5),
                         labels = c("Kayunga, Uganda", "South Rift Valley, Kenya", 
                                    "Kisumu West, Kenya", "Mbeya, Tanzania", 
                                    "Abuja & Lagos Nigeria"))) %>%
  mutate(country = factor(country,
                          levels = c(1,2,3,4),
                          labels = c("Uganda", "Kenya", "Tanzania", "Nigeria"))) %>% 
  mutate(hholdur = factor(hholdur,
                          levels = c(1,2,3),
                          labels = c("Urban", "Peri-urban", "Rural"))) %>%
  mutate(year = year(visitdt)) %>%
  mutate(year = factor(year,
                       levels = c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024),
                       labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024"))) %>%
  
  # vertical vs non-vertical transmission
  mutate(vertical = case_when(hivinfct==8 | flag_mtct==1 ~ 1,
                              hivinfct==1 | hivinfct==2 | hivinfct==3 | hivinfct==4 | hivinfct==5 | hivinfct==6 | 
                                hivinfct==7 | (hivinfct==90 & flag_mtct!=1 & flag_unk!=1) ~ 0,
                              hivinfct==97 | hivinfct==98 | flag_unk==1 ~ NA,
                              .default = 0)) %>%
  mutate(vertical = factor(vertical,
                           levels = c(0,1),
                           labels = c("Not vertical transmission", "Vertical transmission"))) %>% 

  # create SES variable (quartiles of income by country)
  mutate(income_quartiles = create_qgroups(hhincome, groups = 4, strata = country)) %>% 
  
  # HIV-related variables
  # mutate(vls1000 = factor(vls1000,
  #                         levels = c(0,1),
  #                         labels = c("VL>=1000 c/ml", "VL<1000 c/ml"))) %>%
  mutate(cd4cat = factor(cd4cat,
                         levels = c(1,2,3),
                         labels = c("<250 cells/mm3", "250 to <500 cells/mm3", ">=500 cells/mm3"))) %>%

  # fill in values for variables collected at enrollment/added in later amendment
  group_by(subjid) %>%
  fill(c(hivflag, age, gender, progid, country, dobdtn, hholdur, education, income_quartiles, vertical), .direction = "downup") %>%
  fill(c(diagdtn, art_sdtn), .direction = "down") %>%
  ungroup() %>%
  
  # calculate age at visit
  mutate(agev = as.numeric(difftime(visitdt, dobdtn) / 365.25)) %>%
  
  # calculate age at time of HIV dx
  mutate(age_dx = as.numeric(difftime(diagdtn, dobdtn) / 365.25)) %>%
  
  # create youth flag
  mutate(youth = if_else(agev<25, 1, 0)) %>% 
  
  # remove anyone with missing HIV status
  filter(!(is.na(hivflag)))

# fill in duration since HIV dx and duration on ART if missing
j <- is.na(data1$dur_hiv)
data1$dur_hiv[j] <- difftime(data1$visitdt[j], data1$diagdtn[j], units = "days") / 365.25

k <- is.na(data1$dur_art)
data1$dur_art[k] <- difftime(data1$visitdt[k], data1$art_sdtn[k], units = "days") / 365.25


## ART DATA CLEANING -----

# pull in names of ART corresponding to medication codes
data1$arv_name <- ""
data1$arv_name[data1$arv_code=="J12"] <- "Dolutegravir"
data1$arv_name[data1$arv_code=="J12,J54"] <- "Dolutegravir + Abacavir + Lamivudine"
data1$arv_name[data1$arv_code=="J17"] <- "Efavirenz + Emtricitabine + Tenofovir"
data1$arv_name[data1$arv_code=="J19"] <- "Emtricitabine"
data1$arv_name[data1$arv_code=="J19,J34"] <- "Emtricitabine + Rilpivirine"
data1$arv_name[data1$arv_code=="J25"] <- "Lamivudine + Nevirapine + Zidovudine"
data1$arv_name[data1$arv_code=="J26"] <- "Lamivudine + Zidovudine"
data1$arv_name[data1$arv_code=="J26,J42"] <- "Tenofovir + Lamivudine + Zidovudine"
data1$arv_name[data1$arv_code=="J28,J12"] <- "Dolutegravir + Lopinavir + Ritonavir"
data1$arv_name[data1$arv_code=="J29,J42"] <- "Tenofovir + Lamivudine + Lopinavir + Ritonavir"
data1$arv_name[data1$arv_code=="J34"] <- "Rilpivirine"
data1$arv_name[data1$arv_code=="J34,J19"] <- "Emtricitabine + Rilpivirine"
data1$arv_name[data1$arv_code=="J42"] <- "Tenofovir + Lamivudine"
data1$arv_name[data1$arv_code=="J43"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$arv_name[data1$arv_code=="J43,J42"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$arv_name[data1$arv_code=="J50"] <- "Tenofovir + Emtricitabine"
data1$arv_name[data1$arv_code=="J54"] <- "Abacavir + Lamivudine"
data1$arv_name[data1$arv_code=="J55"] <- "Tenofovir + Lamivudine + Dolutegravir"
data1$arv_name[data1$arv_code=="J56"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$arv_name[data1$arv_code=="J57"] <- "Abacair + Lamivudine + Dolutegravir"
data1$arv_name[data1$arv_code=="J57,J12"] <- "Abacair + Lamivudine + Dolutegravir"

# clean up medicat variable
data1$medicat_cl <- ""
data1$medicat_cl[data1$medicat=="10. Atazanazir(ATZ) 300mg"] <- "Atazanazir"
data1$medicat_cl[data1$medicat=="11. Atazanazir/Ritonavir 300mg/100mg"] <- "Atazanazir/Ritonavir"
data1$medicat_cl[data1$medicat=="11. Atazanazir/Ritonavir 300mg/100mg ; 9. Nevirapine(NVP) 200mg"] <- "Atazanazir/Ritonavir + Nevirapine"
data1$medicat_cl[data1$medicat=="12. Lopinavir/Ritonavir 400mg/100mg"] <- "Lopinavir/Ritonavir"
data1$medicat_cl[data1$medicat=="12. Lopinavir/Ritonavir 400mg/100mg ; 6. Tenofoivr(TDF) 300mg ; 4. Lamivudine(3TC) 300mg ; 12. Lopinavir/Ritonavir 400mg/100mg"] <- "Tenofovir + Lamivudine + Lopinavir/Ritonavir"
data1$medicat_cl[data1$medicat=="14. Emtricitabine(FTC) 200mg ; 6. Tenofoivr(TDF) 300mg"] <- "Tenofovir + Emtricitabine"
data1$medicat_cl[data1$medicat=="15. Lamivudine/Nevirapine/Zidovudine"] <- "Lamivudine + Nevirapine + Zidovudine"
data1$medicat_cl[data1$medicat=="16. Lamivudine/Zidovudine 150/300mg"] <- "Lamivudine + Zidovudine"
data1$medicat_cl[data1$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 10. Atazanazir(ATZ) 300mg"] <- "Lamivudine + Zidovudine + Atazanazir"
data1$medicat_cl[data1$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 11. Atazanazir/Ritonavir 300mg/100mg"] <- "Lamivudine + Zidovudine + Atazanazir/Ritonavir"
data1$medicat_cl[data1$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 11. Atazanazir/Ritonavir 300mg/100mg ; 6. Tenofoivr(TDF) 300mg"] <- "Lamivudine + Zidovudine + Atazanazir/Ritonavir + Tenofovir"
data1$medicat_cl[data1$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 12. Lopinavir/Ritonavir 400mg/100mg"] <- "Lamivudine + Zidovudine + Lopinavir/Ritonavir"
data1$medicat_cl[data1$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 8. Efavirenz(EFV) 600mg"] <- "Lamivudine + Zidovudine + Efavirenz"
data1$medicat_cl[data1$medicat=="17. Tenofovir/Lamivudine 300/300mg"] <- "Tenofovir + Lamivudine"
data1$medicat_cl[data1$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 10. Atazanazir(ATZ) 300mg"] <- "Tenofovir + Lamivudine + Atazanazir"
data1$medicat_cl[data1$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 11. Atazanazir/Ritonavir 300mg/100mg"] <- "Tenofovir + Lamivudine + Atazanazir/Ritonavir"
data1$medicat_cl[data1$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 12. Lopinavir/Ritonavir 400mg/100mg"] <- "Tenofovir + Lamivudine + Lopinavir/Ritonavir"
data1$medicat_cl[data1$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 15. Lamivudine/Nevirapine/Zidovudine"] <- "Tenofovir + Lamivudine + Nevirapine + Zidovudine"
data1$medicat_cl[data1$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 9. Nevirapine(NVP) 200mg"] <- "Tenofovir + Lamivudine + Nevirapine"
data1$medicat_cl[data1$medicat=="18. Tenofovir/Lamivudine/Efavirenz 3"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$medicat_cl[data1$medicat=="19"] <- "Tenofovir + Lamivudine + Dolutegravir"
data1$medicat_cl[data1$medicat=="19. Tenofovir/Lamivudine/Dolutegravi"] <- "Tenofovir + Lamivudine + Dolutegravir"
data1$medicat_cl[data1$medicat=="19 ; 17. Tenofovir/Lamivudine 300/300mg"] <- "Tenofovir + Lamivudine + Efavirenz + Dolutegravir"
data1$medicat_cl[data1$medicat=="19 ; 18. Tenofovir/Lamivudine/Efavirenz 3"] <- "Tenofovir + Lamivudine + Dolutegravir"
data1$medicat_cl[data1$medicat=="19 ; 19"] <- "Tenofovir + Lamivudine + Dolutegravir"
data1$medicat_cl[data1$medicat=="20. Tenofovir/Lamivudine/Efavirenz 3"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$medicat_cl[data1$medicat=="20"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$medicat_cl[data1$medicat=="21"] <- "Abacavir + Lamivudine + Dolutegravir"
data1$medicat_cl[data1$medicat=="6. Tenofoivr(TDF) 300mg"] <- "Tenofovir"
data1$medicat_cl[data1$medicat=="6. Tenofoivr(TDF) 300mg ; 4. Lamivudine(3TC) 300mg"] <- "Tenofovir + Lamivudine"
data1$medicat_cl[data1$medicat=="8. Efavirenz(EFV) 600mg"] <- "Efavirenz"
data1$medicat_cl[data1$medicat=="8. Efavirenz(EFV) 600mg ; 6. Tenofoivr(TDF) 300mg ; 4. Lamivudine(3TC) 300mg"] <- "Tenofovir + Lamivudine + Efavirenz"
data1$medicat_cl[data1$medicat=="9. Nevirapine(NVP) 200mg"] <- "Nevirapine"
data1$medicat_cl[data1$medicat=="9. Nevirapine(NVP) 200mg ; 6. Tenofoivr(TDF) 300mg ; 4. Lamivudine(3TC) 300mg"] <- "Nevirapine + Tenofovir + Lamivudine"
data1$medicat_cl[data1$medicat=="9. Nevirapine(NVP) 200mg ; 7. Zidovudine(AZT) 300mg ; 4. Lamivudine(3TC) 300mg"] <- "Nevirapine + Zidovudine + Lamivudine"

# create single variable for ART regimen
data1$art_regimen <- paste(data1$medicat_cl, data1$arv_name)

# remove extra spaces
data1$art_regimen <- gsub("\\s", "", data1$art_regimen)

# categorize by anchor drug
pi <- c("Atazanavir", "Darunavir", "Indinavir", "Lopinavir", "Ritonavir", "Saquinavir", "Tipranavir")

data2 <- data1 %>% 
  mutate(efv = if_else(grepl("Efavirenz", art_regimen, ignore.case=TRUE), 1, NA),
         nvp = if_else(grepl("Nevirapine", art_regimen, ignore.case=TRUE), 1, NA),
         dtg = if_else(grepl("Dolutegravir", art_regimen, ignore.case=TRUE), 1, NA),
         pi = if_else(grepl(paste(pi, collapse='|'), art_regimen, ignore.case=TRUE), 1, NA)) %>%
  mutate(anchor_drug0 = case_when(is.na(dur_art) | dur_art<0 ~ 0,
                                 efv==1 ~ 1,
                                 nvp==1 ~ 2,
                                 dtg==1 ~ 3,
                                 pi==1 ~ 4,
                                 .default = NA)) %>%
  mutate(anchor_drug = if_else(!is.na(anchor_drug0), anchor_drug0, 
                                    if_else(is.na(anchor_drug0) & dur_art>=0, 5, NA))) %>%
  mutate(anchor_drug = factor(anchor_drug,
                                   levels = c(0,1,2,3,4,5),
                                   labels = c("ART-naiive", "EFV", "NVP", "DTG", "PI-based", "Other")))


## DEMOGRAPHICS AT ENROLLMENT VISIT -----

# PLWH and PLWoH enrolled
data2 %>%
  group_by(subjid) %>%
  count(hivflag) %>%
  group_by(hivflag) %>%
  count()

# percent of cohort age 15-24 years
table(data2$visit, data2$youth)
100*(1062/4245)

# summarize age at enrollment, all sites and years
age_summary_all <- data2 %>%
  filter(visit==1 & !(is.na(age))) %>% 
  summarize(med = median(age),
            Q1 = quantile(age, 0.25),
            Q3 = quantile(age, 0.75))

# summarize and plot age by enrollment year
age_summary <- data2 %>%
  filter(visit==1 & !(is.na(age))) %>% 
  group_by(year, progid) %>%
  summarize(med = median(age),
            Q1 = quantile(age, 0.25),
            Q3 = quantile(age, 0.75))

plot1 <- ggplot(age_summary, aes(x = year, y = med, group = progid)) +
  geom_line(aes(color = progid)) +
  geom_point(aes(color = progid)) +
  labs(x = "Enrollment year", 
       y = "Median age (years) at enrollment") +
  guides(color = guide_legend(title = "Program site")) + 
  theme_pubr() + theme(legend.position = c(0.2, 0.2)) +
  lims(y = c(0, 45))

plot(plot1)

# summarize and plot % female by enrollment year
data2$female <- ifelse(data2$gender=="Female", 1, 0)

sex_summary_all <- data2 %>%
  filter(visit==1 & !(is.na(gender))) %>% 
  summarise(tot_female = sum(female), 
            n = n(),
            percent_female = 100*(sum(female)/n()))

sex_summary <- data2 %>%
  filter(visit==1 & !(is.na(gender))) %>%
  group_by(year) %>% 
  summarise(tot_female = sum(female), 
            n = n(),
            percent_female = 100*(sum(female)/n()))

# create labels
sex_summary$label <- paste(sex_summary$tot_female, " (", 
                              round(sex_summary$percent_female, digits = 1), "%)", sep = "")

# plot
plot2 <- ggplot(sex_summary, aes(x = year, y = percent_female)) +
  geom_bar(fill = "darkgrey", stat = "identity") +
  geom_text(aes(label = label), vjust = -0.4, size = 2.6) +
  theme_classic2() +
  xlab("Enrollment year") +
  ylab("Percent female") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11)) +
  lims(y = c(0, 100))

plot(plot2)


## CLINICAL CHARACTERISTICS FOR PLWH AT ENROLLMENT VISIT -----

v1 <- data2 %>%
  filter(visit==1 & hivflag=="PLWH")

# CD4 count
table(v1$hivflag)

# check missingness
sum(is.na(v1$cd3_4_n))

median(v1$cd3_4_n, na.rm = TRUE)
quantile(v1$cd3_4_n, 0.25, na.rm = TRUE)
quantile(v1$cd3_4_n, 0.75, na.rm = TRUE)

# new to HIV care
v1$durcare <- as.numeric(difftime(v1$visitdt, v1$enroldtn, units = "days") / 365.25)
v1 <- v1 %>% 
  mutate(newincare = if_else(durcare<=0.08333333, 1, 0)) %>% 
  mutate(newincare = factor(newincare,
                            levels = c(0, 1),
                            labels = c("Not new in care", "New in care"))) %>% 
  mutate(onart = if_else(dur_art>0 & !is.na(dur_art), 1, 0)) %>% 
  mutate(onart = factor(onart,
                        levels = c(0, 1),
                        labels = c("Not on ART", "On ART")))

table(v1$newincare, useNA = "always")
100*(713/(713+2687))

# % on ART among those not new in care
table(v1$newincare, v1$onart, useNA = "always")
100*(2338/(349+2338))


# summarize CD4 count by enrollment year
cd4_summary <- data1 %>%
  filter(visit==1 & hivflag=="PLWH" & !(is.na(cd3_4_n))) %>%
  group_by(year, progid) %>%
  summarize(med = median(cd3_4_n),
            Q1 = quantile(cd3_4_n, 0.25),
            Q3 = quantile(cd3_4_n, 0.75))

#labelsY = parse(text=paste("CD4 Count (Cells/mm", "^3", "*)", sep=""))
labelsY = expression(paste("CD4 Count (Cells/mm", "^3", "*)", sep=""))

plot3 <- ggplot(cd4_summary, aes(x = year, y = med, group = progid)) +
  geom_line(aes(color = progid)) +
  geom_point(aes(color = progid)) +
  labs(x = "Enrollment year", 
       y = labelsY) +
  guides(color = guide_legend(title = "Program site")) + 
  theme_pubr() + theme(legend.position = c(0.2, 0.8)) +
  lims(y = c(0, 910))

plot(plot3)


## MORTALITY -----

# note: schange=10 corresponds to death

stat_chg0 %>% 
  group_by(subjid) %>%
  count(schange) %>%
  group_by(schange) %>%
  count()

stat_chg1 <- stat_chg0 %>%
  arrange(subjid, visitdt) %>% 
  group_by(subjid) %>% 
  slice(tail(row_number(), 1))

table(stat_chg1$schange)
# n=193 deaths


## CONTINUUM OF CARE CASCADE AT ENROLLMENT VISIT -----

v1vl <- data2 %>%
  filter(visit==1 & hivflag=="PLWH" & dur_art>=0.5)

# check missingness
sum(is.na(v1vl$vl_clean))

# drop records with missing viral load
v1vl <- data2 %>%
  filter(visit==1 & hivflag=="PLWH" & dur_art>=0.5 & !is.na(vl_clean))

# summary
vs_summary <- v1vl %>%
  group_by(progid) %>%
  summarise(tot_vls1000 = sum(vls1000),
            tot_vls200 = sum(vls200),
            tot_vls50 = sum(vls50),
            n = n(),
            percent_vls1000 = 100*(sum(vls1000)/n()),
            percent_vls200 = 100*(sum(vls200)/n()),
            percent_vls50 = 100*(sum(vls50)/n()))

# reshape wide to long to plot
wide <- vs_summary[,c(1,6:8)]
long <- melt(setDT(wide), id.vars = c("progid"), variable.name = "cutoff")

levels(long$cutoff) <- c("<1000 c/mL", "<200 c/mL", "<50 c/mL")

# plot
ggplot(long, aes(x = cutoff, y = value, fill = progid))+
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Viral load threshold", 
       y = "Percent of PLWH on ART for 6 or more months \n virally suppressed",
       fill = "Study site") +
  scale_fill_brewer(palette="Accent") + 
  theme_minimal() +
  lims(y = c(0, 100))


## CONTINUUM OF CARE CASCADE AT MOST RECENT VISIT -----

# use most recent visit with available viral load
vl_recent <- data2 %>%
  filter(hivflag=="PLWH" & dur_art>=0.5 & !is.na(vl_clean)) %>% 
  group_by(subjid) %>% 
  slice(tail(row_number(), 1))

# summary
vs_summary2 <- vl_recent %>%
  group_by(progid) %>%
  summarise(tot_vls1000 = sum(vls1000),
            tot_vls200 = sum(vls200),
            tot_vls50 = sum(vls50),
            n = n(),
            percent_vls1000 = 100*(sum(vls1000)/n()),
            percent_vls200 = 100*(sum(vls200)/n()),
            percent_vls50 = 100*(sum(vls50)/n()))

# reshape wide to long to plot
wide <- vs_summary2[,c(1,6:8)]
long <- melt(setDT(wide), id.vars = c("progid"), variable.name = "cutoff")

levels(long$cutoff) <- c("<1000 c/mL", "<200 c/mL", "<50 c/mL")

# plot
ggplot(long, aes(x = cutoff, y = value, fill = progid))+
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Viral load threshold", 
       y = "Percent of PLWH on ART for 6 or more months \n virally suppressed",
       fill = "Program site") +
  scale_fill_brewer(palette="Accent") + 
  theme_minimal() +
  lims(y = c(0, 100))


## YOUTH COHORT DESCRIPTIVES -----

# summarize and plot enrollment #s by program site and HIV status
youth_summary <- data2 %>% 
  filter(visit==1 & youth==1) %>%
  group_by(progid, hivflag) %>% 
  summarise(n = n())

# create labels
youth_summary$label <- paste("n=", youth_summary$n, sep = "")

# plot
plot4 <- ggplot(youth_summary, aes(x = progid, y = n, fill = hivflag))+
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Program Site", 
       y = "Number enrolled",
       fill = "HIV Status") +
  geom_text(aes(label=label), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.2) +
  scale_fill_brewer(palette="Paired")

plot(plot4)

# descriptives for all youth
youth_v1 <- data2 %>% 
  filter(visit==1 & youth==1)

table <- summary(tableby(~ agev + gender + hivflag + progid,
                          data = youth_v1,
                          numeric.stats = c("Nmiss", "medianq1q3"),
                          numeric.test = "kwt",
                          digits = 1), text = NULL)

as.data.frame(table)

write.csv(table, "G:/DCAC/DCAC_PEPFAR/RV329/Analyses/Cohort profile/analysis files/table_youth.csv")

# descriptives for youth with HIV
youth_v1_hiv <- data2 %>% 
  filter(visit==1 & youth==1 & hivflag=="PLWH") %>%
  mutate(vs = case_when(is.na(dur_art) | dur_art<0.5 ~ 1,
                      dur_art>=0.5 & vls1000==1 ~ 2,
                      dur_art>=0.5 & vls1000==0 ~ 3,
                      .default = NA)) %>%
  mutate(vs = factor(vs,
                         levels = c(1,2,3),
                         labels = c("On ART <6 mo", "VL<1000 c/ml", "VL>=1000 c/ml")))

table <- summary(tableby(vertical ~ dur_hiv + age_dx + dur_art + cd4cat + 
                           anchor_drug + vs,
                         data = youth_v1_hiv,
                         numeric.stats = c("Nmiss", "medianq1q3"),
                         numeric.test = "kwt",
                         digits = 1), text = NULL)

as.data.frame(table)

write.csv(table, "G:/DCAC/DCAC_PEPFAR/RV329/Analyses/Cohort profile/analysis files/table_youth_plwh.csv")
