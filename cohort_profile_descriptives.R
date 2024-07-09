######################################################################
# PROGRAM NAME: 		   Cohort profile descriptives     
# AUTHOR:              Nicole Dear
# DATE WRITTEN:        22Jan2024
# REVIEWED BY:		
# DATE REVIEWED:		
# WRITTEN FOR:         AFRICOS
# PURPOSE:             
# OVERVIEW:                                                       
# INPUT DATA:          merged data from Fauci exported as excel file  
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
# install.packages(c("readxl", "ggplot2", "dplyr", "tidyr", "expss", "ggpubr", "stringr", "tidyverse", "lubridate",
#                   "table1", "kableExtra", "lmtest", "sandwich", "k5", "binom", "glm2"))

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(stringr)
library(tidyverse)
library(lubridate)
library(table1)
library(kableExtra)
library(data.table)

# set working directory
setwd("G:/DCAC/DCAC_PEPFAR/RV329/Analyses/Cohort profile/analysis files")

# load data (data from Fauci exported as excel)
data <- read_excel("africos_1dec2023.xlsx")
names(data) <- tolower(names(data))
names(data)

# enrollment by site
addmargins(table(data$visit, data$progid))


## DATA CLEANING -----

# impute missing data
a <- data %>%
  dplyr::group_by(subjid) %>%
  fill(c(hivflag, gender, diagdtn, art_sdtn, progid, dobdtn), .direction = "downup") %>%
  dplyr::ungroup()

# fill in missing agev using calculated age
i <- is.na(a$agev)
a$agev[i] <- with(a, (a$visitdt[i] - a$dobdtn[i]) / 365.25)

# fill in duration since HIV dx and duration on ART
j <- is.na(a$dur_hiv)
a$dur_hiv[j] <- difftime(a$visitdt[j], a$diagdtn[j], units = "days") / 365.25

k <- is.na(a$dur_art)
a$dur_art[k] <- difftime(a$visitdt[k], a$art_sdtn[k], units = "days") / 365.25

# fill in missing hivflag using hivstat
a$hivstat_ <- ifelse(a$hivstat==1,1,2)
m <- is.na(a$hivflag)
a$hivflag[m] <- with(a, a$hivstat_[m])

# create new variables
a$year <- format(as.Date(data$visitdt, format="%d/%m/%Y"),"%Y")

# create new categorical variables
a$country <- NA
a$country[a$progid==1] <- 1
a$country[a$progid==2|a$progid==3] <- 2
a$country[a$progid==4] <- 3
a$country[a$progid==5] <- 4

a$agecat <- ifelse(a$agev<30,0,
                   ifelse(a$agev>=30 & a$agev<40,1,
                          ifelse(a$agev>=40 & a$agev<50,2,3)))

a$youth <- ifelse(a$agev<25,1,0)

a$cd4cat <- ifelse(a$cd3_4_n<200,0,
                   ifelse(a$cd3_4_n>=200 & a$cd3_4_n <350,1,
                          ifelse(a$cd3_4_n>=350 & a$cd3_4_n<500,2,3)))

a$vs <- ifelse(a$vl<1000,1,0)

# label factor variables
a$progid <- factor(a$progid,
                   levels = c(1,2,3,4,5),
                   labels = c("Kayunga, Uganda", "South Rift Valley, Kenya", "Kisumu West, Kenya", 
                              "Mbeya, Tanzania", "Abuja & Lagos Nigeria"))

a$country <- factor(a$country,
                    levels = c(1,2,3,4),
                    labels = c("Uganda", "Kenya", "Tanzania", "Nigeria"))

a$gender <- factor(a$gender,
                   levels = c(1,2),
                   labels = c("Male", "Female"))

a$hivflag <- factor(a$hivflag,
                    levels = c(1,2),
                    labels = c("PLWH", "PLWoH"))

a$agecat <- factor(a$agecat,
                   levels = c(0,1,2,3),
                   labels = c("<30", "30-39", "40-49", "50+"))

a$youth <- factor(a$youth,
                   levels = c(0,1),
                   labels = c("25+ years", "15-24 years"))

a$cd4cat <- factor(a$cd4cat,
                   levels = c(0,1,2,3),
                   labels = c("<200", "200-349", "350-499", "500+"))

a$vs <- factor(a$vs,
               levels = c(0,1),
               labels = c(">=1000", "<1000"))


# checking for missing values at enrollment
v1 <- a %>% filter(visit==1)
test <- v1[is.na(v1$hivflag), ]

# keep enrollment visit only & drop participant C01-0083 without HIV status, VL etc.
v1 <- a %>% filter(visit==1) %>% filter(subjid != "C01-0083")


## DATA VIZ -----

# summarize and plot age by enrollment year
age_summary <- v1 %>%
  group_by(year, progid) %>%
  summarize(med = median(agev),
            Q1 = quantile(agev, 0.25),
            Q3 = quantile(agev, 0.75))

ggplot(age_summary, aes(x = year, y = med, group = progid)) +
  geom_line(aes(color = progid)) +
  geom_point(aes(color = progid)) +
  labs(x = "Enrolment year", 
       y = "Median age (years) at enrolment") +
  guides(color = guide_legend(title = "Study site")) + 
  theme_pubr() + theme(legend.position = c(0.2, 0.2))

# % youth
addmargins(table(v1$youth))
100*(959/4136)


# summarize and plot % female by enrollment year
v1$female <- ifelse(v1$gender=="Female",1,0)

gender_summary <- v1 %>%
  group_by(year) %>% 
  summarise(tot_female = sum(female), 
            n = n(),
            percent_female = 100*(sum(female)/n()))

# create labels
gender_summary$label <- paste(gender_summary$tot_female, " (", 
                              round(gender_summary$percent_female, digits = 1), "%)", sep = "")

# plot
ggplot(gender_summary, aes(x = year, y = percent_female)) +
  geom_bar(fill = "grey", stat = "identity") +
  geom_text(aes(label = label), vjust = -0.4, size = 3.5) +
  theme_classic2() +
  xlab("Enrolment year") +
  ylab("Percent female") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  lims(y = c(0, 100))


# summarize and plot viral suppression at enrollment visit
v1v <- v1 %>% filter(hivflag=="PLWH" & dur_art>=0.5)

# drop records where vl is missing
miss <- is.na(v1v$vl)
v <- subset(v1v, subset = !miss)

# summarize viral suppression by site
v$vs <- ifelse(v$vl<1000,1,0)
v$vs200 <- ifelse(v$vl<200,1,0)
v$vs50 <- ifelse(v$vl<50,1,0)

vs_summary <- v %>%
  group_by(progid) %>%
  summarise(tot_vs = sum(vs),
            tot_vs200 = sum(vs200),
            tot_vs50 = sum(vs50),
            n = n(),
            percent_vs = 100*(sum(vs)/n()),
            percent_vs200 = 100*(sum(vs200)/n()),
            percent_vs50 = 100*(sum(vs50)/n()))

# now reshape wide to long to plot
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


# summarize and plot viral suppression at most recent visit
plwh <- a %>% filter(subjid != "C01-0083" & hivflag=="PLWH" & dur_art>=0.5)

# drop records where vl is missing
miss <- is.na(plwh$vl)
plwh_nomiss <- subset(plwh, subset = !miss)

# take most recent viral load
v_last <- plwh_nomiss %>%
  arrange(subjid, visitdt) %>% 
  group_by(subjid) %>% 
  summarise_all(last)

# summarize viral suppression by site
v_last$vs <- ifelse(v_last$vl<1000,1,0)
v_last$vs200 <- ifelse(v_last$vl<200,1,0)
v_last$vs50 <- ifelse(v_last$vl<50,1,0)

vs_summary2 <- v_last %>%
  group_by(progid) %>%
  summarise(tot_vs = sum(vs),
            tot_vs200 = sum(vs200),
            tot_vs50 = sum(vs50),
            n = n(),
            percent_vs = 100*(sum(vs)/n()),
            percent_vs200 = 100*(sum(vs200)/n()),
            percent_vs50 = 100*(sum(vs50)/n()))

# now reshape wide to long to plot
wide <- vs_summary2[,c(1,6:8)]
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







# png(file="RV466_PathPos_Nigeria.png", width=800, height=600)
# bar
# dev.off()

