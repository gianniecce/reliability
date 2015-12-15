
# In this script we are comparing the missing with the day and night 
# we come to the conclusion that there was a lots of missing during the night so we focused on the day solely 

library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(Rcpp)
library(Hmisc)
library(mtusRlocal)

load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple2Cleaned.RData')

sourceCpp('/Users/giacomovagni/Rprojects/reliability/cppScript/MismatchCpp.cpp')

#####
#####

timeFR2010_couple %>% group_by() %>% distinct(idind) %>% summarise(n())
timeFR2010_couple %>% group_by() %>% distinct(idmen) %>% summarise(n())
timeFR2010_couple %>% group_by() %>% distinct(idind, jour) %>% summarise(n())

timeFR2010_couple %>% group_by(jour) %>% summarise(nd = n()) %>% mutate(nd / sum(nd)) %>% mutate(sum(nd)) 

#####
#####

timeFR2010_carnet_III_actpresMelt %>% distinct(idind) %>% summarise(n())

# 1. you need to check if the presNA people are different from the other because you would need to remove them 

# Count the blank space as 1 -> missing presence  
timeFR2010_carnet_III_actpresMelt$presNA = ifelse(grepl(pattern = "     ", timeFR2010_carnet_III_actpresMelt$value), 1, 0) 
head(timeFR2010_carnet_III_actpresMelt) 
# 

# Total sum of Missing presence 
timeFR2010_carnet_III_actpresMelt = timeFR2010_carnet_III_actpresMelt %>% group_by(idind) %>% mutate(totalpresNA = sum(presNA))
timeFR2010_carnet_III_actpresMelt$totalpresNA

# partner variable that includes Missings ! 
timeFR2010_carnet_III_actpresMelt$partner_rec = ifelse(timeFR2010_carnet_III_actpresMelt$presNA, NA, timeFR2010_carnet_III_actpresMelt$partner)

################################################
# Check if it is correct on one couple 
test1 = timeFR2010_carnet_III_actpresMelt %>% subset(idmen == timeFR2010_carnet_III_actpresMelt$idmen[1]) %>% select(variable, value, idind) %>% spread(variable, value)
test2 = timeFR2010_carnet_III_actpresMelt %>% subset(idmen == timeFR2010_carnet_III_actpresMelt$idmen[1]) %>% select(variable, partner, idind) %>% spread(variable, partner)

test1 %>% as.data.frame()
test2 %>% as.data.frame()

test3 = timeFR2010_carnet_III_actpresMelt %>% subset(idmen == timeFR2010_carnet_III_actpresMelt$idmen[1])
################################################

#### Compare day and night missing presence 
# count missing presence for 1/3
timeFR2010_carnet_III_actpresMelt$missing_presence = ifelse(timeFR2010_carnet_III_actpresMelt$totalpresNA > 48, 1, 0)
# what happens if we restrict to the daylight 8-22
timeFR2010_carnet_III_actpresMelt = timeFR2010_carnet_III_actpresMelt %>% group_by(idind) %>% mutate(time = 1:144)
# DAY 
timeFR2010_carnet_III_actpresMelt_Day = timeFR2010_carnet_III_actpresMelt %>% filter(time >= 48 & time <= 132) 
# 84 / 3 = 28 
timeFR2010_carnet_III_actpresMelt_Day = timeFR2010_carnet_III_actpresMelt_Day %>% group_by(idind) %>% mutate(totalpresNA_DAY = sum(presNA))

# mean time comparison 
timeFR2010_carnet_III_actpresMelt_Day %>% group_by() %>% summarise(mean(totalpresNA), sd(totalpresNA), median(totalpresNA))
timeFR2010_carnet_III_actpresMelt_Day %>% group_by() %>% summarise(mean(totalpresNA_DAY), sd(totalpresNA_DAY), median(totalpresNA_DAY))
# so obviously it is worth keeping the day light 
timeFR2010_carnet_III_actpresMelt_Day$missing_presenceDAY = ifelse(timeFR2010_carnet_III_actpresMelt_Day$totalpresNA > 28, 1, 0)









