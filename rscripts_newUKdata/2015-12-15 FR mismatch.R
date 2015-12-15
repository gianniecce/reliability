
# Follows 2015-12-11 FR Missing analysis
# In this script 

library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(Rcpp)
library(Hmisc)
library(mtusRlocal)

load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple3Cleaned.RData')
sourceCpp('/Users/giacomovagni/Rprojects/reliability/cppScript/MismatchCpp.cpp')

#####
#####

# act   
timeFR2010_carnet_actprinc = timeFR2010_couple3 %>% select(idmen, idind, jour, matches('actpr')) %>% select(-matches('actprv'))
timeFR2010_carnet_actprincMelt = timeFR2010_carnet_actprinc %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_actprincMelt = timeFR2010_carnet_actprincMelt %>% arrange(idmen)

# presence  
timeFR2010_carnet_actpres = timeFR2010_couple3 %>% select(idmen, idind, jour, matches('pres')) %>% select(-matches('presv'))
timeFR2010_carnet_actpresMelt = timeFR2010_carnet_actpres %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_actpresMelt = timeFR2010_carnet_actpresMelt %>% arrange(idmen)

#####
#####

#########
# create dummies 
#########

timeFR2010_carnet_actpresMelt$alone = ifelse(grepl('1', x = timeFR2010_carnet_actpresMelt$value), 1, 0)
timeFR2010_carnet_actpresMelt$partner = ifelse(grepl('2', x = timeFR2010_carnet_actpresMelt$value), 1, 0)
timeFR2010_carnet_actpresMelt$parents = ifelse(grepl('3', x = timeFR2010_carnet_actpresMelt$value), 1, 0)
timeFR2010_carnet_actpresMelt$child = ifelse(grepl('4', x = timeFR2010_carnet_actpresMelt$value), 1, 0)
timeFR2010_carnet_actpresMelt$housemember = ifelse(grepl('5', x = timeFR2010_carnet_actpresMelt$value), 1, 0)
timeFR2010_carnet_actpresMelt$peers = ifelse(grepl('6', x = timeFR2010_carnet_actpresMelt$value), 1, 0)

#####
#####

timeFR2010_carnet_actpresMelt$presNA = ifelse(grepl(pattern = "     ", timeFR2010_carnet_actpresMelt$value), 1, 0) 
timeFR2010_carnet_actpresMelt = timeFR2010_carnet_actpresMelt %>% group_by(idind) %>% mutate(time = 1:n())

# DAY 
timeFR2010_carnet_actpresMelt_Day = timeFR2010_carnet_actpresMelt %>% filter(time >= 48 & time <= 132) 
timeFR2010_carnet_actpresMelt_Day 

# check summarise 
dtaMelt = timeFR2010_carnet_actpresMelt_Day %>% group_by() %>% select(idind, time, alone, partner, parents, child, housemember, peers, presNA)
dtaMelt %>% filter(idind == '011000821802') %>% as.data.frame()

dtaMelt = dtaMelt %>% melt(id.vars = c('idind', 'time'))
dtaMelt %>% head()
dtaMelt = dtaMelt %>% filter(value == 1)

#### time together individual - Sum

timeFR2010_couple4 = dtaMelt %>% group_by(idind, variable) %>% summarise(partnerTime = n() * 10) %>% 
  mutate(sumPresenceTime = sum(partnerTime)) %>% filter(variable == 'partner') %>% 
  merge(., timeFR2010_couple3, by = 'idind')

#### time children individual - Sum

timeFR2010_couple4 = dtaMelt %>% group_by(idind, variable) %>% summarise(childTime = n() * 10) %>% 
  mutate(sumPresenceTime2 = sum(childTime)) %>% filter(variable == 'child') %>% 
  merge(., timeFR2010_couple4, by = 'idind')

#################################################################
#################################################################
#################################################################

dtaMeltNet = timeFR2010_carnet_actpresMelt_Day %>% group_by() %>% select(idind, time, alone, partner, parents, child, housemember, peers, presNA)

dtaMeltNet = dtaMeltNet %>% group_by(idind, time) %>% mutate(family = ifelse(partner == 1  & child == 1, 1, 0) ) 
dtaMeltNet = dtaMeltNet %>% group_by(idind, time) %>% mutate(children = ifelse(partner == 0  & child == 1, 1, 0) ) 
dtaMeltNet = dtaMeltNet %>% group_by(idind, time) %>% mutate(partnerNoChildren = ifelse(partner == 1  & child == 0, 1, 0) ) 
dtaMeltNet 

dtaMeltNet = dtaMeltNet %>% group_by(idind, time) %>% 
  mutate(FamilyNetwork = ifelse(family == 1, 'family', 
                                ifelse(children == 1, 'children', 
                                       ifelse(partnerNoChildren == 1, 'partner', 
                                              ifelse(alone == 1, 'alone', 
                                                     ifelse(presNA == 1, 'MISSING', 'Other')))))) 


# 

dtaMeltNet2 = dtaMeltNet %>% select(idind, time, FamilyNetwork)
dtaMeltNet2 %>% head()

CoupleSequence = dtaMeltNet2 %>% spread(time, FamilyNetwork)
CoupleSequence %>% head()

library(TraMineR)
seqCouple = seqdef(CoupleSequence[,-1])

seqdplot(seqCouple, border = NA)

####
# summary 
####

dtaMeltNet3 = dtaMeltNet2 %>% group_by(idind, FamilyNetwork) %>% summarise(netTime = n() * 10) %>% spread(FamilyNetwork, netTime, fill = 0)
# good 
rowSums(dtaMeltNet3[,-1])

timeFR2010_couple4 = dtaMeltNet3 %>% 
  merge(., timeFR2010_couple4, by = 'idind')

# 
# 

# working ! 
timeFR2010_couple4 %>% group_by(sexe) %>% summarise(mean(family), sd(family), median(family))
timeFR2010_couple4 %>% group_by(sexe) %>% summarise(mean(children), sd(children), median(children))
timeFR2010_couple4 %>% group_by(sexe) %>% summarise(mean(alone), sd(alone), median(alone))

timeFR2010_couple4 %>% group_by(sexe_rec, dip14_rec) %>% summarise(mean(alone), sd(alone), median(alone))
timeFR2010_couple4 %>% group_by(sexe_rec, dip14_rec) %>% summarise(mean(family), sd(family), median(family))

timeFR2010_couple4 %>% group_by(sexe_rec, situacj_rec) %>% summarise(mean(alone), sd(alone), median(alone))
timeFR2010_couple4 %>% group_by(sexe_rec, situacj_rec) %>% summarise(mean(family), sd(family), median(family))

#  

timeFR2010_couple4 = timeFR2010_couple4 %>% tbl_df()
# 
timeFR2010_couple4 %>% group_by(sexe) %>% summarise(mean(partnerTime), sd(partnerTime), median(partnerTime))
timeFR2010_couple4 %>% group_by(revmen_rec) %>% summarise(mean(partnerTime), sd(partnerTime), median(partnerTime))
# 

# couple mean time partner 
timeFR2010_couple4 %>% group_by(idmen) %>% summarise(mean(partnerTime)) 
timeFR2010_couple4 %>% summarise(mean(partnerTime), min(partnerTime), max(partnerTime)) 

#
gg = 4
timeFR2010_couple4$partnerTime_rec = cut2(timeFR2010_couple4$partnerTime, g = gg) %>% factor(labels = paste(1:gg)) %>% as.character() %>% as.numeric()
#

timeFR2010_couple4$partnerTime_rec %>% table
glm( partnerTime_rec == 4 ~ age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen + I(revmen^2), data = timeFR2010_couple4, subset = sexe_rec == 'Men', family = binomial) %>% summary

##################################################################
##################################################################
# to continue here


####
timeFR2010_couple4$revmen_rec = timeFR2010_couple4$revmen_rec %>% as.factor()
#### 

lm( partnerTime ~ age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen + I(revmen^2), data = timeFR2010_couple4, subset = sexe_rec == 'Men') %>% summary
lm( partnerTime ~ age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen + I(revmen^2), data = timeFR2010_couple4, subset = sexe_rec == 'Women') %>% summary
#

# 
boxplot(timeFR2010_couple4$partnerTime ~ timeFR2010_couple4$revmen_rec)
boxplot(timeFR2010_couple4$partnerTime ~ timeFR2010_couple4$sexe)
boxplot(timeFR2010_couple4$partnerTime ~ timeFR2010_couple4$dip14_rec)
boxplot(timeFR2010_couple4$partnerTime ~ timeFR2010_couple4$situacj_rec)
boxplot(timeFR2010_couple4$partnerTime ~ timeFR2010_couple4$nenfants_rec)
# 

###############################################################################################
################################## Missing of Any presence #################################### 
###############################################################################################

# total of presence NA 
timeFR2010_carnet_actpresMelt_Day = timeFR2010_carnet_actpresMelt_Day %>% group_by(idind) %>% mutate(totalpresNA = sum(presNA))

# creating the partner presence - based on the NA - otherwise we have a 1 and 0. 
# partner_rec is the variable we will use for the analysis 
timeFR2010_carnet_actpresMelt_Day$partner_rec = ifelse(timeFR2010_carnet_actpresMelt_Day$presNA, NA, timeFR2010_carnet_actpresMelt_Day$partner)

# to spread for the sequences : 
timeFR_seq = timeFR2010_carnet_actpresMelt_Day %>% select(time, partner_rec, idind) %>% spread(time, partner_rec)

###################
library(TraMineR)
#### Looks fine 
timeFR_seq2 = seqdef(timeFR_seq[,-1], cpal = c('orange', 'lightblue'))
# 
quartz()
seqdplot( timeFR_seq2, with.missing = T)
seqiplot( timeFR_seq2, with.missing = T)
###################

# Individual missing 
timeFR2010_couple = timeFR2010_carnet_actpresMelt_Day %>% group_by(idind) %>% 
  summarise(totalindpresNA = sum(presNA))  %>% 
  merge(., timeFR2010_couple, by = 'idind') 

# Couple missing 
timeFR2010_couple = timeFR2010_carnet_actpresMelt_Day %>% group_by(idmen) %>% 
  summarise(totalcouplepresNA = sum(presNA)) %>% 
  merge(., timeFR2010_couple, by = 'idmen')

# check 
timeFR2010_couple[1:10, c('idmen', 'idind', 'totalcouplepresNA', 'totalindpresNA') ]

###########
###########






##### Mismatch 
#  
timeFR2010_carnet_III_actpresMis = timeFR2010_carnet_III_actpresMelt %>% 
  group_by(idmen, idind) %>% 
  mutate(epnum = 1:n()) %>%
  group_by(epnum) %>% 
  mutate(ep = 1:n()) %>%
  group_by(variable) %>% 
  mutate(mismatch = as.numeric (partner_rec[ep == 1] != partner_rec[ep == 2])) %>% 
  group_by() %>%
  mutate(max(epnum)) %>% 
  select(idmen, idind, ep, variable, epnum, alone, partner_rec, child, parents, housemember, peers,presNA, mismatch) 

head(timeFR2010_carnet_III_actpresMis) 
# 
timeFR2010_carnet_III_actpresMis$mismatch = ifelse(is.na(timeFR2010_carnet_III_actpresMis$mismatch), 'NA', timeFR2010_carnet_III_actpresMis$mismatch)
timeFR2010_carnet_III_actpresMis$partner_rec = ifelse(is.na(timeFR2010_carnet_III_actpresMis$partner_rec), 'NA', timeFR2010_carnet_III_actpresMis$partner_rec)
# 
MisPartnerProp = timeFR2010_carnet_III_actpresMis %>% group_by(variable, partner_rec) %>% summarise(n_mis = n()) %>% mutate(n_mis = n_mis / sum(n_mis))
# 
MisPartnerProp %>% ggplot(aes(x = variable, fill = factor(partner_rec), y = n_mis)) + 
  geom_bar(stat = "identity", width = 1, colour = "grey") + theme_minimal()
# 

############
###### quantile NA 
############

SumpresNA = timeFR2010_carnet_III_actpresMis %>% 
  group_by(idmen) %>% 
  summarise(SumpresNA = sum(presNA))

#
SumpresNA$quantileNA = cut2(SumpresNA$SumpresNA, g = 4)
SumpresNA$quantileNA = factor(SumpresNA$quantileNA, labels = c('0-25', '25-50', '50-75', '75-100'))
# 

# merge with original data 
timeFR2010_ind_II = merge(x = timeFR2010_ind, SumpresNA, by = 'idmen') 
timeFR2010_ind_II %>% summarise(mean(age), max(age))

cp( table(timeFR2010_ind_II$quantileNA, timeFR2010_ind_II$sexe), 2)
timeFR2010_ind_II %>% group_by(sexe, quantileNA) %>% summarise(mean(age))

timeFR2010_ind_II$age_rec = cut2(timeFR2010_ind_II$age, g = 3)
timeFR2010_ind_II$age_rec = factor(timeFR2010_ind_II$age_rec, labels = c('20-39', '40-53', '54-65'))

summary(glm(timeFR2010_ind_II$quantileNA == '75-100' ~  age_rec + sexe + etamatri, data = timeFR2010_ind_II, family = binomial))

timeFR2010_ind_II$etamatri %>% table
timeFR2010_ind_II$COUPLRP %>% table

# 
tus = merge(timeFR2010_carnet_III_actpresMis, SumpresNA, by = 'idmen')
head(tus)




# 
MisPartnerProp = tus %>% group_by(variable, partner_rec, quantileNA) %>% summarise(n_mis = n()) %>% mutate(n_mis = n_mis / sum(n_mis)) 
MisPartnerProp %>% group_by(variable, quantileNA) %>% mutate(sum(n_mis))
# 

MisPartnerProp %>% ggplot(aes(x = variable, fill = factor(partner_rec), y = n_mis)) + 
  geom_bar(stat = "identity", width = 1, colour = "grey") + theme_minimal() + facet_grid(.~ quantileNA)

# 

########################################
########################################

# 1. Missing reported presence / Number and Timing 
# Number of missing - what kind of couple 

# 2. Activities during missing 
# 

# 3. Mismatch / Number and Timing and activities during mismatch 



# 
ggplot(tus, aes(x = variable, fill = factor(value_rec), y = prop)) + 
  geom_bar(stat = "identity", width = 1, colour = "grey") + theme_minimal()
# 

# 
tus %>%
  ggplot(aes(x=variable,y=idind,fill=value)) +
  geom_tile() + theme_minimal()

