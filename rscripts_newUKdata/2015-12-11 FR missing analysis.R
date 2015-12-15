
# Follows 2015-11-20 FR Missing Night
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

load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple2Cleaned.RData')
sourceCpp('/Users/giacomovagni/Rprojects/reliability/cppScript/MismatchCpp.cpp')

#####
#####

timeFR2010_couple$revmen %>% hist(breaks = 100) # revenu du ménage 
timeFR2010_couple$revmen %>% log %>% hist(breaks = 100) # revenu du ménage 

timeFR2010_couple$revenu_men_cor %>% hist(breaks = 100)

timeFR2010_couple$nqui # Nombre de questionnaire individu par ménage
timeFR2010_couple$ncar # Nombre de carnets journaliers par ménage
timeFR2010_couple$nsem

max( timeFR2010_couple$age )
max( timeFR2010_couple$agediff )

timeFR2010_couple$revmen_rec = cut2(timeFR2010_couple$revmen, g = 5)
timeFR2010_couple$revmen_rec = factor(timeFR2010_couple$revmen_rec, labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5'))
timeFR2010_couple$revmen_rec %>% table

timeFR2010_couple$revmen_rec = as.character(timeFR2010_couple$revmen_rec)
timeFR2010_couple$revmen_rec[is.na(timeFR2010_couple$revmen_rec)] = 'NA'
timeFR2010_couple$revmen_rec %>% table()

#####
#####

# act   
timeFR2010_carnet_actprinc = timeFR2010_couple %>% select(idmen, idind, jour, matches('actpr')) %>% select(-matches('actprv'))
timeFR2010_carnet_actprincMelt = timeFR2010_carnet_actprinc %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_actprincMelt = timeFR2010_carnet_actprincMelt %>% arrange(idmen)

# presence  
timeFR2010_carnet_actpres = timeFR2010_couple %>% select(idmen, idind, jour, matches('pres')) %>% select(-matches('presv'))
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
timeFR2010_carnet_actpresMelt = timeFR2010_carnet_actpresMelt %>% group_by(idind) %>% mutate(time = 1:144)

# DAY 
timeFR2010_carnet_actpresMelt_Day = timeFR2010_carnet_actpresMelt %>% filter(time >= 48 & time <= 132) 

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

# recoding 
timeFR2010_couple$totalindpresNA_cut = cut2(timeFR2010_couple$totalcouplepresNA, g = 11)
timeFR2010_couple$totalindpresNA_cutnum = factor(timeFR2010_couple$totalindpresNA_cut, labels = c(paste(0:8))) %>% as.character() %>% as.numeric()

# error here
timeFR2010_couple$totalcouplepresNA_cut = cut2(timeFR2010_couple$totalcouplepresNA, g = 11)

timeFR2010_couple$totalcouplepresNA_cut = factor(timeFR2010_couple$totalcouplepresNA_cut, labels = c(paste('Q', 1:length(levels(timeFR2010_couple$totalcouplepresNA_cut)), sep = '')))
# illusion of normality 
timeFR2010_couple$totalcouplepresNA_cutnum = factor(timeFR2010_couple$totalcouplepresNA_cut, labels = c(paste(1:length(levels(timeFR2010_couple$totalcouplepresNA_cut)), sep = ''))) %>% as.character() %>% as.numeric()

table(timeFR2010_couple$totalcouplepresNA_cut, timeFR2010_couple$age6) %>% cp(2)
table(timeFR2010_couple$totalcouplepresNA_cut, timeFR2010_couple$nenfants_rec) %>% cp(2)

# All presence missing 
timeFR2010_couple$presencediarymissing = ifelse(timeFR2010_couple$totalindpresNA == '85', 1, 0)
# 

######
# 3 regressions on INDIVIDUAL missing 
######
# lm 
lm( totalindpresNA ~ sexe_rec + age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple) %>% summary
lm( totalindpresNA_cutnum ~ sexe_rec + age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple) %>% summary
lm( totalindpresNA_cutnum ~ sexe_rec + age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple) %>% summary

# glm - only 31 cases 
glm( presencediarymissing ~ age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple, family = 'binomial', subset = sexe == 1) %>% summary

# predicting 0 
glm( totalindpresNA_cut == " 0" ~ sexe_rec + age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple, family = 'binomial') %>% summary
glm( totalindpresNA_cut == "[29,170]" ~ sexe_rec + age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple, family = 'binomial') %>% summary

# men 
glm( totalindpresNA_cut == " 0" ~ age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple, subset = sexe_rec == 'Men', family = 'binomial') %>% summary
# women
glm( totalindpresNA_cut == " 0" ~ age6 + nenfants_rec + situacj_rec + dip14_rec + natio7_rec + revmen_rec, data = timeFR2010_couple, subset = sexe_rec == 'Women', family = 'binomial') %>% summary

timeFR2010_couple = timeFR2010_couple %>% group_by()

# reducing the mean missing by cutting on Q9 and Q8
dta = timeFR2010_couple[ timeFR2010_couple$totalcouplepresNA_cut != 'Q9' & timeFR2010_couple$totalcouplepresNA_cut != 'Q8', ] 

timeFR2010_couple %>% group_by() %>% summarise(mean(totalindpresNA))
dta %>% group_by() %>% summarise(mean(totalindpresNA))

dta %>% group_by(revmen_rec) %>% summarise(mean(totalindpresNA))
timeFR2010_couple %>% group_by(revmen_rec) %>% summarise(mean(totalindpresNA), mean(age), median(nenfants))

table(timeFR2010_couple$revmen_rec == 'NA', timeFR2010_couple$dip14_rec) %>% cp(2)
table(timeFR2010_couple$revmen_rec == 'NA', timeFR2010_couple$situacj_rec) %>% cp(2) 

# timeFR2010_couple$revmen_rec = timeFR2010_couple$revmen_rec %>% as.character()
# timeFR2010_couple$revmen_rec[is.na(timeFR2010_couple$revmen_rec)] <- 'NA'
# timeFR2010_couple$revmen_rec %>% table()

# max missing sequence 
28 / 84 

################################ 
################################ 

timeFR2010_couple = timeFR2010_couple[ timeFR2010_couple$totalcouplepresNA_cut != 'Q9' & timeFR2010_couple$totalcouplepresNA_cut != 'Q8', ] 
timeFR2010_couple %>% group_by(revmen_rec) %>% summarise(mean(totalindpresNA), sd(totalindpresNA))

# summaries 
timeFR2010_couple %>% group_by(sexe_rec) %>% summarise(mean(totalindpresNA))
timeFR2010_couple %>% group_by() %>% summarise(mean(totalindpresNA), max(totalindpresNA))
# 
timeFR2010_couple = timeFR2010_couple %>% group_by(idmen) %>% mutate(meanagecouple = mean(age))
# 
timeFR2010_couple %>% group_by(sexe) %>% summarise(mean(totalindpresNA))

# create value labels 
y = timeFR2010_couple$totalindpresNA
x <- timeFR2010_couple$revmen_rec %>% as.factor()

# plot densities 
sm.density.compare(y, x, xlab="Missing Time")
title(main="NA Distribution by Incomes")

# add legend via mouse click
colfill<-c(2:(2+length(levels(x)))) 
legend(locator(1), levels(x), fill=colfill)

##########
timeFR2010_couple %>% group_by(sexe) %>% summarise(mean(totalindpresNA), median(totalindpresNA), max(totalindpresNA))
##########

# max missing 21% 
18 / 84

timeFR2010_couple3 = timeFR2010_couple
# save(timeFR2010_couple3, file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple3Cleaned.RData')
load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple3Cleaned.RData')



