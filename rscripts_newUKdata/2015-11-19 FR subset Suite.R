
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

load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_coupleCleaned.RData')
sourceCpp('/Users/giacomovagni/Rprojects/reliability/cppScript/MismatchCpp.cpp')

#####
#####

###
# Situation 
###
# Je serais pour oter de l'echantiller les étudiants, handicapés ou apprentis 
timeFR2010_couple %>% group_by(situacj) %>% summarise(n())
# 
timeFR2010_couple = timeFR2010_couple %>% filter(situacj == 1 | situacj == 4 | situacj == 5 | situacj == 6) 
# 
timeFR2010_couple$situacj_rec[timeFR2010_couple$situacj == '1'] <- 'Employed'
timeFR2010_couple$situacj_rec[timeFR2010_couple$situacj == '4'] <- 'Unemployed'
timeFR2010_couple$situacj_rec[timeFR2010_couple$situacj == '5'] <- 'Retired'
timeFR2010_couple$situacj_rec[timeFR2010_couple$situacj == '6'] <- 'Home'
# 
timeFR2010_couple %>% group_by(situacj_rec) %>% summarise(n())
# 

timeFR2010_couple %>% group_by(cs24cj) %>% summarise(n = n()) %>% mutate(p = n / sum(n)) %>% as.data.frame()

timeFR2010_couple %>% group_by(cs24cj) %>% summarise(n = n()) %>% mutate(p = n / sum(n)) %>% ggplot(aes(x=cs24cj, y = p, fill=cs24cj)) + 
  geom_bar(stat="identity") 

###
# Nbr enfants dans le ménage  
### 

timeFR2010_couple %>% group_by(situacj_rec) %>% summarise(mean(nenfants))
timeFR2010_couple %>% group_by(typmen15_rec) %>% summarise(mean(nenfants), min(nenfants), max(nenfants), median(nenfants))

max( timeFR2010_couple$age )
min( timeFR2010_couple$age )

timeFR2010_couple %>% group_by(natio7pr) %>% summarise(n())
timeFR2010_couple %>% group_by(typmen15_rec) %>% summarise(n())

# 
timeFR2010_couple %>% group_by(situacj_rec) %>% summarise(mean(age))
timeFR2010_couple %>% group_by(nactifs) %>% summarise(mean(age))

timeFR2010_couple %>% group_by(situacj_rec) %>% summarise(mean(surface, na.rm = T))
timeFR2010_couple %>% group_by(situacj_rec) %>% summarise(median(npieces, na.rm = T))

# zone sensible 
timeFR2010_couple %>% group_by(situacj_rec, lzus) %>% summarise(n())
# Zonage en aires urbaines (codage 2011)
timeFR2010_couple %>% group_by(zau, situacj_rec) %>% summarise(n())
# situation and typology 
timeFR2010_couple %>% group_by(situacj_rec, typlog) %>% summarise(n())

# Existence de personnes handicapées dans le ménage 
# I think we should remove also this population 
timeFR2010_couple %>% group_by(handic1e) %>% summarise(n())
# 2 == non 
timeFR2010_couple = timeFR2010_couple %>% filter(handic1e == 2) 
# 

#######
timeFR2010_couple = timeFR2010_couple %>% mutate(nbpersenfantWrong = ifelse(npers == 2 & nenfants == 3, 1, 0)) %>% filter(nbpersenfantWrong == 0)
#######

###### ###### ###### 
###### Subset both partner in couple 
###### ###### ###### 

timeFR2010_couple %>% group_by(sexe) %>% summarise(n())
timeFR2010_couple = timeFR2010_couple %>% group_by(idmen) %>% mutate(npartnerdiary = n()) %>% mutate(nsexe = sum(sexe)) %>% arrange(idmen) %>% group_by()

table(timeFR2010_couple$nsexe, timeFR2010_couple$npartnerdiary)
timeFR2010_couple = timeFR2010_couple %>% filter(npartnerdiary == 2)

# sexe parity
timeFR2010_couple %>% group_by(sexe) %>% summarise(n())
# 

######## 
# Nationalité - rec 
######## 
timeFR2010_couple$natio7_rec = as.character(timeFR2010_couple$natio7)

timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '1'] <- 'FR'
timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '2'] <- 'FR'
timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '3'] <- 'UE'
timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '4'] <- 'UE'
timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '5'] <- 'OTHER'
timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '6'] <- 'OTHER' 
timeFR2010_couple$natio7_rec[timeFR2010_couple$natio7 == '7'] <- 'OTHER' 
#####

######## 
# Nationalité - sexe 
######## 
timeFR2010_couple$sexe_rec = factor(timeFR2010_couple$sexe, labels = c('Men', 'Women'))
#####

######## 
# Nationalité - nenfants  
######## 
timeFR2010_couple$nenfants_rec = ifelse(timeFR2010_couple$nenfants == 0, '0', 
                                    ifelse(timeFR2010_couple$nenfants == 1, '1', 
                                           ifelse(timeFR2010_couple$nenfants == 2, '2', ifelse(
                                             timeFR2010_couple$nenfants == 3, '3', '4')))) 
#####

timeFR2010_couple %>% count(nenfants_rec)
timeFR2010_couple %>% count(npers, nenfants_rec)

timeFR2010_couple[1:10, 1:20]

timeFR2010_couple %>% count(natio7pr, natio7pr_rec)

######## 
# Nationalité - rec 
######## 
timeFR2010_couple$dip14_rec = as.character(timeFR2010_couple$dip14)

timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '10'] <- 'PhD-GrandeEcole'
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '12'] <- 'PhD-GrandeEcole'
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '20'] <- 'Uni'
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '30'] <- 'Uni'
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '31'] <- 'Uni'
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '33'] <- 'Uni' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '41'] <- 'Uni' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '42'] <- 'Primary-Secondary' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '43'] <- 'Primary-Secondary' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '44'] <- 'Primary-Secondary' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '50'] <- 'Primary-Secondary' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '60'] <- 'Primary-Secondary' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '70'] <- 'Primary-Secondary' 
timeFR2010_couple$dip14_rec[timeFR2010_couple$dip14 == '71'] <- 'No Diploma' 

timeFR2010_couple$dip14_rec = factor(timeFR2010_couple$dip14_rec, levels = c('No Diploma', 'Primary-Secondary', 'PhD-GrandeEcole', 'Uni'))
#####

# mean age by couple 
timeFR2010_couple %>% group_by(idmen) %>% summarise(mean(age))

# 
# sexe 1 = Men 
# sexe 2 = Women  
timeFR2010_couple = timeFR2010_couple %>% group_by(idmen) %>% mutate(agediff = age[sexe == 1] - age[sexe == 2])
# 
timeFR2010_couple$agediff %>% mean()
timeFR2010_couple$agediff %>% summary()

#####
#####

# save(timeFR2010_couple, file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple2Cleaned.RData')
load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple2Cleaned.RData')

#####
#####

# act   
timeFR2010_carnet_actprinc = timeFR2010_couple %>% select(idmen, idind, jour, matches('actpr')) %>% select(-matches('actprv'))
timeFR2010_carnet_actprincMelt  = timeFR2010_carnet_actprinc %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_actprincMelt = timeFR2010_carnet_actprincMelt %>% arrange(idmen)

# presence  
timeFR2010_carnet_actpres = timeFR2010_couple %>% select(idmen, idind, jour, matches('pres')) %>% select(-matches('presv'))
timeFR2010_carnet_actpresMelt  = timeFR2010_carnet_actpres %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_actpresMelt = timeFR2010_carnet_actpresMelt %>% arrange(idmen)

#####
#####

# Il faut aussi s'assurer que les deux partners aient repondu au time use ! 



