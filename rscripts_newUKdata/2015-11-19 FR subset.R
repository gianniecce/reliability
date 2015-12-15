
# In this script we are selection our analylitical subsample  

library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

getwd()

timeFR2010_carnet = read.spss(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/TimeUseFR2010/SPSS/carnet.sav', to.data.frame = T)
timeFR2010_ind = read.spss(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/TimeUseFR2010/SPSS/individu.sav', to.data.frame = T)
timeFR2010_menages = read.spss(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/TimeUseFR2010/SPSS/menage.sav', to.data.frame = T)

#
names(timeFR2010_menages) = tolower(names(timeFR2010_menages))
#

# checking the carnet 
timeFR2010_carnet %>% distinct(idmen) %>% summarise(n())
timeFR2010_carnet %>% distinct(idind) %>% summarise(n())

# number of diaries by id couple
timeFR2010_carnet %>% group_by(idmen) %>% summarise(n())
# number of diaries by individual 
timeFR2010_carnet %>% group_by(idind) %>% summarise(n())

# 
timeFR2010_carnet %>% group_by(idind) %>% summarise(ndiary= n()) %>% group_by(ndiary) %>% summarise(n())

# days and id couple individual 
timeFR2010_carnet %>% group_by(idmen, idind, jour) %>% summarise(n()) 

# distribution of days  
timeFR2010_carnet %>% group_by(idmen, idind, jour) %>% summarise(ndiary = n()) %>% group_by(jour) %>% summarise(nd = n()) %>% mutate(nd / sum(nd)) %>% mutate(sum(nd)) 

# days should be - id couple et le meme jour 
timeFR2010_carnet_II = timeFR2010_carnet %>% group_by(idmen, jour) %>% mutate(daysN = n()) %>% filter(daysN == 2)

# check 
timeFR2010_carnet_II %>% group_by(idind, jour) %>% summarise(n())
timeFR2010_carnet_II %>% group_by(jour) %>% summarise(nd = n()) %>% mutate(nd / sum(nd)) %>% mutate(sum(nd)) 

timeFR2010_carnet_II %>% group_by() %>% distinct(idind) %>% summarise(n())
timeFR2010_carnet_II %>% group_by() %>% distinct(idmen) %>% summarise(n())

timeFR2010_carnet_II %>% group_by() %>% distinct(idind, jour) %>% summarise(n())

timeFR2010_carnet_II$weekend = ifelse(timeFR2010_carnet_II$jour ==  "samedi  " | timeFR2010_carnet_II$jour == 'dimanche', 1, 0) 
timeFR2010_carnet_II$weekdays = ifelse(timeFR2010_carnet_II$jour == 'dimanche' | timeFR2010_carnet_II$jour == "samedi  ", 0, 1) 

timeFR2010_carnet_II %>% group_by(weekend) %>% summarise(n())

##########
# sample days by couple and days # only 1 diaries 
##########
# timeFR2010_carnet_III = timeFR2010_carnet_II %>% group_by(idmen) %>% filter(jour == jour[sample(length(jour),1)]) %>% group_by()
timeFR2010_carnet_III = timeFR2010_carnet_II %>% group_by(idmen) %>% filter(weekend == 1)

timeFR2010_carnet_III %>% group_by(idind, jour) %>% summarise(n())
timeFR2010_carnet_III %>% group_by(jour) %>% summarise()
#  

timeFR2010_carnet_III %>% group_by() %>% distinct(idind) %>% summarise(n())
timeFR2010_carnet_III %>% group_by() %>% distinct(idmen) %>% summarise(n())
timeFR2010_carnet_III %>% group_by() %>% distinct(idind, jour) %>% summarise(n())

timeFR2010_carnet_III %>% group_by(jour) %>% summarise(nd = n()) %>% mutate(nd / sum(nd)) %>% mutate(sum(nd)) 

##########
##########

timeFR2010 = merge(timeFR2010_ind, timeFR2010_carnet_III, by = c('idind', 'idmen')) 
timeFR2010 = merge(timeFR2010, timeFR2010_menages, by = c('idmen')) 

##########
##########

timeFR2010_couple = timeFR2010 %>% group_by(idmen) %>% mutate(ncouple = n()) %>% filter(ncouple == 2)
timeFR2010_couple = timeFR2010_couple %>% filter(typmen5 != 5)

table(timeFR2010_couple$typmen15) 
nrow(timeFR2010_couple) 

timeFR2010_couple %>% distinct(idmen) %>% summarise(n())

timeFR2010_couple$typmen15_rec = factor(timeFR2010_couple$typmen15, 
       labels = c('Couple sans enfant, un actif', 
                  'Couple sans enfant, deux actifs', 
                  'Couple sans enfant, tous inactifs', 
                  'Couple avec enfant, un membre du couple actif', 
                  'Couple avec enfant, deux membres du couple actifs', 
                  'Couple avec enfant, couple inactif et au moins un enfant actif', 
                  'Couple avec enfant, tous inactifs'))


table(timeFR2010_couple$typmen15, timeFR2010_couple$typmen15_rec)  

##########
########## 
##########

table(timeFR2010_couple$jour) 
timeFR2010_couple %>% group_by(jour) %>% summarise(n()) 

timeFR2010_couple %>% group_by(sexe, jour) %>% summarise(n())

# gay 
timeFR2010_couple = timeFR2010_couple %>% group_by(idmen) %>% mutate(n = sum(sexe)) %>%  mutate( gay = ifelse(n == 3, 0, 1) )
timeFR2010_couple = timeFR2010_couple %>% filter(gay == 0)
timeFR2010_couple 

timeFR2010_couple %>% group_by(sexe, jour) %>% summarise(n())
timeFR2010_couple %>% group_by(sexe) %>% summarise(mean(age), max(age), min(age))

timeFR2010_couple %>% group_by(sexe, nenfants) %>% summarise(n())
timeFR2010_couple %>% group_by(sexe, typmen15_rec) %>% summarise(n())

timeFR2010_couple %>% group_by(typmen15_rec) %>% summarise(min(age), max(age), median(age))
timeFR2010_couple = timeFR2010_couple %>% filter(age < 66) 

# summarise age 
timeFR2010_couple %>% group_by(typmen15_rec) %>% summarise(min(age), max(age), median(age))

# same day 
check = timeFR2010_couple %>% group_by(idmen, jour) %>% summarise(n = n()) %>% mutate( nif = ifelse(n == 2, 0, 1) )
table(check$nif)

max(timeFR2010_couple$age)

# save(timeFR2010_couple, file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_coupleCleaned.RData')
load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_coupleCleaned.RData')

########## 
##########

# act   
timeFR2010_carnet_III_actprinc = timeFR2010_carnet_III %>% select(idmen, idind, jour, matches('actpr')) %>% select(-matches('actprv'))
timeFR2010_carnet_III_actprincMelt  = timeFR2010_carnet_III_actprinc %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_III_actprincMelt = timeFR2010_carnet_III_actprincMelt %>% arrange(idmen)

# presence  
timeFR2010_carnet_III_actpres = timeFR2010_carnet_III %>% select(idmen, idind, jour, matches('pres')) %>% select(-matches('presv'))
timeFR2010_carnet_III_actpresMelt  = timeFR2010_carnet_III_actpres %>% melt(id.vars = c('idmen', 'idind', 'jour')) 
timeFR2010_carnet_III_actpresMelt = timeFR2010_carnet_III_actpresMelt %>% arrange(idmen)

#########
# create dummies 
#########

timeFR2010_carnet_III_actpresMelt$alone = ifelse(grepl('1', x = timeFR2010_carnet_III_actpresMelt$value), 1, 0)
timeFR2010_carnet_III_actpresMelt$partner = ifelse(grepl('2', x = timeFR2010_carnet_III_actpresMelt$value), 1, 0)
timeFR2010_carnet_III_actpresMelt$parents = ifelse(grepl('3', x = timeFR2010_carnet_III_actpresMelt$value), 1, 0)
timeFR2010_carnet_III_actpresMelt$child = ifelse(grepl('4', x = timeFR2010_carnet_III_actpresMelt$value), 1, 0)
timeFR2010_carnet_III_actpresMelt$housemember = ifelse(grepl('5', x = timeFR2010_carnet_III_actpresMelt$value), 1, 0)
timeFR2010_carnet_III_actpresMelt$peers = ifelse(grepl('6', x = timeFR2010_carnet_III_actpresMelt$value), 1, 0)

head(timeFR2010_carnet_III_actpresMelt)

###########################
save(timeFR2010_carnet_III_actpresMelt, file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_carnet_III_actpresMelt.RData')
load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_carnet_III_actpresMelt.RData')

save(timeFR2010_carnet_III_actprincMelt, file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_carnet_III_actprincMelt.RData')
load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_carnet_III_actpresMelt.RData')
###########################



###########################
###### proportion of act ##
###########################

# 
tus = timeFR2010_carnet_III_actprincMelt %>% group_by(variable, value) %>% summarise(prop = n()) %>% mutate(prop = prop / sum(prop))
n_distinct(tus$variable)

# 
ggplot(tus, aes(x = variable, fill = factor(value), y = prop)) + 
  geom_bar(stat = "identity", width = 1, colour = "white") + theme_minimal()
# 

# sample days by couple and days # only 1 diaries 
timeFR2010_carnet_III_actpresence = timeFR2010_carnet_III %>% select(idind, jour, matches('pres', 'jour')) %>% select(-matches('presv'))
timeFR2010_carnet_III_actpresenceMelt  = timeFR2010_carnet_III_actpresence %>% melt(id.vars = c('idind', 'jour')) 

timeFR2010_carnet_III_actpresenceMelt %>% head()
table(timeFR2010_carnet_III_actpresenceMelt$value)
# 

# 5 co-presence 
nchar(timeFR2010_carnet_III_actpresenceMelt$value)
# 
timeFR2010_carnet_III_actpresenceMelt$value_rec = substr(timeFR2010_carnet_III_actpresenceMelt$value, start = 1, stop = 1)
# 
tus = timeFR2010_carnet_III_actpresenceMelt %>% group_by(variable, value_rec) %>% summarise(prop = n()) %>% mutate(prop = prop / sum(prop))
n_distinct(tus$variable) 
# 

# 
ggplot(tus, aes(x = variable, fill = factor(value_rec), y = prop)) + 
  geom_bar(stat = "identity", width = 1, colour = "grey") + theme_minimal()
# 

# 
tus %>%
  ggplot(aes(x=variable,y=idind,fill=value)) +
  geom_tile() + theme_minimal()



