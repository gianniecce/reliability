
# Follows 2015-11-20 FR Coding Explanatory variables 
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

names(timeFR2010_couple) [ grepl('rec', names(timeFR2010_couple)) ] 

# typmen15_rec 
# situacj_rec 
# sexe_rec 
# age 
# 


timeFR2010_couple %>% count(typmen15_rec)
timeFR2010_couple %>% count(etamatri)
timeFR2010_couple %>% count(dip14_rec)
timeFR2010_couple %>% count(ag6)

timeFR2010_couple %>% count(npers, nenfants)
timeFR2010_couple %>% count(nenfants_rec)

timeFR2010_couple %>% count(typmen5)

timeFR2010_couple %>% count(enfhors)
timeFR2010_couple %>% count(handic1e)

timeFR2010_couple %>% count(enfant)

timeFR2010_couple %>% count(npers, nenfants)


timeFR2010_couple %>% group_by(idmen, ncouples) %>% summarise(n())


timeFR2010_couple %>% count(nenfants,npers)



# save(timeFR2010_couple, file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple2Cleaned.RData')
load(file = '/Users/giacomovagni/Rprojects/reliability/dataFR/timeFR2010_couple2Cleaned.RData')


