library(Hmisc)
library(plyr)
library(dplyr)
library(mtusRlocal)
library(magrittr)
library(reshape)
library(reshape2)
library(TraMineR)
library(mtusRlocal)
library(Rage)

# 
# SN1 ????????????Point number Area / quota indicator
# Each person was asked to complete diaries for 2 separate days (weekday & weekend day), so this has the values 1 or 2.
# SN1 + SN2 + SN3 + SN4 is the unique serial number for each record. There are 20981 records on the file.
# 

load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtindividual.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtdiaryEpisode.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtdiary.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dthhld.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtworksheet.RData')
load(file ='/Users/giacomovagni/Documents/Data/TimeUse/UK/UK_R/dtweight.RData')

dtindividual = as.data.frame( apply(dtindividual, MARGIN = 2, FUN = as.character) )
dtdiary = as.data.frame( apply(dtdiary, MARGIN = 2, FUN = as.character) )

################################################  ###########################################################
################################################  Unique key ################################################
################################################  ###########################################################

# compute hldid = sn1*1000 + sn2.
# compute persid = sn3.
# compute id = sn4.

dtindividual$SN1 = gsub('[[:blank:]]', '', dtindividual$SN1)
dtindividual$SN2 = gsub('[[:blank:]]', '', dtindividual$SN2)
dtindividual$SN3 = gsub('[[:blank:]]', '', dtindividual$SN3)

dtindividual$householdid = paste(dtindividual$SN1, dtindividual$SN2, sep = '')
dtindividual$idno = paste(dtindividual$SN1, dtindividual$SN2, dtindividual$SN3, sep = '')
colnames(dtindividual) = tolower(colnames(dtindividual))

dtdiary$SN1 = gsub('[[:blank:]]', '', dtdiary$SN1)
dtdiary$SN2 = gsub('[[:blank:]]', '', dtdiary$SN2)
dtdiary$SN3 = gsub('[[:blank:]]', '', dtdiary$SN3)

dtdiary$householdid = paste(dtdiary$SN1, dtdiary$SN2, sep = '')
dtdiary$idno = paste(dtdiary$SN1, dtdiary$SN2, dtdiary$SN3, sep = '')
colnames(dtdiary) = tolower(colnames(dtdiary))

dtindividual$idno = as.numeric(dtindividual$idno)
dtdiary$idno = as.numeric(dtdiary$idno)

dtindividual = arrange(dtindividual, idno)
dtdiary = arrange(dtdiary, idno)

n_distinct(dtindividual$idno) 
n_distinct(dtdiary$idno)

################################################  ###########################################################
################################################  Select Sample ############################################# 
################################################  ###########################################################

# Pour cet article nous n'avons pas besoin forcément de prendre 
# les couples AVEC enfant(s) 
# Simplement les couples 

table(dtindividual$hhtype5)
levels(dtindividual$hhtype5)

# select married / coha with children less than 15 
data = filter(dtindividual, hhtype5 == "Married/cohab couple - with children <= 15 " |  hhtype5 == "Married/cohab couple -  NO children <= 15 ")

## C'est une bonne condition - on sait un peu qui sont nos répondants 
# select only the age group 25-44
data = subset(data, iagegrp == "25 -44 yrs") 
# employement 
data = subset(data, 
              econact3 == "Econ active - full time" | 
                econact3 == "Econ active - part time" |  
                econact3 == "Econ active - unemployed (ILO definition)" | 
                econact3 == "Econ inactive - looking after family/ home"
) 

table(data$econact3)

# check 
table(data$spouse1) 
table(data$spouse2) 

# problem with number of people living in household 
data_c = filter(data, sn3 == 1 | sn3 == 2)

# check 
table(data_c$spouse1) 
table(data_c$spouse2) 

###########
###########

data_c %>% 
  group_by (ifelse(iethnic == 'WHITE', 1, 0)) %>% 
  summarise( n = n() ) %>% 
  mutate(freq = n / sum(n)) 

data_c %>% 
  group_by (ifelse(iethnic == 'WHITE', 1, 0), isex) %>% 
  summarise(n(), mean(iage))

data_c %>% 
  group_by (isex) %>%
  summarise(n(), mean(iage), sd(iage))

data_c %>% 
  group_by (isex, pq55) %>%
  summarise(n())

data_c %>% 
  group_by (ageyngst) %>% 
  summarise(n(), mean(iage))

data_c %>% 
  group_by (child) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c$numchild_rec = cut(as.numeric(as.character(data_c$numchild)), breaks = c(-Inf, 0, 1, 2, Inf), labels = c('0', '1', '2', '>2'))
table(data_c$numchild_rec, data_c$numchild)

data_c %>% 
  group_by (numchild_rec != 0, ageyngst) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c %>% 
  group_by (numchild) %>% 
  summarise(n())

data_c %>% 
  filter(isex == 'MALE') %>%
  group_by (hhtype5, econact3) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c %>% 
  filter(isex == 'FEMALE') %>%
  group_by (hhtype5, econact3) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c$econact3 = droplevels(data_c$econact3)

data_c = data_c %>% 
  group_by(sn1, sn2, sn3) %>% 
  mutate(occupPartner= rev(econact3)) 

data_c %>% 
  group_by(isex, occupPartner) %>% 
  summarise( n = n() ) %>% 
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c %>% 
  group_by(isex, hiqual5) %>% 
  summarise( n = n() ) %>% 
  mutate(freq = n / sum(n),  Total = sum(freq))

data_c %>% 
  group_by(isex, hiqual4) %>% 
  summarise( n = n() ) %>% 
  as.data.frame( mutate(freq = n / sum(n),  Total = sum(freq)) )

#############
############# Homogamie 
############# 

cp(table(data_c$hiqual4), NULL)
data_c$hiqualDegree = ifelse(data_c$hiqual4 == "Degree level qualification or above", 1, 0)
data_c$hiqualNoQual = ifelse(data_c$hiqual4 == "No qualifications", 1, 0)

data_c$hiqual4 = droplevels(data_c$hiqual4)
levels(data_c$hiqual4)
table(data_c$hiqual4)
data_c$hiqual4 = factor(data_c$hiqual4, levels = c("Degree level qualification or above", 
                                  "Higher edn below degree level (eg HNC, nursing qual) ", 
                                  "A levels, vocational level 3 & equivlnt (eg AS level, NVQ 3)", 
                                  "O levels, GCSE grade A-C, vocational level 2 & equivlnt", 
                                  "GCSE below grade C, CSE, vocational level 1 & equivlnt", 
                                  "Qualification below GCSE/O level (eg trade apprenticeships) ", 
                                  "Other qualification (incl professional, vocational, foreign)", 
                                  "Qualifications - but DK which", 
                                  "Qualifications - GCSE - but DK grade", 
                                  "Qualifications - City & Guilds - DK level", 
                                  "Qualifications - Other - but DK grade/level", 
                                  "No qualifications"
                                  ))

data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[1]] <- "University"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[2]] <- "University"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[3]] <- "2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[4]] <- "2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[5]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[6]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[7]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[8]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[9]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[10]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[11]] <- "Part 2nd"
data_c$hiqual5[data_c$hiqual4 == levels(data_c$hiqual4)[12]] <- "Primary"

table(data_c$hiqual5)
data_c$hiqual5 = factor(data_c$hiqual5, levels = c('University', '2nd', 'Part 2nd', 'Primary'))
  
data_c %>% 
  group_by(isex, hiqual5) %>% 
  summarise( n = n() ) %>% 
  mutate(freq = n / sum(n),  Total = sum(freq) ) 

data_c$econact0 = ifelse(data_c$econact == 'Econ active - in employment', 'Emp', 'Other')

men = filter(data_c, isex == 'MALE') 
women = filter(data_c, isex == 'FEMALE') 

dtt = merge(men, women, by = c('sn1', 'sn2'))
dtt[1:10, c('sn1', 'sn2', 'isex.x', 'isex.y', 'econact3.x', 'econact3.y', 'econact0.x', 'econact0.y', 
            "hiqualDegree.x", "hiqualDegree.y", "hiqualNoQual.x", "hiqualNoQual.y", "hiqual5.x", "hiqual5.y")]

# correct 
prop.table(table(Men = dtt$econact3.x, Women = dtt$econact3.y) )
prop.table(table(Men = dtt$econact0.x, Women = dtt$econact0.y) ) * 100

prop.table(table(Men = dtt$hiqualDegree.x, Women = dtt$hiqualDegree.y) ) * 100
prop.table(table(Men = dtt$hiqualNoQual.x, Women = dtt$hiqualNoQual.x) ) * 100

chiSquare(dtt$hiqualDegree.x ~ dtt$hiqualDegree.y)

summary(glm(hiqualDegree.x ~ hiqualDegree.y, data = dtt, family = binomial))
exp(coef(glm(hiqualDegree.x ~ hiqualDegree.y, data = dtt, family = binomial)))
exp(coef(glm(hiqualDegree.y ~ hiqualDegree.x, data = dtt, family = binomial)))

library('fmsb')
NagelkerkeR2(glm(hiqualDegree.x ~ hiqualDegree.y, data = dtt, family = binomial))

cp(table(Men = dtt$hiqual5.x, Women = dtt$hiqual5.y), 2)

cp(table(dtt$hiqual5.x), NULL)
cp(table(dtt$hiqual5.y), NULL)

########
########

n_distinct(data_c$idno)
n_distinct(dtdiary$idno)

# Les adultes 
dtdiary_c = filter(dtdiary, dtype == 'adult diary')

# il faut separer les weekend des weekdays mais garder les deux 
dtdiary_c$day_rec = ifelse(dtdiary_c$ddayofwk == 'Saturday' | dtdiary_c$ddayofwk == 'Sunday', yes = 'Weekend', no = 'Weekdays')

dtdiary_c2 = dtdiary_c[dtdiary_c$idno %in% data_c$idno, ] 
n_distinct(dtdiary_c2$idno)
n_distinct(data_c$idno) 

## Not sure 
# Refaire mais en inversant 
# data_c2 = dtdiary_c[data_c$idno %in% dtdiary_c$idno, ]  

# merge solve the problem 
dataM = merge(data_c, dtdiary_c2, by = 'idno')
# 
# dataC = filter(dataC, householdid.x != 22471) # gay couple 

dm = dataM %>% 
  group_by(householdid.x) %>% 
  summarise( nhouse = n() ) %>% 
  merge(., dataM, all = T)

# alternative 
dm2 = dataM %>% 
  group_by(householdid.x) %>% 
  mutate( nhouse = n() )

table(dm$nhouse)
table(dm2$nhouse)

# ici il faut que ce soit 4 - car 2 partners ----> 1 weekend + 1 weekdays 
dm[1:10, c(1:10, 5191)]
dataC = filter(dm, nhouse == 4)
check = dataC[, c(1:3, 5191) ]

c = check %>% 
  group_by(householdid.x, day_rec) %>% 
  mutate(CheckBothDaysFilled = n())

# good 
table(c$CheckBothDaysFilled)

# 
table(dataC$gorpaf.x) 
plot(table(dataC$hrs_job1))
barplot(table(dataC$totpinc.x)) 

# GAY COUPLES 

# dataC = filter(dataC, householdid.x != 22471) # gay couple 

dataC %>% 
  group_by(householdid.x, isex, day_rec) %>% 
  summarise(n())
  
d = dataC %>% 
  group_by(householdid.x, isex) %>% 
  summarise( gay = n() != 2 ) 

filter(d, gay == T)  
22471  

  
dta[order(dta$householdid.x, dta$isex), ]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# save(dataC, file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dataCIatur2015.RData') 
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dataCIatur2015.RData') 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


