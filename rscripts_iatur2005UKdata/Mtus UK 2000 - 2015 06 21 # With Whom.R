
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dataCIatur2015.RData') 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

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
library(RColorBrewer)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Il faut selectionner les jours ! 

########################################################################################################################################
############################################################# WEEKENDS #################################################################
########################################################################################################################################

dtc = filter(dataC, day_rec == 'Weekend')

women = which(dtc$isex == 'FEMALE')
men = which(dtc$isex == 'MALE')

# Select the sequence
dtcseq = select(dtc, contains('act1') )
head(dtcseq)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Le temps 
timer = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
length(timer)

##################
# Where 
##################
seq_where = select(dataC, householdid.x, idno, contains('wher'))
seq_where_r = ifelse(seq_where == " Home ", yes = 'Home', no = ifelse(seq_where == " Other people's home ", "Other people's home", no = 'Other Location'))
seq_where_r[is.na(seq_where_r)] <- 'NA'

# Sequence Location 
seq_where_s = seqdef(seq_where_r[, -c(1,2)])
seqdplot(seq_where_s, border = NA, withlegend = F)
seqlegend(seq_where_s)

##################
# With Whom  
##################

# alone 
wit0 
# with children up to 9 
wit1_003
# With children aged 10 to 14 living in your household between
wit2_003
# With other household members 
wit3_003
# With other persons that you know 
wit4_003 

# Alone 
seq_alone = select(dtc, householdid.x, idno, contains('wit0'))
seq_alone_s = seqdef(seq_alone[, -c(1,2)], cpal = c('orange', 'white'))
seqdplot(seq_alone_s, border = NA)

# Children 
seq_child9 = select(dtc, householdid.x, idno, contains('wit1'))
seq_child9_s = seqdef(seq_child9[, -c(1,2)], cpal = c('white', 'orange'))
seqdplot(seq_child9_s, border = NA)

seq_child14 = select(dtc, householdid.x, idno, contains('wit2'))
seq_child14_s = seqdef(seq_child14[, -c(1,2)], cpal = c('white', 'orange'))
seqdplot(seq_child14_s, border = NA)

# HouseMember 
seq_houseMemb = select(dtc, householdid.x, idno, contains('wit3'))
seq_houseMemb_s = seqdef(seq_houseMemb[, -c(1,2)], cpal = c('white', 'orange'))
seqdplot(seq_houseMemb_s, border = NA)

# Others 
seq_acquaintance = select(dtc, householdid.x, idno, contains('wit4'))
seq_acquaintance_s = seqdef(seq_acquaintance[, -c(1,2)], cpal = c('white', 'orange'))
seqdplot(seq_acquaintance_s, border = NA)

# Let us create dummies ! 

alone = select(dtc, contains('wit0')) 
alone = ifelse(alone == "Alone or with people you don't know", 1, 0)

child1 = select(dtc, contains('wit1')) 
child1 = ifelse(child1 == "With children up to 9 living in your household", 1, 0)

child2 = select(dtc, contains('wit2')) 
child2 = ifelse(child2 == "With children aged 10 to 14 living in your household", 1, 0)

child = ifelse(child1 == 1 | child2 == 1, 1, 0)

partner = select(dtc, contains('wit3')) 
partner = ifelse(partner == "With other household members", 1, 0)

acquaintance = select(dtc, contains('wit4')) 
acquaintance = ifelse(acquaintance == "With other people that you know", 1, 0)

########################
# Combined Alphabet 
########################

Cbn = expand.grid( c('Partner', 'NotPartner'), 
                   c('Children', 'NotChildren'),
                   c('Acquaintance', 'NotAcquaintance'), 
                   c('Alone', 'NotAlone')
)

Cbn

family <- matrix(0, ncol = ncol(partner), nrow = nrow(partner))

for(j in 1:ncol(partner)){
  for(i in 1:nrow(partner)){
    
    # 9     Partner    Children    Acquaintance NotAlone
    if(acquaintance[i,j] == 1 & child[i,j] == 1 & partner[i,j] == 1) 
    {family[i,j] <- 'nuclear and acquaintance'}   
    
    # 10 NotPartner    Children    Acquaintance NotAlone
    if(acquaintance[i,j] == 1 & child[i,j] == 1 & partner[i,j] != 1) 
    {family[i,j] <- 'children and acquaintance'}   
    
    # 11    Partner NotChildren    Acquaintance NotAlone
    if(acquaintance[i,j] == 1 & child[i,j] != 1 & partner[i,j] == 1) 
    {family[i,j] <- 'partner and acquaintance'}   
    
    # 12 NotPartner NotChildren    Acquaintance NotAlone 
    if(acquaintance[i,j] == 1 & child[i,j] != 1 & partner[i,j] != 1) 
    {family[i,j] <- 'acquaintance'}   
    
    # 13    Partner    Children NotAcquaintance NotAlone
    if(partner[i,j] == 1 & child[i,j] == 1 & acquaintance[i,j] != 1)
    {family[i,j] <- 'nuclear'}   
    
    # 14 NotPartner    Children NotAcquaintance NotAlone
    if(child[i,j] == 1 & partner[i,j] != 1 & acquaintance[i,j] != 1) 
    {family[i,j] <- 'child'}   
    
    # 15    Partner NotChildren NotAcquaintance NotAlone
    if(partner[i,j] == 1 & child[i,j] != 1 & acquaintance[i,j] != 1) 
    {family[i,j] <- 'partner'}   
    
    # 8  NotPartner NotChildren NotAcquaintance    Alone
    # Alone - we enforce the ALONE with making it 1 and not stand the other variables and in the end of the loop ! 
    if(alone[i,j] == 1) 
    {family[i,j] <- 'alone'}  
    
    # 16 NotPartner NotChildren NotAcquaintance NotAlone
    if(partner[i,j] != 1 & child[i,j] != 1 & acquaintance[i,j] != 1 & alone[i,j] != 1) 
    {family[i,j] <- 'notnotnot'}   
    
  }
}

layout(rbind(c(2,1,1,1)))
barplot(table(family), las = 2, horiz = T, col = brewer.pal(9, 'Set3'))

# select time of day 
colnames(family) <- timer
family_rec = as.data.frame(family)
row.number(family_rec)

barplot(table(family[,c(1:25, 110:144)]), las = 2, horiz = T, col = brewer.pal(9, 'Set3'))
barplot(table(family[,c(26:110)]), las = 2, horiz = T, col = brewer.pal(9, 'Set3'))

women = which(dtc$isex == 'FEMALE')
men = which(dtc$isex == 'MALE')

prop.table( table(Women = family[women,c(26:110)]) )
prop.table( table(Men = family[men,c(26:110)]) )

# Sequence 
seqFam = seqdef(family)
seqdplot(seqFam, border = NA)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Mining notnotnot # # # # # # # # # # # # # # ## 
# # # # # # # # # # # # # # # # # # # # # # # # # 

# Either WORK / SLEEP / STUDY 
wit5 = select(dtc, contains('wit5'))
# Sequences 
seqwit5 = seqdef(wit5)

# # # 
seqdplot(seqwit5, border = NA, cpal = c('white', 'red'))
# # # 

########## ########## 
########## Let us see if notnotnot if out of the bound of wit5 
########## ########## 

matwit5 = as.matrix(seqwit5)
matwit5 = ifelse(matwit5 == 'Main Activity is asleep, employment or study', 1, 0)
matwit5

matWIT <- matrix(0, ncol = ncol(matwit5), nrow = nrow(matwit5))

for(j in 1:ncol(matwit5)){
  for(i in 1:nrow(matwit5)){
    
    if(matwit5[i,j] == 1 & family[i,j] == 'notnotnot') 
    {matWIT[i,j] <- 'work study sleep'}  
    
    if(matwit5[i,j] != 1 & family[i,j] == 'notnotnot') 
    {matWIT[i,j] <- 'TRUEMISSING'}  
    
    if(matwit5[i,j] != 1 & family[i,j] != 'notnotnot') 
    {matWIT[i,j] <- family[i,j]}  
    
    if(matwit5[i,j] == 1 & family[i,j] != 'notnotnot') 
    {matWIT[i,j] <- 'sleep/work'}  
    
  }
}

#### Watch out !! 
#### 0 means something ! 
# It means the true missing family time ! 

# Alternative 
mat1 = matwit5
mat2 = ifelse(family == 'notnotnot', 1, 0)
# loop without loop 
matWIT_test = ifelse( ( mat1 | !mat2 ), 0, 1)
# check 
table(matWIT)
table(matWIT_test)
# we loose the distinction between sleep/study/work and 0 -> the good activities 
# but hte still not not not is correct 

########## 
########## Check Sequence 
########## 

seqWITT = seqdef(matWIT)
seqdplot(seqWITT, border = NA)
seqmeant(seqWITT) * 10 
seqistatd(seqWITT) * 10 

seqmsplot(seqWITT, border = NA) 
seqmtplot(seqWITT, border = NA) 

# Summary 
seqWITT_m = as.matrix(seqWITT)
data_frame(var = c(seqWITT_m)) %>% 
  group_by_("var") %>% 
  summarise(n())

require(tidyr)
# grouped
seqWITT_m %>% 
  as.data.frame %>% 
  gather %>% 
  group_by(variable, value) %>% 
  summarise(N = n()) %>% 
  spread(variable, N)

########################
######################## What I need to do now is to check what's the activities associated with this missings 
########################

matWIT_test

##################################################################
##################################################################

dtcseq

# Colors 
rc = c( brewer.pal(n = 7, name = 'Set2'), brewer.pal(n = 7, name = 'Set1'), brewer.pal(n = 8, name = 'Set3'))  
length(rc)

# Le temps 
timer = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
length(timer)
# 
colnames(dtcseq) <- c(timer)
# Def Seq 
dtcseq = seqdef(dtcseq, cpal = rainbow(210))

# dplot 
seqdplot(dtcseq, border = NA)

# how to check !? 
dtcsek = as.matrix(dtcseq)
matWIT_test

ncol(dtcsek)
ncol(matWIT_test)
nrow(dtcsek)
nrow(matWIT_test)

which(matWIT_test == 1)
matWIT_check <- matrix(0, ncol = ncol(dtcsek), nrow = nrow(dtcsek))

for(j in 1:ncol(matWIT_check)){
  for(i in 1:nrow(matWIT_check)){
    
    if(matWIT_test[i,j] == 1) 
    {matWIT_check[i,j] <- dtcsek[i,j]}  
    
    if(matWIT_test[i,j] != 1) 
    {matWIT_check[i,j] <- '0'}  
    
  }
}

matWIT_check
# Colors 
rc = c( brewer.pal(n = 7, name = 'Set2'), brewer.pal(n = 6, name = 'Set1'), brewer.pal(n = 5, name = 'Set3'))  
length(rc)
# 
matWIT_check_seq = seqdef(matWIT_check, cpal = rainbow(124)) 
# 
seqdplot(matWIT_check_seq, border = NA)
# seqIplot(matWIT_check_seq, border = NA)

par(mfrow = c(1,1))
seqmsplot(matWIT_check_seq, border = NA) 
seqmtplot(matWIT_check_seq, border = NA, ylim = c(0,2), las = 2, withlegend = F) 
round(seqmeant(matWIT_check_seq), 2) * 10

e Travel/Commute                    3.3
f Cooking                           2.5
g Housework                         7.1
h Odd jobs                          8.3
i Eating                            3.8
m Leisure                           2.7
u TV/Radio                          9.9
v Others                            6.1

########################
######################## Check the incoherence - Solo / Partner 
########################

seqWithWhomMen = seqdef(matWIT[men, ], cpal = brewer.pal(10, 'Set3'))
seqWithWhomWomen = seqdef(matWIT[women, ], cpal = brewer.pal(10, 'Set3'))

seqdplot(seqWithWhomMen, border = NA, cex.legend = 0.6)
seqdplot(seqWithWhomWomen, border = NA, cex.legend = 0.6)

matWIT[1,1:115]

# Il faut que je trouve comment trouver le premier episode apres 
SoloNight = ifelse(matWIT[1:10,1:40] == 'alone', 1, 0)
sd = seqdef(SoloNight)
seqdplot(sd, border = NA)

############
############

matSolo = matrix(0, ncol = ncol(SoloNight), nrow = nrow(SoloNight))

indx <- max.col(SoloNight, 'first')*(rowSums(SoloNight)!=0)
matSolo[cbind(1:nrow(matSolo), indx) ] <- 1

NightSoloImp = rowSums(matSolo) 

wAlone = which(max.col(SoloNight, 'first') > 1)
SoloNight [wAlone, ] 

# Ensuite je dois trouver comment mettre un 1 avant tous les premiers episodes 

max.col( SoloNight [wAlone, ] )
Sn = SoloNight [wAlone, ] 

########################
######################## For workweek too ! 
######################## 