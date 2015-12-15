# The objects # 
# Je préfère consacrer un script séparé aux objects et aux données - au lieu d'avoir des gigantesques lignes de codes dans un même fichier 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dataCIatur2015.RData') 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(cluster)
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
library(reshape2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Il faut selectionner les jours ! 

n_distinct(dataC$householdid.x)
dataC = as.data.frame(dataC)

########################################################################################################################################
############################################################# WEEKENDS #################################################################
########################################################################################################################################

dtc = filter(dataC, day_rec == 'Weekend')
dtd = filter(dataC, day_rec == 'Weekdays')

women = which(dtc$isex == 'FEMALE')
men = which(dtc$isex == 'MALE')

womend = which(dtd$isex == 'FEMALE')
mend = which(dtd$isex == 'MALE')

# Select the sequence Weekend 
dtcseq = select(dtc, contains('act1') )
# head(dtcseq)

# Select the sequence Weekdays
dtdseq = select(dtd, contains('act1') )
# head(dtcseq)

##################
# With Whom  
##################

# alone 
#wit0 
# with children up to 9 
#wit1_003
# With children aged 10 to 14 living in your household between
#wit2_003
# With other household members 
#wit3_003
# With other persons that you know 
#wit4_003 

# Alone 
seq_alone = select(dataC, householdid.x, idno, contains('wit0'))
seq_alone_s = seqdef(seq_alone[, -c(1,2)], cpal = c('orange', 'white'))

# Children 
seq_child9 = select(dataC, householdid.x, idno, contains('wit1'))
seq_child9_s = seqdef(seq_child9[, -c(1,2)], cpal = c('white', 'orange'))

seq_child14 = select(dataC, householdid.x, idno, contains('wit2'))
seq_child14_s = seqdef(seq_child14[, -c(1,2)], cpal = c('white', 'orange'))

# HouseMember 
seq_houseMemb = select(dataC, householdid.x, idno, contains('wit3'))
seq_houseMemb_s = seqdef(seq_houseMemb[, -c(1,2)], cpal = c('white', 'orange'))

# Others 
seq_acquaintance = select(dataC, householdid.x, idno, contains('wit4'))
seq_acquaintance_s = seqdef(seq_acquaintance[, -c(1,2)], cpal = c('white', 'orange'))

# Let us create dummies ! 
alone = select(dataC, contains('wit0')) 
alone = ifelse(alone == "Alone or with people you don't know", 1, 0)

child1 = select(dataC, contains('wit1')) 
child1 = ifelse(child1 == "With children up to 9 living in your household", 1, 0)

child2 = select(dataC, contains('wit2')) 
child2 = ifelse(child2 == "With children aged 10 to 14 living in your household", 1, 0)

child = ifelse(child1 == 1 | child2 == 1, 1, 0)

partner = select(dataC, contains('wit3')) 
partner = ifelse(partner == "With other household members", 1, 0)

acquaintance = select(dataC, contains('wit4')) 
acquaintance = ifelse(acquaintance == "With other people that you know", 1, 0)

# Either WORK / SLEEP / STUDY 
wit5 = select(dataC, contains('wit5'))
# Sequences 
seqwit5 = seqdef(wit5)

# # # 
# # # 

# Le temps 
timer = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
# length(timer)

colnames(seq_alone_s) <- timer
colnames(seq_child9_s) <- timer
colnames(seq_houseMemb_s) <- timer
colnames(seq_acquaintance_s) <- timer

family <- matrix(0, ncol = ncol(partner), nrow = nrow(partner))

for(j in 1:ncol(partner)){  
  for(i in 1:nrow(partner)){  
    
    # 9     Partner    Children    Acquaintance NotAlone
    if(acquaintance[i,j] == 1 & child[i,j] == 1 & partner[i,j] == 1) 
    {family[i,j] <- 'e nuclear and acquaintance'}   
    
    # 10 NotPartner    Children    Acquaintance NotAlone
    if(acquaintance[i,j] == 1 & child[i,j] == 1 & partner[i,j] != 1) 
    {family[i,j] <- 'f children and acquaintance'}   
    
    # 11    Partner NotChildren    Acquaintance NotAlone
    if(acquaintance[i,j] == 1 & child[i,j] != 1 & partner[i,j] == 1) 
    {family[i,j] <- 'g partner and acquaintance'}   
    
    # 12 NotPartner NotChildren    Acquaintance NotAlone 
    if(acquaintance[i,j] == 1 & child[i,j] != 1 & partner[i,j] != 1) 
    {family[i,j] <- 'h acquaintance'}   
    
    # 13    Partner    Children NotAcquaintance NotAlone
    if(partner[i,j] == 1 & child[i,j] == 1 & acquaintance[i,j] != 1)
    {family[i,j] <- 'd nuclear'}   
    
    # 14 NotPartner    Children NotAcquaintance NotAlone
    if(child[i,j] == 1 & partner[i,j] != 1 & acquaintance[i,j] != 1) 
    {family[i,j] <- 'c child'}   
    
    # 15    Partner NotChildren NotAcquaintance NotAlone
    if(partner[i,j] == 1 & child[i,j] != 1 & acquaintance[i,j] != 1) 
    {family[i,j] <- 'b partner'}   
    
    # 8  NotPartner NotChildren NotAcquaintance    Alone
    # Alone - we enforce the ALONE with making it 1 and not stand the other variables and in the end of the loop ! 
    if(alone[i,j] == 1) 
    {family[i,j] <- 'a alone'}  
    
    # 16 NotPartner NotChildren NotAcquaintance NotAlone
    if(partner[i,j] != 1 & child[i,j] != 1 & acquaintance[i,j] != 1 & alone[i,j] != 1) 
    {family[i,j] <- 'i True Missing'}   
    
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Mining notnotnot # # # # # # # # # # # # # # ## 
# # # # # # # # # # # # # # # # # # # # # # # # # 

# Either WORK / SLEEP / STUDY 
wit5 = select(dataC, contains('wit5'))
# Sequences 
seqwit5 = seqdef(wit5)

# # # 
# seqdplot(seqwit5, border = NA, cpal = c('white', 'red'))
# # # 

########## ########## 
########## Let us see if notnotnot if out of the bound of wit5 
########## ########## 

matwit5 = as.matrix(seqwit5)
matwit5 = ifelse(matwit5 == 'Main Activity is asleep, employment or study', 1, 0)

matWIT <- matrix(0, ncol = ncol(matwit5), nrow = nrow(matwit5))

for(j in 1:ncol(matwit5)){
  for(i in 1:nrow(matwit5)){
    
    if(matwit5[i,j] == 1 & family[i,j] != 'notnotnot') 
    {matWIT[i,j] <- 'j work study sleep'}  
    
    if(matwit5[i,j] != 1 & family[i,j] == 'notnotnot') 
    {matWIT[i,j] <- 'k TRUEMISSING'}  
    
    if(matwit5[i,j] != 1 & family[i,j] != 'notnotnot') 
    {matWIT[i,j] <- family[i,j]}  
    
    #    if(matwit5[i,j] == 1 & family[i,j] != 'notnotnot') 
    #    {matWIT[i,j] <- 'l sleep/work'}  
    
  }
}

# Le temps 
timer = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
# length(timer)

# Colors 
rc = c("#8DD3C7", 
       "#FFFFB3", 
       "#FB8072", 
       "#B3DE69", 
       "#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5", # blues for acquaintances, 
       "#BC80BD",  "gainsboro")  
# length(rc)
# # # # 
colnames(matWIT) <- timer
# 

family = matWIT

# save(family, file = "/Users/giacomovagni/ukavecqui/data/family.RData")
load("/Users/giacomovagni/ukavecqui/data/family.RData") 
