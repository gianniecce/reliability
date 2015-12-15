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

########################################################################################################################################
############################################################# Act rec #################################################################
########################################################################################################################################

# # # #
# Recoding Function 
# # # #

recodeList <- function(x) {
  for (i in seq_along(recodes)) {
    x[x %in% recodes[[i]]] <- names(recodes)[i]
  }
  return(x)
}

# # # #
# # # # # # # # # # # #
# # # #

names(table(as.matrix(dtdseq)))

recodes = 
  list( 'a Sleep' = c("Sleep", "Sick in bed", 
                      "Help in employment and farming", 
                      "Unspecified activities related to employment"), 
        
        'b Work' = c(  "Working time in main job", 
                       "Activities related to job seeking", 
                       "Coffee and other breaks in main job",          
                       "Commercial and administrative services" , 
                       "Other specified activities related to employment", 
                       "Other specified organisational work", 
                       "Work for an organisation",                    
                       "Working time in second job", 
                       "Coffee and other breaks in second job"), 
        
        'c Studies/library' = c("Classes and lectures", 
                                "Brwing bks rcds audio video,CDs,VDs from library", 
                                "Free Time Study", 
                                "Unspecified study", 
                                "Unspecified library", 
                                "Other specified activities related to school or university ", 
                                "Other specified library activities", 
                                "Homework", 
                                "Reference to bks and other library materials within library" ), 
        
        'd Travel/Commute' = c( "Other specified travel", 
                                "Travel escorting a child (other than education)",             
                                "Travel escorting an adult (other than education)",    
                                "Travel escorting to/ from education",                         
                                "Travel for day trip/ just walk",                      
                                "Travel related to education" ,                                
                                "Travel related to entertainment and culture",     
                                "Travel related to gambling",                     
                                "Travel related to hobbies other than gambling",       
                                "Travel related to household care",                            
                                "Travel related to hunting & fishing",             
                                "Travel related to informal help to other households",         
                                "Travel related to organisational work",           
                                "Travel related to other social activities",                   
                                "Travel related to personal business",             
                                "Travel related to physical exercise",                         
                                "Travel related to religious activities" ,         
                                "Travel related to services",                                  
                                "Travel related to shopping",                       
                                "Travel related to unspecified time use",                      
                                "Travel rlt to participatory actv except rel actv" ,    
                                "Travel to holiday base",                                      
                                "Travel to visit friends/ relatives in their homes",   
                                "Travel to work from home and back only", 
                                'Travel to work from a place other than home', 
                                "Travel in the course of work"), 
        
        'e Cooking' = c("Food preparation", 
                        "Baking", 
                        "Other specified food management"
                        ), 
        
        'f Housework' = c("Dish washing",                                                        
                          "Cleaning dwelling",  
                          "Cleaning yard",  
                          "Disposal of Waste", 
                          "Household management not using the internet", 
                          "Ironing",                                                                                                
                          "Laundry", 
                          "Other specified household upkeep", 
                          "Wash and dress", 
                          "Unspecified household upkeep" ), 
        
        'g Odd jobs' =  c("Construction and repairs as help", 
                          "Gardening",                            
                          "Gardening and pet care as help", 
                          "Handicraft and producing textiles", 
                          "Heating and water", 
                          "House construction and renovation", 
                          "Other specified construction and repairs",    
                          "Other specified gardening and pet care",  
                          "Other specified making and care for textiles",
                          "Other specified making, repairing and maintaining equipment", 
                          "Other specified physical exercise" ,
                          "Other specified water sports", 
                          "Repairs of dwelling", 
                          "Various arrangements",                   
                          "Vehicle maintenance", 
                          "Unspecified construction and repairs", 
                          "Unspecified making, repairing and maintaining equipment"), 
        
        'h Eating' = c("Eating", "Feasts", "Lunch break"), 
        
        'i Child care'= c( "Accompanying child",            
                           "Feeding the child", 
                           "Physical care and supervision of a child as help", 
                           "Other specified childcare", 
                           "Reading, playing & talking to the child as help", 
                           "Reading, playing and talking with child", 
                           "Teaching the child", 
                           "Unspecified childcare as help", 
                           "Unspecified physical care & supervision of a child", 
                           "Unspecified childcare", 
                           "Other specified childcare as help", 
                           "Teaching the child as help", 
                           'Accompanying the child as help', 
                           "Unspecified personal care"),  
        
        'j Care for others' = c("Caring for pets", 
                                "Food management as help", 
                                "Household upkeep as help", 
                                "Accompanying an adult household member", 
                                "Personal services",                   
                                "Physical care & supervision of an adult household member", 
                                "Other specified help to an adult household member", 
                                "Other specified informal help", 
                                "Physical care and supervision of an adult as help", 
                                "Other specified help to an adult member of another household",
                                "Other specified personal care",
                                "Other specified physical care & supervision of a child", 
                                "Tending domestic animals", 
                                "Walking the dog", 
                                "Unspecified help to an adult member of another household", 
                                "Unspecified household and family care",      
                                "Unspecified informal help", 
                                "Unspecified help to an adult household member", 
                                "Unspecified other personal care", 
                                "Accompanying an adult as help"),    
        
        'k Shopping' = c("Other specified shopping", 
                         "Other specified shopping and services", 
                         "Shopping and services as help",                               
                         "Shopping mainly for clothing",                             
                         "Shopping mainly for food",                                    
                         "Shopping mainly related to accommodation",              
                         "Shopping or browsing at car boot sales or antique fairs",     
                         "Shping for&ordring food via the internet", 
                         "Window shopping or other shopping as leisure", 
                         "Unspecified shopping", 
                         "Unspecified shopping and services"), 
        
        'l Leisure' = c( "Billiards, pool, snooker or petanque",                  
                         "Cinema", 
                         "Gambling", 
                         "Hunting and fishing", 
                         "Indoor pairs or doubles games", 
                         "Listening to recordings ", 
                         "Other specified hobbies", 
                         "Other specified entertainment or culture",   
                         "Other specified parlour games and play", 
                         "Punctuating activity",
                         "Reading periodicals", 
                         "Other walk or hike",                   
                         "Unspecified games", 
                         "Outdoor pairs or doubles games",    
                         "Outdoor team games", 
                         "Other specified games", 
                         "Resting \226 Time out", 
                         "Solo games and play", 
                         "Sports events", 
                         "Taking a walk or hike that lasts at least 2 miles or 1 hour", 
                         "Visiting a leisure park", 
                         "Visiting an urban park, playground or designated play area", 
                         "Unspecified games and play with others", 
                         "Other specified games", 
                         "Indoor team games", 
                         "Preserving", 
                         "Other specified arts", 
                         "Other specified games ", 
                         "Live music other than classical concerts, opera and musicals", 
                         "Other specified ball games", 
                         "Picking berries, mushroom and herbs"
                         
                         ),  
        
        'm Highbrow' = c("Art exhibitions and museums", 
                         "Chess and bridge", 
                         "Making videos, taking photos or related activities", 
                         "Other specified performing arts", 
                         "Other specified reading", 
                         "Painting, drawing or other graphic arts", 
                         "Reading books", 
                         "Singing or other musical activities", 
                         "Visiting a historical site", 
                         "Visiting a wildlife site",  
                         "Woodcraft, metal craft, sculpture and pottery", 
                         "Unspecified reading",                                             
                         "Unspecified theatre or concerts", 
                         'Plays, musicals or pantomimes', 
                         "Literary arts", 
                         "Concerts or other performances of classical music", 
                         "Other specified theatre or concerts" ), 
        
        'n Sport' = c("Fitness", 
                      "Activities related to productive exercise", 
                      "Activities related to sports", 
                      "Biking", 
                      "Gymnastics", 
                      "Jogging and running", 
                      "Skiing or skating", 
                      "Swimming", 
                      "Unspecified sports related activities", 
                      "Unspecified physical exercise" ), 
        
        'o Telephone/Online Communication' = c("Communication on the internet", 
                                               "Correspondence", 
                                               "Telephone conversation"), 
        
        'p Computing/Internet' = c("Computer games", 
                                   "Computing \226 programming", 
                                   "Information searching on the internet", 
                                   "Other specified communication by computing", 
                                   "Other specified computing", 
                                   "Other specified information by computing", 
                                   "Unspecified internet use", 
                                   "Unspecified communication by computer", 
                                   "Unspecified other computing", 
                                   "Unspecified information by computing", 
                                   "Shping for&ordring unspec gds&srvs via internet", 
                                   "Banking and bill paying via the internet", 
                                   "Other specified household management using the internet" ), 
        
        'q Civic' = c( "Meetings", 
                       "Volunteer work through an organisation", 
                       "Unspecified participatory activities", 
                       "Other specified participatory activities"), 
        
        'r Religious' = c( "Religious activities"), 
        
        's Visiting/Socialising' = c("Other specified social life", 
                                     "Socialising with household members", 
                                     "Visiting and receiving visitors"), 
        
        't TV/Radio' = c("Listening to sport on the radio", 
                         "Other specified TV watching", 
                         "Other specified video watching", 
                         "Other specified radio listening", 
                         "Watching a film on TV",
                         "Watching a film on video", 
                         "Watching sport on TV ", 
                         "Watching sport on video", 
                         "Unspecified TV watching", 
                         "Unspecified video watching", 
                         "Unspecified listening to radio and music",
                         "Unspecified radio listening", 
                         "Listening to music on the radio"), 
        
        'u Others' = c("Filling in the time use diary", 
                       "Illegible activity", 
                       "No main activity, no idea what it might be", 
                       "No main activity, some idea what it might be", 
                       "Unspecified time use")
  ) 


head(dtcseq)
dtcseq = as.matrix(dtcseq) 
dtcseq  = recodeList(dtcseq)
head(dtcseq)

head(dtdseq)
dtdseq = as.matrix(dtdseq) 
dtdseq  = recodeList(dtdseq)
head(dtdseq)

names(table(dtdseq))

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
rc = c(brewer.pal(n = 10, name = 'Set3'))  
# length(rc)
# # # # 
colnames(matWIT) <- timer
# 

weekend = which(dataC$day_rec == 'Weekend')
weekdayz = which(dataC$day_rec == 'Weekdays')

SequenceWithWhomWeekend = seqdef(matWIT[weekend, ], cpal = rc) 
SequenceWithWhomWeekdayz = seqdef(matWIT[weekdayz, ], cpal = rc) 

# Store in a List 
data = list()
data$SequenceWithWhomWeekend <- SequenceWithWhomWeekend 
data$SequenceWithWhomWeekdayz <- SequenceWithWhomWeekdayz 
data$dtcseq <- dtcseq 
data$dtdseq <- dtdseq 

# save(data, file = "/Users/giacomovagni/ukavecqui/data/dataAvecQui.RData")
load("/Users/giacomovagni/ukavecqui/data/dataAvecQui.RData") 
