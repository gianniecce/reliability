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

# # # # # 

women = which(dtc$isex == 'FEMALE')
men = which(dtc$isex == 'MALE')

# Select the sequence
dtcseq = select(dataC, contains('act1') )
head(dtcseq)

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

recodes = 
  list( 'a Sleep' = c("Sleep"), 
        
        'b Personal care' = c("Sick in bed"), 
        
        'c Work' = c(  "Working time in main job", 
                       "Activities related to job seeking", 
                       "Coffee and other breaks in main job",          
                       "Commercial and administrative services" , 
                       "Other specified activities related to employment", 
                       "Other specified organisational work", 
                       "Work for an organisation",                    
                       "Working time in second job"), 
        
        'd Studies/library' = c("Classes and lectures", 
                                "Brwing bks rcds audio video,CDs,VDs from library", 
                                "Free Time Study", 
                                "Unspecified study", 
                                "Unspecified library", 
                                "Other specified activities related to school or university ", 
                                "Other specified library activities", 
                                "Homework"), 
        
        'e Travel/Commute' = c( "Other specified travel", 
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
        
        'f Cooking' = c("Food preparation",  "Baking" ), 
        
        'g Housework' = c("Dish washing",                                                        
                          "Cleaning dwelling",  
                          "Cleaning yard",  
                          "Disposal of Waste", 
                          "Household management not using the internet", 
                          "Ironing",                                                                                                
                          "Laundry", 
                          "Other specified household upkeep", 
                          "Wash and dress"), 
        
        'h Odd jobs' =  c("Construction and repairs as help", 
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
        
        'i Eating' = c("Eating", "Feasts"), 
        
        'j Child care'= c( "Accompanying child",            
                           "Feeding the child", 
                           "Physical care and supervision of a child as help", 
                           "Other specified childcare", 
                           "Reading, playing & talking to the child as help", 
                           "Reading, playing and talking with child", 
                           "Teaching the child", 
                           "Unspecified childcare as help", 
                           "Unspecified physical care & supervision of a child", 
                           "Unspecified childcare", 
                           "Other specified childcare as help"),  
        
        'k Care for others' = c("Caring for pets", 
                                "Food management as help", 
                                "Household upkeep as help", 
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
                                "Unspecified other personal care"
        ),    
        
        'l Shopping' = c("Other specified shopping", 
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
        
        'm Leisure' = c( "Billiards, pool, snooker or petanque",                  
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
                         "Outdoor pairs or doubles games",    
                         "Outdoor team games", 
                         "Resting \226 Time out", 
                         "Solo games and play", 
                         "Sports events", 
                         "Taking a walk or hike that lasts at least 2 miles or 1 hour", 
                         "Visiting a leisure park", 
                         "Visiting an urban park, playground or designated play area", 
                         "Unspecified games and play with others", 
                         "Other specified games"),  
        
        'n Highbrow' = c("Art exhibitions and museums", 
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
                         "Concerts or other performances of classical music"), 
        
        'o Sport' = c("Fitness", 
                      "Activities related to productive exercise", 
                      "Activities related to sports", 
                      "Biking", 
                      "Gymnastics", 
                      "Jogging and running", 
                      "Skiing or skating", 
                      "Swimming"), 
        
        'p Telephone/Online Communication' = c("Communication on the internet", 
                                               "Correspondence", 
                                               "Telephone conversation"), 
        
        'q Computing/Internet' = c("Computer games", 
                                   "Computing \226 programming", 
                                   "Information searching on the internet", 
                                   "Other specified communication by computing", 
                                   "Other specified computing", 
                                   "Other specified information by computing", 
                                   "Unspecified internet use", 
                                   "Unspecified communication by computer", 
                                   "Unspecified other computing"), 
        
        'r Civic' = c( "Meetings", 
                       "Volunteer work through an organisation", 
                       "Unspecified participatory activities" ), 
        
        's Religious' = c( "Religious activities"), 
        
        't Visiting/Socialising' = c("Other specified social life", 
                                     "Socialising with household members", 
                                     "Visiting and receiving visitors"), 
        
        'u TV/Radio' = c("Listening to sport on the radio", 
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
                         "Unspecified radio listening"), 
        
        'v Others' = c("Filling in the time use diary", 
                       "Illegible activity", 
                       "No main activity, no idea what it might be", 
                       "No main activity, some idea what it might be", 
                       "Unspecified time use")
  ) 


head(dtcseq)
dtcseq = as.matrix(dtcseq) 

dtcseq_r  = recodeList(dtcseq)
dtcseq_r

sort(table(dtcseq_r))

##################################################################
##################################################################

# Colors 
rc = c( brewer.pal(n = 7, name = 'Set2'), brewer.pal(n = 7, name = 'Set1'), brewer.pal(n = 8, name = 'Set3'))  
length(rc)

# Le temps 
timer = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
length(timer)
# 
colnames(dtcseq_r) <- c(timer)
# Def Seq 
dtcseq = seqdef(dtcseq_r, cpal = rc)

# dplot 
seqdplot(dtcseq, border = NA)

# Par Genre 
seqdplot(dtcseq[men, ], border = NA)
seqdplot(dtcseq[women,], border = NA)

##################################################################
##################################################################


