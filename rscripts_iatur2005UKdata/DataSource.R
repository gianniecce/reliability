
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
# library(Rage)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
require(gridExtra)
library(plot3D)
library('gplots')
library(tidyr)
library('xtable')
library(pander)
library('plot3D') 
library('Rcpp')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Couple Original Data 
load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dataCIatur2015.RData') 
# Sequence Data in a list 
load("/Users/giacomovagni/ukavecqui2/data/dataAvecQui.RData") 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

n_distinct(dataC$householdid.x)
dataC = as.data.frame(dataC)

# Data Subset -------------------------------------------------------------
dtc = filter(dataC, day_rec == 'Weekend')
dtd = filter(dataC, day_rec == 'Weekdays')

############
### Weekends 
############
womenW = which(dtc$isex == 'FEMALE')
menW = which(dtc$isex == 'MALE')
#

womenNoChildW = which(dtc$isex == 'FEMALE' & dtc$numchild.x == 0)
womenChildW = which(dtc$isex == 'FEMALE' & dtc$numchild.x != 0)
#
menNoChildW = which(dtc$isex == 'MALE' & dtc$numchild.x == 0)
menChildW = which(dtc$isex == 'MALE' & dtc$numchild.x != 0)

############
### Weekdays
############
womenD = which(dtd$isex == 'FEMALE')
menD = which(dtd$isex == 'MALE')
# 
womenNoChildD = which(dtd$isex == 'FEMALE' & dtc$children == 0)
womenChildD = which(dtd$isex == 'FEMALE' & dtc$children == 1)
# 
menNoChildD = which(dtd$isex == 'MALE' & dtc$children == 0)
menChildD = which(dtd$isex == 'MALE' & dtc$children == 1)

########################
# Re-vectorize 
SequenceWithWhomWeekend <- data$SequenceWithWhomWeekend
SequenceWithWhomWeekdayz <- data$SequenceWithWhomWeekdayz
dtcseq <- data$dtcseq  
dtdseq <- data$dtdseq  
########################

# Colors  -----------------------------------------------------------------
rc = c(brewer.pal(n = 10, name = 'Set3'))  
# length(names(table(dtcseq)))
rcAct = c(brewer.pal(n = 8, name = 'Set3'), brewer.pal(n = 8, name = 'Set2'), brewer.pal(n = 5, name = 'Set1'))  
ActCol = list()
ActCol$act <- names(table(dtcseq))
ActCol$rcAct <- rcAct
