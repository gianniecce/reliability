
# Bit 2 

source('/Users/giacomovagni/ukavecqui2/rscripts/DataSource.R')
source('/Users/giacomovagni/ukavecqui2/rscripts/FunctionSource.R')

##############################################################################################################
##############################################################################################################
##############################################################################################################

## Missing data and activities 

# Here we are going to calculate OLS for each activities for each with whom 
# dtc 
levi = names(table(as.matrix(dtcseq)))
tab = vector('list', 21)

# TimeSumAct(data = dtcseq, int = 10, cat = levi)
for(i in 1:21){
  tab[[i]] = TimeSumAct(data = dtcseq, int = 10, cat = levi[i])
}

names(tab) <- levi
activitiesSum = as.data.frame(tab)

alone_Mean = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "a alone")
partner_Mean = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "b partner")
child_Mean = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "c child")
miss = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "i True Missing")

# what is the activity when people are alone ? 
al = cbind(alone_Mean, activitiesSum)
summary(lm(alone_Mean ~., al))

# check with this 
# round(seqmeant(SequenceWithWhomWeekend), 3) * 10
# round(seqmeant(SequenceWithWhomWeekdayz), 3) * 10

########
# For all missing cases or with whom categories we are going to display what activities was performed 
# lst gives this results - first you plug what you are looking for - missing or activities 
# and it gives you the sequence of the activities 

########################## Week end ############
mat1_act = dtcseq
mat2_whom = SequenceWithWhomWeekend

m1 <- matrix('Valid Cases', ncol=ncol(mat1_act), nrow=nrow(mat1_act))
lst <- lapply(c('a alone', "i True Missing"), function(x) {
  m1[mat2_whom==x] <- mat1_act[mat2_whom==x]
  m1})
names(lst) <- c('alone', 'missing')

lstWe = lst$missing
seqMissWe = as.data.frame(lst$missing)

# 8 + 2 + 9 + 1 + 2 + 2 + 1 + 2 + 1 + 12 + 5 # 46 minutes missing 

########### The Sequence 

# names of activities in the sequence 
nmiss = names(table(as.matrix(seqMissWe)))

# match the colours 
colMiss = ActCol$rcAct[match( nmiss, ActCol$act) ] 
colMiss[1] <- "white"

# define the sequences of activities when missing 
seqMissingsWe = seqdef(seqMissWe, cpal = colMiss)

########################## Weekdays ############
# same 
mat1_act = dtdseq
mat2_whom = SequenceWithWhomWeekdayz

m1 <- matrix('Valid Cases', ncol=ncol(mat1_act), nrow=nrow(mat1_act))
lst <- lapply(c('a alone', "i True Missing"), function(x) {
  m1[mat2_whom==x] <- mat1_act[mat2_whom==x]
  m1})
names(lst) <- c('alone', 'missing')

lstWd = lst$missing
seqMissWd = as.data.frame(lst$missing)

# 8 + 2 + 9 + 1 + 2 + 2 + 1 + 2 + 1 + 12 + 5 # 46 minutes missing 

########### The Sequence 
nmiss = names(table(as.matrix(seqMissWd)))
colMiss = ActCol$rcAct[match( nmiss, ActCol$act) ] 
colMiss[1] <- "white"
seqMissingsWd = seqdef(seqMissWd, cpal = colMiss)

# seqdplot(seqMissingsWe, border = NA, ylim = c(0.9, 1) )
# seqdplot(seqMissingsWd, border = NA, ylim = c(0.9, 1) )

# Here we are computing the number of missing by episode 

require(tidyr)
# grouped
dt_missWe = lstWe %>% 
  as.data.frame %>% 
  gather %>% 
  group_by(variable, value)%>% 
  summarise(N = n()) %>% 
  spread(variable, N)

dt_missWe = as.data.frame(dt_missWe)
dt_missWe[is.na(dt_missWe)] <- 0
# head(dt_missWe)

# removing the valide cases on the first row 
mcWe = dt_missWe[-1, -1]
mcWe = as.matrix(mcWe)
row.names(mcWe) <- dt_missWe[-1, 1]

# max(mcWe)
# which(mcWe == max(mcWe), arr.ind = TRUE)

colnames(mcWe) = colnames(SequenceWithWhomWeekend)

# define sequence of it 
sqWe = seqdef(mcWe[,-1], cpal = rev(heat.colors(40)))

dttWe = as.data.frame(cbind( as.character(dt_missWe[,1]), rowSums(dt_missWe[,-1])))
dttWe$V2 = as.numeric(as.character(dttWe$V2))
# 
dttWe$V3 = round(prop.table(dttWe$V2, NULL), 4) * 100
dttWe$V2 = round( (dttWe$V2 * 10) / 1610, 2) # mean sum by activities during weekend 
sum(round( (dttWe$V2[-1] * 10) / 1610, 2) ) # mean sum by of missing during weekend 
# 52 minutes ! 

colnames(dttWe) <- c('Activities', 'Mean', 'Prop')
dttWe = dttWe[-1, ]

dttWe = dttWe %>% 
  summarise(Activities = 'SUM', Mean= sum(Mean), Prop = sum(Prop)) %>% 
  merge(dttWe, ., all = T)

## Weekend 
# grouped
dt_missWd = lstWd %>% 
  as.data.frame %>% 
  gather %>% 
  group_by(variable, value)%>% 
  summarise(N = n()) %>% 
  spread(variable, N)

dt_missWd = as.data.frame(dt_missWd)
dt_missWd[is.na(dt_missWd)] <- 0
head(dt_missWd)

dttWd = as.data.frame(cbind( as.character(dt_missWd[,1]), rowSums(dt_missWd[,-1])))
dttWd$V2 = as.numeric(as.character(dttWd$V2))
# 
dttWd$V3 = round(prop.table(dttWd$V2), 4) * 100
dttWd$V2 = round( (dttWd$V2 * 10) / 1610, 2) # mean sum by activities during weekend 
sum(round( (dttWd$V2[-1] * 10) / 1610, 2)) # mean sum by of missing during weekend 
# 44 minutes 

colnames(dttWd) <- c('Activities', 'Mean', 'Prop')
dttWd = dttWd[-1, ]

dttWd = dttWd %>% 
  summarise(Activities = 'SUM', Mean= sum(Mean), Prop = sum(Prop)) %>% 
  merge(dttWd, ., all = T)

mcWd = dt_missWd[-1, -1]
mcWd = as.matrix(mcWd)
row.names(mcWd) <- dt_missWd[-1, 1]

colnames(mcWd) = colnames(SequenceWithWhomWeekdayz)
sqWd = seqdef(mcWd[,-1], cpal = rev(heat.colors(42)))

cm.colors(42)

dttWd = as.data.frame(cbind( as.character(dt_missWd[,1]), rowSums(dt_missWd[,-1])))
dttWd$V2 = as.numeric(as.character(dttWd$V2))
# 
dttWd$V3 = round(prop.table(dttWd$V2, NULL), 4) * 100
dttWd$V2 = round( (dttWd$V2 * 10) / 1610, 2) # mean sum by activities during weekend 
sum(round( (dttWd$V2[-1] * 10) / 1610, 2) ) # mean sum by of missing during weekend 
# 44 minutes ! 

colnames(dttWd) <- c('Activities', 'Mean', 'Prop')
dttWd = dttWd[-1, ]

dttWd = dttWd %>% 
  summarise(Activities = 'SUM', Mean= sum(Mean), Prop = sum(Prop)) %>% 
  merge(dttWd, ., all = T)


