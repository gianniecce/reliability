# Bit 5
library('Rcpp')

source('/Users/giacomovagni/ukavecqui2/rscripts/DataSource.R')
source('/Users/giacomovagni/ukavecqui2/rscripts/FunctionSource.R')
sourceCpp('/Users/giacomovagni/ukavecqui2/cppScript/MismatchCpp.cpp')

############ 
# 1. Mismatch 
############ 

# Setting up 
Seq_WithWhomWeekend = SequenceWithWhomWeekend 
hid <- dtc$householdid.x
dtc$total = 0 
Seq_WithWhomWeekend$hid = as.character(hid)

# cpp 
dtc$mismatch = Mismatch(Am = as.matrix(Seq_WithWhomWeekend), hid = hid, Total = dtc$total) 

# quantile 
dtc$quantile = cut2(dtc$mismatch, g = 4)
dtc$quantile = factor(dtc$quantile, labels = c('0-25', '25-50', '50-75', '75-100'))

# first quantile 
a = which(dtc$quantile == '0-25')
aW = which(dtc$quantile == '0-25' & dtc$isex == 'FEMALE')
aM = which(dtc$quantile == '0-25' & dtc$isex == 'MALE')

# last quantile 
e = which(dtc$quantile == '75-100')
eW = which(dtc$quantile == '75-100' & dtc$isex == 'FEMALE')
eM = which(dtc$quantile == '75-100' & dtc$isex == 'MALE')

############ 
#### 2. Partner's comparison  
############ 

# SequenceWithWhomWeekend_sub = gsub( " .*$", "", as.matrix(SequenceWithWhomWeekend) )
SequenceWithWhomWeekend_sub = as.matrix(SequenceWithWhomWeekend) 

# Partner 
SekPartWe = ifelse(as.matrix(SequenceWithWhomWeekend) == 'b partner', 1, 0) 
SekPartWeDEF = seqdef(SekPartWe, cpal = c('white', 'orange'))

# Alone 
SekAloneWe = ifelse(as.matrix(SequenceWithWhomWeekend) == 'a alone', 1, 0) 
SekAloneWeDEF = seqdef(SekPartWe, cpal = c('white', 'orange'))

###### ######
###### Partner 
###### ###### 

SekPartWe = ifelse(as.matrix(SequenceWithWhomWeekend) == 'b partner', 1, 0) 
SekPartWeDEF = seqdef(SekPartWe, cpal = c('white', 'orange'))

SekPartWeE_complete = RetrieveInfoCondMatrixR_Complete(mat1 = SekPartWe[, ], mat2 = SequenceWithWhomWeekend_sub[, ], hid = dtc[, 'householdid.x'])

ssqqPartner = seqdef(SekPartWeE_complete)
# seqiplot(ssqqPartner[199:209, ], border = NA)

SekPartWeE_complete[womenW, 40:45]
SekPartWeE_complete[menW, 40:45]

# What WoMen Answer When Men say Partner 
cccc = matrix( paste(SekPartWeE_complete[womenW, ], SekPartWeE_complete[menW, ], sep = '->'), nrow = nrow(SekPartWeE_complete[womenW, ]) ) 

# What Women Answer When Men say Partner 
dddd = matrix( paste(SekPartWeE_complete[menW, ], SekPartWeE_complete[womenW, ], sep = '->'), nrow = nrow(SekPartWeE_complete[womenW, ]) ) 

table(cccc) * 10 / 804
table(dddd) * 10 / 804

# table(aaaa) * 10 / 804
# table(bbbb) * 10 / 804

# table(SekPartWe[womenW, ]) * 10 / nrow(SekPartWe[womenW, ])
# 9.0422886 + 115.4975124 + 3.2213930 + 13.4203980 + 1.3308458 + 0.3482587 + 4.1542289 + 5.3358209  +  6.6666667  + 18.2960199 

mean(TimeSumAct(SekPartWeE_complete, cat = 'b partner', int = 10)) 
bestdf = as.data.frame(cbind( table(cccc) * 10 / 804, table(dddd) * 10 / 804 )) 
bestdf$row = 1:nrow(bestdf)

sum(bestdf[3:12,1])
sum(bestdf[3:12,2])

( sum(bestdf[3:12,1]) + sum(bestdf[3:12,2]) ) / 2


#####
timer = c( TimeClock(seq(240, 1430, by = 10)), TimeClock(seq(0, 230, by = 10)) )
colnames(SekPartWeE_complete) = timer
sqf = seqdef(SekPartWeE_complete, cpal = c('white', "#8DD3C7", "#FFFFB3", "#FB8072", "#B3DE69", "#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5", "#BC80BD", "gainsboro")) 
sqf 
seqiplot(sqf[11:20,], border = NA, withlegend = F)

quartz()
seqlegend(sqf, bty = 'n')

## 
hid <- dtc$householdid.x

# mat1 : dummy partner matrix 
mat1 = SekPartWe

# mat 2 : full with whom mat 
mat2 = as.matrix(SequenceWithWhomWeekend)

mat3 = RetrieveInfoCondMatrixR(mat1 = mat1, mat2 = mat2, hid = hid)
mat4 = ifelse(mat3 == '0', '0', '1')

mat4seq = seqdef(mat4, cpal = c('white', 'firebrick1'))
# mat4seq2 = seqdef(mat4, cpal = c('white', 'grey'))

dtc$quantile_num = factor(dtc$quantile, labels = c('1', '2', '3', '4'))
dtc$quantile_num = as.numeric(dtc$quantile_num)
table(dtc$quantile_num, dtc$quantile)

# seqIplot(mat4seq, border = NA, sortv = dtc$quantile_num)

quartz() 
seqIplot(mat4seq, border = NA, withlegend = F, group = dtc$quantile, ytlab = F)
# seqIplot(mat4seq[which(dtc$quantile_num == 1), ], border = NA, withlegend = F, axes = F)
# par(new = T)
# seqdplot(mat4seq2[which(dtc$quantile_num == 1), ], border = NA, withlegend = F, axes = F)

mat3[1:10, 40:50]
mat1[1:10, 40:50]
dtcseq[1:10, 40:50]
matActMisPartner[1:10, 40:50]

matActMisPartner = RetrieveInfoCondMatrixR(mat1 = mat3, mat2 = dtcseq, hid = hid)
table(matActMisPartner) 

# What Men Answer When Men say Partner 
yyyy = matrix( paste(matActMisPartner[womenW, ], matActMisPartner[menW, ], sep = '->'), nrow = nrow(matActMisPartner[womenW, ]) ) 

# What Women Answer When Men say Partner 
zzzz = matrix( paste(matActMisPartner[menW, ], matActMisPartner[womenW, ], sep = '->'), nrow = nrow(matActMisPartner[womenW, ]) ) 

sort(table(yyyy)) 
sort(table(zzzz)) 

######

dtc$iage = as.numeric(as.character(dtc$iage)) 

quartz()
summary(glm(dtc$quantile == '75-100' ~ 
              dtc$child.x + 
              Min_Partner +
            + dtc$econact3_rec
            + dtc$married 
            + dtc$income_rec, family = 'binomial')) 

dtc$Min_Partner = TimeSumAct(as.matrix(SequenceWithWhomWeekend), cat = 'b partner', int = 10)
dtc$Min_Alone = TimeSumAct(as.matrix(SequenceWithWhomWeekend), cat = 'a alone', int = 10)
dtc$Min_Child = TimeSumAct(as.matrix(SequenceWithWhomWeekend), cat = 'c child', int = 10)
dtc$Min_Nuclear = TimeSumAct(as.matrix(SequenceWithWhomWeekend), cat = 'd nuclear', int = 10)

summary(lm(dtc$Min_Alone ~ dtc$child.x + dtc$isex + dtc$iage + dtc$quantile)) 
TimeSignLm(x = lm(dtc$Min_Alone ~ dtc$child.x + dtc$isex + dtc$iage))
confint(lm(dtc$Min_Alone ~ dtc$child.x + dtc$isex + dtc$iage))

# plots 

dtM = dtc %>% 
  group_by(quantile) %>% 
  summarise( Alone = mean(Min_Alone),
             Partner = mean(Min_Partner),
             Child = mean(Min_Child), 
             Nuclear = mean(Min_Nuclear))

dtM = as.data.frame(dtM)
rownames(dtM) = dtM$quantile

quartz()
par(bg = "floralwhite")

dotchart(as.matrix(dtM[,-c(1)]), pch = 15, border = NA, xlim = c(0,300), main = 'Co-presence Mean Minutes by Mismatch Quantiles', 
         family = 'Garamond',
         xlab = 'Mean Minutes', 
         cex = 1.2, 
         axes = F, 
         # col = c("red"), 
        col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), 
         lcolor = c("grey"), 
         gcolor = c('black')
)

# by gender 

dtM = dtc %>% 
  group_by(quantile, isex) %>% 
  summarise( Alone = mean(Min_Alone),
             Partner = mean(Min_Partner),
             Child = mean(Min_Child), 
             Nuclear = mean(Min_Nuclear))

dtM
dtM = as.data.frame(dtM)

dtMm = filter(dtM, isex == 'MALE')
dtMf = filter(dtM, isex == 'FEMALE')

row.names(dtMm) = dtMm$quantile
row.names(dtMf) = dtMf$quantile

quartz()
par(bg = "floralwhite")

dotchart(as.matrix(dtMm[,-c(1,2)]), pch = 15, border = NA, xlim = c(0,300), main = 'Co-presence Mean Minutes by Mismatch Quantiles', 
         family = 'Garamond',
         xlab = 'Mean Minutes', 
         cex = 1.2, 
         axes = F, 
         col = c("red"), 
         #col = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), 
         lcolor = c("grey"), 
         gcolor = c('black')
         )

par(new = T)

dotchart(as.matrix(dtMf[,-c(1,2)]), pch = 15, border = NA, xlim = c(0,300), 
         family = 'Garamond',
         xlab = 'Mean Minutes', 
         cex = 1.2, 
         axes = F, 
         col = c("blue"), 
         # lcolor = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), 
         gcolor = c('black')
)

legend('topright', legend = c('Men', 'Women'), pch = c(15), col = c('red', 'blue'), bty = 'n')


#



dttm = dtc %>% 
  group_by(quantile) %>% 
  summarise( Alone = mean(Min_Alone),
             Partner = mean(Min_Partner),
             Child = mean(Min_Child), 
             Nuclear = mean(Min_Nuclear))

dttm = as.data.frame(dttm)
dttm = melt(dttm, id.vars = 'quantile')



###### ######
###### alone 
###### ###### 

SekAloneWe = ifelse(as.matrix(SequenceWithWhomWeekend) == 'a alone', 1, 0) 
SekAloneWeDEF = seqdef(SekPartWe, cpal = c('white', 'orange'))

# SekPartWeE = RetrieveInfoCondMatrixR(mat1 = SekPartWe[, ], mat2 = SequenceWithWhomWeekend_sub[, ], hid = dtc[, 'householdid.x'])
SekAloneWeE_complete = RetrieveInfoCondMatrixR_Complete(mat1 = SekPartWe[, ], mat2 = SequenceWithWhomWeekend_sub[, ], hid = dtc[, 'householdid.x'])

# What Men Answer When Men say Partner 
aaaa = matrix( paste(SekPartWeE_complete[womenW, ], SekPartWeE_complete[menW, ], sep = '->'), nrow = nrow(SekPartWeE_complete[womenW, ]) ) 

# What Women Answer When Men say Partner 
bbbb = matrix( paste(SekPartWeE_complete[menW, ], SekPartWeE_complete[womenW, ], sep = '->'), nrow = nrow(SekPartWeE_complete[womenW, ]) ) 

# table(aaaa) * 10 / 804
# table(bbbb) * 10 / 804

# table(SekPartWe[womenW, ]) * 10 / nrow(SekPartWe[womenW, ])
# table(SekPartWe[menW, ]) * 10 / nrow(SekPartWe[menW, ])

115 / 181
115 / 181


sort(table(cccc) * 10 / 804)
sort(table(dddd) * 10 / 804)
