# Bit 5 

library('Rcpp')

source('/Users/giacomovagni/ukavecqui2/rscripts/DataSource.R')
source('/Users/giacomovagni/ukavecqui2/rscripts/FunctionSource.R')
sourceCpp('/Users/giacomovagni/ukavecqui2/cppScript/MismatchCpp.cpp')

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

# mean by quantile 
mmiss = dtc %>% 
  group_by(quantile) %>% 
  summarise(mean(mismatch))

t(mmiss)

xtable(t(mmiss), digits = 2) 

# first quantile 
a = which(dtc$quantile == '0-25')
aW = which(dtc$quantile == '0-25' & dtc$isex == 'FEMALE')
aM = which(dtc$quantile == '0-25' & dtc$isex == 'MALE')

# last quantile 
e = which(dtc$quantile == '75-100')
eW = which(dtc$quantile == '75-100' & dtc$isex == 'FEMALE')
eM = which(dtc$quantile == '75-100' & dtc$isex == 'MALE')

#  

dtc$iage = as.numeric(as.character(dtc$iage))

summary(glm(dtc$quantile == '75-100' ~ 
              dtc$child.x + 
              dtc$iage + 
              , 
            subset = dtc$isex == 'FEMALE'))

summary(glm(dtc$quantile == '75-100' ~ dtc$child.x + 
              dtc$iage, 
            subset = dtc$isex == 'MALE'))

# # # #
t_partner = TimeSumAct(data = as.matrix(SequenceWithWhomWeekend), cat = 'b partner', int = 10)
mean(t_partner)

dtc$housework = rowSums(ifelse(dtcseq == 'f Housework', 1, 0))*10
mean(dtc$housework)

woman = which(dtc$isex == 'FEMALE')
man = which(dtc$isex == 'MALE')

menshare = round(dtc$housework[man] / (dtc$housework[man] + dtc$housework[woman]), 2)
aggregate(menshare ~ dtc$quantile[man], FUN = mean)

dtc$iage = as.numeric(as.character(dtc$iage))

summary(glm(dtc$quantile[man] == '75-100' ~ 
              dtc$children[man] + 
              dtc$iage[man] + 
              dtc$income_rec[man] + 
              dtc$hiqual5[man]) )


x = dataC$income
qnt <- quantile(x,seq(0,1,.25), na.rm = T)
cut2(x, g = 4)
dataC$income_rec = cut(x,unique(qnt),include.lowest=TRUE, labels = c("Low", "Middle", "Middle-High", "High")) 




t.test(dtc$housework ~ dtc$isex)



dtc$housework ~ dtc$isex

mean(t_partner[eW]) 
mean(t_partner[eM]) 

mean(t_partner[aW]) 
mean(t_partner[aM]) 

table(dtc$quantile)

quartz()
seqdplot(seqdef(SequenceWithWhomWeekend[e, ]), border = NA)
quartz()
seqdplot(seqdef(SequenceWithWhomWeekend[a, ]), border = NA)
quartz()
seqlegend(seqdef(SequenceWithWhomWeekend[e, ]) )


