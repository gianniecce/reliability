library(plyr)
library(dplyr)

load(file = '/Users/giacomovagni/Documents/Data/TimeUse/UK/DataUKarticle/dataCIatur2015.RData') 
sourceCpp('/Users/giacomovagni/ukavecqui2/cppScript/MismatchCpp.cpp')

load('/Users/giacomovagni/Documents/Data/MTUS/Aggregate/UK/MtusUK2000.RData')

names(MtusUK2000) = paste(names(MtusUK2000), '_sim', sep = '')
MtusUK2000$idno_simple = paste(MtusUK2000$hldid, MtusUK2000$persid, sep = '') 

# 
dataC$hldid = as.numeric(dataC$sn1.x) * 1000 + as.numeric(dataC$sn2.x)
dataC$persid = as.numeric(dataC$sn3.x)
#

dataC$idno_simple = paste(dataC$hldid, dataC$persid, sep = '') 

# dtindividual$householdid = paste(dtindividual$SN1, dtindividual$SN2, sep = '')
# dtindividual$idno = paste(dtindividual$SN1, dtindividual$SN2, dtindividual$SN3, sep = '')

t = arrange(MtusUK2000, idno_simple)
t = t[1:100, ]
d = arrange(dataC, idno_simple)
d = dataC[1:100, ]

t[t$idno_simple %in% d$idno_simple, ] 

mtsim = MtusUK2000[MtusUK2000$idno_simple %in% dataC$idno_simple, ] 
mtsim = arrange(mtsim, idno_simple)
n_distinct(mtsim$idno)
n_distinct(mtsim$hldid_sim)

head(mtsim)

dt = dataC[,c("householdid.x", "idno_simple", 'idno', 'numchild.x', "day_rec", "isex")]
mt = mtsim[,c('hldid_sim', 'persid_sim', "id_sim", 'nchild_sim', 'idno_simple', "day_sim", "diary_sim", "sex_sim")]

dt = arrange(dt, idno_simple)
mt = arrange(mt, idno_simple)

dt = dt[1:20, ]
mt = mt[1:20, ]


dt$indx <-with(dt, ave(1:nrow(dt), idno_simple, FUN=seq_along))
mt$indx <-with(mt, ave(1:nrow(mt), idno_simple, FUN=seq_along))
suck = merge(dt, mt, by = c('idno_simple', 'indx'))
suck

dput(dt)
dput(mt)

head(mt)
head(dt)

mdt  = merge(dt, mt, by = 'idno_simple')
head(mdt)

dataCc = merge(dataC, MtusUK2000, by = 'idno_simple')
n_distinct(dataCc$idno)
n_distinct(dataCc$idno_simple)

table(dataCc$numchild.x, dataCc$nchild_sim) 

mtsim[1:10, 1:10]
dataC[1:10, 1:10]




