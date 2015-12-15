
# Bit 3

source('/Users/giacomovagni/ukavecqui2/rscripts/DataSource.R')
source('/Users/giacomovagni/ukavecqui2/rscripts/FunctionSource.R')
 
########
# For all missing cases or with whom categories we are going to display what activities was performed 
# lst gives this results - first you plug what you are looking for - missing or activities 
# and it gives you the sequence of the activities 
########

########################## Week end ############
mat1_actWE = dtcseq
mat1_actWD = dtdseq
mat2_whomWE = SequenceWithWhomWeekend
mat2_whomWD = SequenceWithWhomWeekdayz

# w/m Weekend  
lst = TimeWhomAct('Valid Cases', mat1_actWE, mat2_whomWE, vecWhom = c('a alone', "i True Missing")) 
lstWe = lst$iTrueMissing
lstWeSum = TimelstSummary(lstWe = lstWe) 
nrow(lstWeSum$sq) 

# w/m Weedays   
lst = TimeWhomAct('Valid Cases', mat1_actWD, mat2_whomWD, vecWhom = c('a alone', "i True Missing")) 
lstWd = lst$iTrueMissing
lstWdSum = TimelstSummary(lstWe = lstWd) 

lstWeSum$dtt
lstWdSum$dtt

seqIplot(lstWeSum$sq, withlegend = F, ylab = '', ytlab = F )
axis(2, at= 1:nrow(lstWeSum$sq) - 0.6, labels=rownames(lstWeSum$sq), las = 2, cex.axis = 0.7)

seqIplot(lstWdSum$sq, withlegend = F, ylab = '', ytlab = F )
axis(2, at= 1:nrow(lstWdSum$sq) - 0.6, labels=rownames(lstWdSum$sq), las = 2, cex.axis = 0.7)

# w/m Weekend  
lst = TimeWhomAct('Valid Cases', mat1_actWE, mat2_whomWE, vecWhom = c('a alone', "i True Missing")) 
lstWe = lst$iTrueMissing
lstWeSumWE = TimelstSummary(lstWe = lstWe[womenW, ]) 
lstWeSumME = TimelstSummary(lstWe = lstWe[menW, ]) 
nrow(lstWeSumWE$sq)
nrow(lstWeSumME$sq) 

as.data.frame(table(lstWe[menW, ]))
as.data.frame(table(lstWe[womenW, ]))

# w/m Weekdays  
lst = TimeWhomAct('Valid Cases', mat1_actWD, mat2_whomWD, vecWhom = c('a alone', "i True Missing")) 
lstWd = lst$iTrueMissing
lstWeSumWD = TimelstSummary(lstWe = lstWd[womenD, ]) 
lstWeSumMD = TimelstSummary(lstWe = lstWd[menD, ]) 

as.data.frame(table(lstWd[menD, ]))
as.data.frame(table(lstWd[womenD, ]))

# seqIplot(lstWeSum$sq, border = NA, withlegend = F, ylab=NA, ytlab=FALSE)
# lstWeSum$sq[sort.list(row.names(lstWeSum$sq)), ]



