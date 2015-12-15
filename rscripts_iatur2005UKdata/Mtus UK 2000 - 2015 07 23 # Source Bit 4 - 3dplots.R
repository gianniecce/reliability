
# Bit 4 - 3d plots 

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

lstWdSum$dtt

#TimelstSummary( lstWe [womenChildW, ], n = nrow(lstWe [womenChildW, ]))$dtt
#TimelstSummary( lstWe [womenNoChildW, ], n = nrow(lstWe [womenNoChildW, ]))$dtt
#TimelstSummary( lstWe [menChildW, ], n = nrow(lstWe [menChildW, ]))$dtt
#TimelstSummary( lstWe [menNoChildW, ], n = nrow(lstWe [menNoChildW, ]))$dtt

# Missings  
i_menWE = TimeLstdplyr(lstWd = lstWe [menW, ] )
i_menWEcolM = ActCol$rcAct[match(rownames(i_menWE), ActCol$act ) ] 
i_menWEcolM = na.omit(i_menWEcolM)

i_menWD = TimeLstdplyr(lstWd = lstWd [menW, ] )
i_menWDcolM = ActCol$rcAct[match(rownames(i_menWD), ActCol$act) ] 
i_menWDcolM = na.omit(i_menWDcolM)

i_womenWE = TimeLstdplyr(lstWd = lstWe [womenW, ] )
i_womenWEcolM = ActCol$rcAct[match(rownames(i_womenWE), ActCol$act ) ] 
i_womenWEcolM = na.omit(i_womenWEcolM)

i_womenWD = TimeLstdplyr(lstWd = lstWd [womenW, ] )
i_womenWDcolM = ActCol$rcAct[match(rownames(i_womenWD), ActCol$act) ] 
i_womenWDcolM = na.omit(i_womenWDcolM)

# plots 
par(mfrow = c(2,2))
# Men Weekend 
TimeLstPlot(p = i_menWE, pcol = i_menWEcolM, kol = NA, main = "Weekend - Men", cexlegend = 0.7, left = -0.4)
# Men Weekdays  
TimeLstPlot(p = i_menWD, pcol = i_menWDcolM, kol = NA, main = "Weekend - Women", cexlegend = 0.7, left = -0.4)
# Women Weekend 
TimeLstPlot(p = i_womenWE, pcol = i_womenWEcolM, kol = NA, main = "Weekday - Men", cexlegend = 0.7, left = -0.4)
# Women Weekdays  
TimeLstPlot(p = i_womenWD, pcol = i_womenWDcolM, kol = NA, main = "Weekday - Women", cexlegend = 0.7, left = -0.4)









# Not used anymore now 

############################################################### ############################################################### 
############################################################### Weekend #######################################################
############################################################### ############################################################### 

mat1_act = dtcseq 
mat2_whom = SequenceWithWhomWeekend

# "c child", "d nuclear"

TimeWhomAct = function(matrixCases = 'Valid', mat1_act, mat2_whom, vecWhom = c('a alone', 'b partner', 'h acquaintance'), whichRows = 1:nrow(mat1_act)){
  
  mat1_act = mat1_act[whichRows, ] 
  mat2_whom = mat2_whom[whichRows, ] 
  
  m1 <- matrix(matrixCases, ncol=ncol(mat1_act), nrow=nrow(mat1_act))
  lstst <- lapply(vecWhom, function(x) {
    m1[mat2_whom==x] <- mat1_act[mat2_whom==x]
    m1})
  names(lstst) <- gsub("[[:blank:]]", replacement = "", x = vecWhom)
  # print(names(lstst)) 
  return(lstst) 
}

#### #### #### #### #### #### #### #### 
#### menW - - - - MEN - - - - - - #####  
#### #### #### #### #### #### #### #### 

lstst = TimeWhomAct(matrixCases = 'Not', mat1_act = dtcseq, mat2_whom = SequenceWithWhomWeekend, whichRows = menW, vecWhom = c('a alone', 'b partner', 'h acquaintance') )
lstWd_alone = lstst$aalone
lstWd_partner = lstst$bpartner
lstWd_acquaintance = lstst$hacquaintance

# Alone 
aM = TimeLstdplyr(lstWd = lstWd_alone)
acolM = ActCol$rcAct[match(ActCol$act, rownames(aM)) ] 
acolM = na.omit(acolM)

# Partner 
pM = TimeLstdplyr(lstWd = lstWd_partner)
pcolM = ActCol$rcAct[match(ActCol$act, rownames(pM)) ] 
pcolM = na.omit(pcolM)

# Acquaintance 
hM = TimeLstdplyr(lstWd = lstWd_acquaintance)
hcolM = ActCol$rcAct[match(ActCol$act, rownames(hM)) ] 
hcolM = na.omit(hcolM) 

#### #### #### #### #### #### #### #### 
#### womenW - - - - WOMEN - - - - - - #####  
#### #### #### #### #### #### #### #### 

lstst = TimeWhomAct(matrixCases = 'Not', mat1_act = dtcseq, mat2_whom = SequenceWithWhomWeekend, whichRows = womenW)
lstWd_alone = lstst$aalone
lstWd_partner = lstst$bpartner
lstWd_acquaintance = lstst$hacquaintance

# Alone 
aF = TimeLstdplyr(lstWd = lstWd_alone)
acolF = ActCol$rcAct[match(ActCol$act, rownames(aF)) ] 
acolF = na.omit(acolF)

# Partner 
pF = TimeLstdplyr(lstWd = lstWd_partner)
pcolF = ActCol$rcAct[match(ActCol$act, rownames(pF)) ] 
pcolF = na.omit(pcolF)

# Acquaintance 
hF = TimeLstdplyr(lstWd = lstWd_acquaintance)
hcolF = ActCol$rcAct[match(ActCol$act, rownames(hF)) ] 
hcolF = na.omit(hcolF)

#### #### #### #### #### #### #### #### 
#### - - - - CHILDREN - - - - - - #####  
#### #### #### #### #### #### #### #### 

# # # Men 
lstst = TimeWhomAct(matrixCases = 'Not', vecWhom = c("c child", 'd nuclear'), mat1_act = dtcseq, mat2_whom = SequenceWithWhomWeekend, whichRows = menChildW)
lstWd_child = lstst$cchild
lstWd_nuclear = lstst$dnuclear

# Children 
cM = TimeLstdplyr(lstWd = lstWd_child)
ccolM = ActCol$rcAct[match(ActCol$act, rownames(cM)) ] 
ccolM = na.omit(ccolM)

# Nuclear  
dM = TimeLstdplyr(lstWd = lstWd_nuclear)
dcolM = ActCol$rcAct[match(ActCol$act, rownames(dM)) ] 
dcolM = na.omit(dcolM)

# # # Women  
lstst = TimeWhomAct(matrixCases = 'Not', vecWhom = c("c child", 'd nuclear'), mat1_act = dtcseq, mat2_whom = SequenceWithWhomWeekend, whichRows = womenChildW)
lstWd_child = lstst$cchild
lstWd_nuclear = lstst$dnuclear

# Children 
cF = TimeLstdplyr(lstWd = lstWd_child)
ccolF = ActCol$rcAct[match(ActCol$act, rownames(cF)) ] 
ccolF = na.omit(ccolF)

# Nuclear  
dF = TimeLstdplyr(lstWd = lstWd_nuclear)
dcolF = ActCol$rcAct[match(ActCol$act, rownames(dF)) ] 
dcolF = na.omit(dcolF)

####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

# ------# ------# ------# ------# ------# ------
# dtc 
levi = names(table(as.matrix(dtcseq)))
tab = vector('list', 21)

