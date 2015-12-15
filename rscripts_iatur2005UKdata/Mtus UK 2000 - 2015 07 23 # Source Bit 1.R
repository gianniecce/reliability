
# Bit 1 

source('/Users/giacomovagni/ukavecqui2/rscripts/DataSource.R')
source('/Users/giacomovagni/ukavecqui2/rscripts/FunctionSource.R')

########################
# Seq - Act 
dtcsek = seqdef(dtcseq, cpal = rcAct)
dtdsek = seqdef(dtdseq, cpal = rcAct)
########################

# seqdplot(dtcsek, border = NA)
# seqdplot(dtdsek, border = NA)

########################
# Seq - Whom  
seqWhomWe = seqdef(SequenceWithWhomWeekend, cpal = rc)
seqWhomDa = seqdef(SequenceWithWhomWeekdayz, cpal = rc)
########################

# seqdplot(seqWhomDa, border = NA)
# seqdplot(seqWhomWe, border = NA)

################################################
# Mat 1 - 2 
mat1 = as.matrix(dtcseq)
mat2 = as.matrix(SequenceWithWhomWeekend)

mat3 = as.matrix(dtdseq)
mat4 = as.matrix(SequenceWithWhomWeekdayz)
################################################

################################################## ##################################
################################################## Recoding #########################
################################################## ################################## 

dataC$econact3_rec = droplevels(dataC$econact3.x)
dataC$iage_rec = cut(as.numeric(as.character(dataC$iage)), breaks = c(0, 30,40,100), labels = c("<30", "30-40", ">40"))
dataC$iethnic = ifelse(dataC$iethnic == "WHITE", 1, 0)
dataC$x.q22b2 = as.character(dataC$x.q22b2) 
dataC$x.q22b2[is.na(dataC$x.q22b2)] <- 'z NA'
dataC$x.q22b2 = factor(dataC$x.q22b2)

# dataC$hiqual4.x # edu harmo 
dataC$numchild_rec = cut(as.numeric(as.character(dataC$numchild.x)), breaks = c(-Inf,0,1,2,Inf), labels = c("0", "1", "2", ">2")) 
# Create dummy var children 
dataC$children = ifelse(dataC$numchild_rec == 0, 0, 1)
# marriage 
dataC$married = ifelse(dataC$livarr.x == 'Married  (& living with spouse)', 1, 0) 
# income 
dataC$income_na = ifelse(is.na(dataC$totpinc.x), 1, 0)

for(i in 1:length(levels(dataC$ageyngst.x))){
  dataC$ageyngst_rec[dataC$ageyngst.x == levels(dataC$ageyngst.x)[i] ] <- i-1
}

for(i in 1:length(levels(dataC$totpinc.x))){
  dataC$income[dataC$totpinc.x == levels(dataC$totpinc.x)[i] ] <- i-1
}

# en attendant 
dataC$income[is.na(dataC$income)] <- sample(x = c(2:5), size = 178, replace = T) 
# 
x = dataC$income
qnt <- quantile(x,seq(0,1,.25), na.rm = T)
cut2(x, g = 4)
dataC$income_rec = cut(x,unique(qnt),include.lowest=TRUE, labels = c("Low", "Middle", "Middle-High", "High")) 

# table(dataC$totpinc.x, dataC$income_rec)
# pq54 - british national 
# nssecb_3 
# q8d # shift work 
#  unemp2.x +
# dlm(formula = varSum ~ isex + numchild_rec + econact3_rec + income_rec + income_na + x.q22b2 + iage_rec + iethnic + married + gorpaf2.x,
# data = matWIT_check_seq, cat = "a alone", df = dataC) 
# dlm(formula = varSum ~ income_na, data = matWIT_check_seq, cat = "i True Missing", df = dataC) 

dataC$hiqual4.x = droplevels(dataC$hiqual4.x)
# levels(dataC$hiqual4.x)
table(dataC$hiqual4.x)
dataC$hiqual4.x = factor(dataC$hiqual4.x, levels = c("Degree level qualification or above", 
                                                     "Higher edn below degree level (eg HNC, nursing qual) ", 
                                                     "A levels, vocational level 3 & equivlnt (eg AS level, NVQ 3)", 
                                                     "O levels, GCSE grade A-C, vocational level 2 & equivlnt", 
                                                     "GCSE below grade C, CSE, vocational level 1 & equivlnt", 
                                                     "Qualification below GCSE/O level (eg trade apprenticeships) ", 
                                                     "Other qualification (incl professional, vocational, foreign)", 
                                                     "Qualifications - but DK which", 
                                                     "Qualifications - GCSE - but DK grade", 
                                                     "Qualifications - City & Guilds - DK level", 
                                                     "Qualifications - Other - but DK grade/level", 
                                                     "No qualifications"
))

as.data.frame(table(dataC$hiqual4.x)) 

dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[1]] <- "University"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[2]] <- "University"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[3]] <- "2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[4]] <- "2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[5]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[6]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[7]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[8]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[9]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[10]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[11]] <- "Part 2nd"
dataC$hiqual5[dataC$hiqual4.x == levels(dataC$hiqual4.x)[12]] <- "Primary"

table(dataC$hiqual5)
dataC$hiqual5 = factor(dataC$hiqual5, levels = c('University', '2nd', 'Part 2nd', 'Primary'))

dataC$sector = relevel(dataC$q5, 'PUBLIC SECTOR')
dataC$gorpaf2.x = relevel(dataC$gorpaf2.x, 'London & South East') 

# # # # # # # # # # # #
# # # # # # # # # # # #

dtc = filter(dataC, day_rec == 'Weekend')
dtd = filter(dataC, day_rec == 'Weekdays')

### Weekend 
# Women 
Row_WeekEndWomen = which(dtc$isex == 'FEMALE')
Row_WeekEndWomenChild = which(dtc$isex == 'FEMALE' & dtc$children == 1)
# Men 
Row_WeekEndMen = which(dtc$isex == 'MALE')
Row_WeekEndMenChild = which(dtc$isex == 'MALE' & dtc$children == 1)
### 

a = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomen, ], cat = 'a alone', df = dtc[Row_WeekEndWomen, ] ) 
b = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMen, ], cat = 'a alone', df = dtc[Row_WeekEndMen, ] ) 

c = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomen, ], cat = 'b partner', df = dtc[Row_WeekEndWomen, ] ) 
d = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMen, ], cat = 'b partner', df = dtc[Row_WeekEndMen, ] ) 

e = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomenChild, ], cat = 'c child', df = dtc[Row_WeekEndWomenChild, ] ) 
f = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMenChild, ], cat = 'c child', df = dtc[Row_WeekEndMenChild, ] ) 

g = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomenChild, ], cat = 'd nuclear', df = dtc[Row_WeekEndWomenChild, ] ) 
h = dlm(formula = varSum ~ iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMenChild, ], cat = 'd nuclear', df = dtc[Row_WeekEndMenChild, ] ) 

names(table(as.matrix(SequenceWithWhomWeekend)))
dtc$alone_Mean = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "a alone")
dtc$partner_Mean = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "b partner")
dtc$child_Mean = TimeSumAct(data = SequenceWithWhomWeekend, int = 10, cat = "c child")

i = dlm(formula = varSum ~ partner_Mean + child_Mean + iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomen, ], cat = "h acquaintance", df = dtc[Row_WeekEndWomen, ] ) 
j = dlm(formula = varSum ~ partner_Mean + child_Mean + iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMen, ], cat = "h acquaintance", df = dtc[Row_WeekEndMen, ] ) 

# alone_Mean + partner_Mean + child_Mean + 
# alone_Mean + partner_Mean + child_Mean +

k = dlm(formula = varSum ~ iage_rec + numchild_rec + married + econact3.x + sector + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomen, ], cat = "i True Missing", df = dtc[Row_WeekEndWomen, ] ) 

l = dlm(formula = varSum ~ iage_rec + numchild_rec + married + econact3.x + sector 
        + hiqual5 + income_rec + income_na 
        + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMen, ], cat = "i True Missing", df = dtc[Row_WeekEndMen, ] ) 

m = dlm(formula = varSum ~ isex + iage_rec + numchild_rec + married + hiqual5 + income_rec + income_na + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[, ], cat = "i True Missing", df = dtc[, ] ) 

control = select(dtc, isex, iage_rec, numchild_rec, married, econact3.x, sector, hiqual5, income_rec, income_na, iethnic, gorpaf2.x) 
control = apply(control, MARGIN = 2, as.character)

Variables_Description(control, group = 'isex')

sc = summary(control)
con = rbind(sc, c('Gender', 'Age', 'Number Child', 'Marriage', 'Education', 'Income', 'Inc Missing', 'Ethnicity', 'Region'))

# # 

load("/Users/giacomovagni/ukavecqui/data/family.RData") 

nrow(family) 
nrow(dataC)

copresence = seqdef(family, cpal = c("#8DD3C7", "#FFFFB3", "#FB8072", "#B3DE69", 
                        "#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5", # blues for acquaintances, 
                        "#BC80BD", "gainsboro")  ) 

seqdplot(copresence, border = NA, group = interaction(dataC$day_rec, dataC$child.x, dataC$isex))

ff = which(dataC$isex == 'FEMALE')
mm = which(dataC$isex == 'MALE')

ww = which(dataC$day_rec == 'Weekend')
wd = which(dataC$day_rec == 'Weekdays')

# # 

TimeTraMinutes = function(x = copresence[, ] ){
  a = as.data.frame( seqmeant(x) * 10 )
  a$act = row.names(a)
  return(a)
}

# Distribution of With Whom Time - Weekdays
TimeTraMinutes(copresence[ww, ])   
TimeTraMinutes(copresence[wd, ])   

table(dataC$econact3.x, dataC$isex)

dlm(formula = varSum ~ 
      iage_rec + 
      numchild_rec + 
      married + 
      econact3.x + 
      sector + 
      hiqual5 + 
      income_rec + 
      income_na + 
      day_rec + 
      gorpaf2.x, 
    data = family[mm, ], cat = "i True Missing", df = dataC[mm, ] ) 

dlm(formula = varSum ~ 
      iage_rec + 
      numchild_rec + 
      married + 
      econact3.x + 
      sector + 
      hiqual5 + 
      income_rec + 
      income_na + 
      day_rec + 
      gorpaf2.x, 
    data = family[ff, ], cat = "i True Missing", df = dataC[ff, ] ) 


