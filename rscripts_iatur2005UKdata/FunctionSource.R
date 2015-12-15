
################################################## ########################################
################################################## Func Time Mean #########################
################################################## ########################################

TimeLstdplyr = function(lstWd){
  
  lstWd = lstWd %>% 
    as.data.frame %>% 
    gather %>% 
    group_by(variable, value)%>% 
    summarise(N = n()) %>% 
    spread(variable, N) 
  
  lstWd = as.data.frame(lstWd) 
  lstWd[is.na(lstWd)] <- 0
  row.names(lstWd) <- lstWd$value
  lstWd = lstWd[,-1] # enleve le value column 
  lstWd = as.matrix(lstWd[-1,]) # Enleve the Valid Row 
  lstWd = as.data.frame(lstWd)
  lstWd$sum = apply(lstWd, 1, sum)
  
  lstWd = lstWd[sort.list(lstWd$sum), ]
  
  # lstWd = lstWd[sort.list(rownames(lstWd)), ]
  lstWd = lstWd[, -c(145)]
  lstWd = as.matrix(lstWd)
  
  lstWd = return(lstWd)   
}


TimeLstPlot = function(p, pcol, main = 'Time with Partner Activities', kol = "gray25", cexlegend = 1.2, left = -0.6){
  # layout(rbind(c(1,1,1,2))) 
  
  par(mar=c(1.1, 2.1, 2.1, 8.1), xpd=TRUE)
  
  m <- matrix(rep(seq(1:nrow(p)), each=144), ncol=144, nrow=nrow(p), byrow = TRUE)
  hist3D(z = p, space = 0.1, theta = 200, phi = 20, 
         border = kol, shade = 0.2, scale = T, col = pcol, add = F, 
         colvar = m, colkey = F, ticktype = "detailed", main = main) 
  # plot(0, type = 'n', axes = F, xlab = '', ylab = '')
  
  Mean_Time = as.data.frame(cp(apply(p, 1, sum), NULL))
  names(Mean_Time) <- 'prop'
  
  legend('topright',  inset=c(left,0), title = "% of Time", legend = paste(rev(Mean_Time$prop), rev(row.names(Mean_Time))) , horiz = F, cex = cexlegend,  bty = "n", pch=15, col = rev(pcol)) 
}


# 
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


TimelstSummary = function(lstWe, n = nrow(lstWe)){
  message('Pseudo-Function')
  
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
  z = length(table(mcWe))
  sqWe = seqdef(mcWe[,-1], cpal = rev(heat.colors(z)))
  
  dttWe = as.data.frame(cbind( as.character(dt_missWe[,1]), rowSums(dt_missWe[,-1])))
  dttWe$V2 = as.numeric(as.character(dttWe$V2))
  # 
  dttWe$V3 = round(prop.table(dttWe$V2, NULL), 4) * 100
  dttWe$V2 = round( (dttWe$V2 * 10) / n, 2) # mean sum by activities during weekend 
  sum(round( (dttWe$V2[-1] * 10) / n, 2) ) # mean sum by of missing during weekend 
  # 52 minutes ! 
  
  colnames(dttWe) <- c('Activities', 'Mean', 'Prop')
  dttWe = dttWe[-1, ]
  
  dttWe = dttWe %>% 
    summarise(Activities = 'SUM', Mean= sum(Mean), Prop = sum(Prop)) %>% 
    merge(dttWe, ., all = T)
  
  dta = list()
  dta$dtt = dttWe
  dta$sq = sqWe 
  return(dta)
}


RetrieveInfoCondMatrixR = function(mat1, mat2, hid){
  
  mat3 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 2:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i,j] <- 1 }
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i-1,j] <- 1 }
      
    }
  }
  
  mat4 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 1:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( mat3[i,j] == 1){
        mat4[i,j] = mat2[i,j] 
      } 
      
    }
  }
  
  return(mat4)
}


RetrieveInfoCondMatrixR_Complete = function(mat1, mat2, hid){
  
  # Retreive even the correct partner report 
  
  mat3 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 2:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( hid[i] == hid[i-1] & (mat1[i,j] != mat1[i-1,j]) ) 
      { mat3[i,j] <- 1 }
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i-1,j] <- 1 }
      
    }
  }
  
  for(i in 2:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( hid[i] == hid[i-1] & mat1[i,j] == 1 ) 
      { mat3[i,j] <- 1 }
      
      if( hid[i] == hid[i-1] & mat1[i-1,j] == 1 ) 
      { mat3[i-1,j] <- 1 }
      
    }
  }
  
  mat4 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 1:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( mat3[i,j] == 1){
        mat4[i,j] = mat2[i,j] 
      } 
      
    }
  }
  
  return(mat4)
}



RetrieveInfoCondMatrixR_suite = function(mat1, mat2, hid){
  
  mat3 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 2:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i,j] <- 1 }
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i-1,j] <- 1 }
      
    }
  }
  
  mat4 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 1:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( mat3[i,j] == 1){
        mat4[i,j] = mat2[i,j] 
      } 
      
    }
  }
  
  mat5 = matrix(0, nrow(mat4), ncol(mat4))  
  
  for(i in 1:nrow(mat5)){
    for(j in 1:ncol(mat5)){
      
      if( mat3[i,j] == 1){
        mat5[i,j] = paste( mat4[i,j], mat4[i-1,j]) 
      } 
    }
  }
  
  ws = seq(from = 2, to = nrow(mat5), by = 2)
  mat6 = mat5[ws, ]
  rownames(mat6) = unique(hid)
  
  return(mat6)
}


RetrieveInfoCondMatrixR_suiteAlter = function(mat1, mat2, hid){
  
  mat3 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 2:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i,j] <- 1 }
      
      if( hid[i] == hid[i-1] & mat1[i,j] != mat1[i-1,j] ) 
      { mat3[i-1,j] <- 1 }
      
    }
  }
  
  mat4 = matrix(0, nrow(mat1), ncol(mat1))  
  
  for(i in 1:nrow(mat1)){
    for(j in 1:ncol(mat1)){
      
      if( mat3[i,j] == 1){
        mat4[i,j] = mat2[i,j] 
      } 
      
    }
  }
  
  # Partn1  = seq(from = 1, to = nrow(mat4), by = 2)
  # Partn2  = seq(from = 2, to = nrow(mat4), by = 2)
  
  mat3W = mat3 [womenW, ]
  mat4W = mat4[womenW, ]
  mat4M = mat4[menW, ]
  
  mat5 = matrix(0, nrow(mat4W), ncol(mat4))  
  
  for(i in 1:nrow(mat5)){
    for(j in 1:ncol(mat5)){
      
      if( mat3W[i,j] == 1){
        mat5[i,j] = paste( mat4W[i,j], mat4M[i,j]) 
      } 
    }
  }
  
  # rownames(mat5) = unique(hid)
  
  return(mat5)
}

# Time Sum  ---------------------------------------------------------------
# Compute the individual sum for a time use category 

TimeSumAct= function(data, cat = 'a alone', int = 10){
  varSum = rowSums(ifelse( as.matrix( data ) == cat, 1, 0)) * int 
  return(varSum)
}


# dlm  --------------------------------------------------------------------
# Compute easily a set of regression for a given category  

dlm = function(data, cat = "a alone", df, formula, sum = F){
  message('varSum')
  df$varSum = rowSums(ifelse( as.matrix( data ) == cat, 1, 0)) * 10 
  lmm = lm(formula, data = df)
  # print(summary(lmm))
  sm = round(summary(lmm)$coefficients[,c(1,4)], 3)
  
  sm[,2] <- symnum(sm[,2], na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                   symbols = c("***", "**", "*", ".", " "))
  
  sm = as.data.frame(sm)
  colnames(sm) = c(cat, 'pvalue')
  
  conF = round(confint.lm(lmm), 3)
  conF = na.omit(conF)
  sm = cbind(sm, conF)

  # dt = as.data.frame( cbind(cat = 'R2', pavlue = round(summary(lmm)$r.squared, 2)) )
  # row.names(dt) <- '-'
  
  # colnames(dt) <- colnames(sm)
  # sm = rbind(sm, dt) 
  
  if(sum == T){
    lv = vector('list',2)
    lv[[1]] = as.data.frame(sm)
    lv[[2]] = df$varSum
    
    names(sm) <- c('sm', 'sum') 
  }
  
  print( cat("R2 = ", round(summary(lmm)$r.squared, 2) ))
  
  return(sm)
}


# Function Mean  ----------------------------------------------------------

TimeMeanDplyr = function(seqdf = SequenceWithWhomWeekdayz, 
                         dt = dtd, 
                         n = nrow(SequenceWithWhomWeekdayz), int = 10, children = F, 
                         variables = c("idno", "householdid.x", "children", "isex")){
  message('PseudoFunction')
  
  # Means by activities 
  dtableAct =
    data_frame(var = c( as.matrix(seqdf) ) ) %>% 
    group_by_("var") %>% 
    summarise( cases = n() * int)
               
    summarise( cases = n() * int, minutes = cases / n) 
  # as DF 
  Var = as.data.frame(as.matrix(seqdf))
  # Selecting the explanatory parameters 
  dta = cbind(Var, dt [, variables] )  
  # Long Format 
  dta = melt(dta, id = variables)
  # arrange 
  dta = arrange(dta, idno)
  
  if(children == T){
    # The code 
    datSum = dta %>% 
      group_by(isex, children) %>% 
      mutate(ng = n_distinct(idno) ) %>% 
      group_by(value, add=TRUE) %>%
      summarise(mean = 10*n()/ng[1] ) %>%
      spread(isex, mean) %>% 
      arrange(as.character(value))
    
    datSum1 = filter(datSum, children == 1)
    datSum1 = datSum1[,-1]
    
    datSum1 =
      datSum1 %>% 
      summarise(value = "z Sum", FEMALE = 1444, MALE = 1440) %>% 
      merge(datSum1, ., all = T) %>% 
      arrange(as.character(value))
    
    datSum0 = filter(datSum, children == 0)
    datSum0 = datSum0[,-1]
    
    datSum0 =
      datSum0 %>% 
      summarise(value = "z Sum", FEMALE = 1444, MALE = 1440) %>% 
      merge(datSum0, ., all = T) %>% 
      arrange(as.character(value))
    
    datSum = list()
    datSum$nochild <- datSum0
    datSum$child <- datSum1
  }
  
  if(children == F){
    # The code 
    datSum = dta %>% 
      group_by(isex) %>% 
      mutate(ng = n_distinct(idno) ) %>% 
      group_by(value, add=TRUE) %>%
      summarise(mean = 10*n()/ng[1] ) %>%
      spread(isex, mean) %>% 
      arrange(as.character(value))
    
    datSum =
      datSum %>% 
      summarise(value = "z Sum", FEMALE = 1444, MALE = 1440) %>% 
      merge(datSum, ., all = T) %>% 
      arrange(as.character(value))
  }
  
  return(datSum)
}

# smn = 10*n() # for the sum 
# ggplot(data=datSum, aes(x=isex, y=mean, fill =  value)) +
# geom_bar(stat="identity") + theme_minimal() + facet_grid(~value) + scale_fill_manual(values=rcAct) + ylim(c(0,200))

# Long to Wide 
# Transform time use data long to wide format 

TimeLongToWide = function(dta = weekend, id = 'idno', av = 'av', time = 'time'){
  dta$id = dta[, id]  
  dta$av = dta[, av] 
  dta$time = dta[, time] 
  
  dta = dta[, c('id', 'av', 'time')] 
  
  # Sequence 
  seqDay = dta[rep(1:nrow(dta), dta[,time] ), -3] %>%
    group_by(id) %>% 
    mutate( Time = 1:n() ) %>%
    spread(Time, av)
  
  seqDay = as.data.frame(seqDay)
  
  row.names(seqDay) <- seqDay[,'id']
  seqDay = seqDay [,-1]
  
  return(seqDay)
}

# seqWeekend = TimeLongToWide(dta = weekend)


# The mean function 
# Compute the mean of a long time use data 

TimeLongToWideMeanById = function(dta = weekend, id = 'idno', av = 'av', time = 'time', byAct = F){
  dta$id = weekend[, id]  
  dta$av = weekend[, av] 
  dta$time = weekend[, time] 
  
  dta = dta[, c('id', 'av', 'time')] 
  
  # Sequence 
  seqDay = dta[rep(1:nrow(dta), dta[,time] ), -3] %>%
    group_by(id) %>% 
    mutate( Time = 1:n() ) %>%
    dcast(id ~ av) 
  
  if(byAct == T){
    seqDay = apply(MeanAct[,-1], MARGIN = 2, FUN = mean) 
    print("The mean minutes by Activities: ")
  }
  
  return(seqDay)
}

# TimeLongToWideMeanById(dta = weekend, byAct = T)
