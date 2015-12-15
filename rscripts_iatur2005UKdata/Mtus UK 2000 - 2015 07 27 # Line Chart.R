source("/Users/giacomovagni/ukavecqui2/rscripts/Mtus UK 2000 - 2015 07 23 # Source Bit 5.R")

# aaaa 
dta = as.data.frame((table(aaaa)[-1] * 10) / 806) 
colnames(dta) = c('freq')

dta$freq = round(dta$freq, 1)
dta$name = rownames(dta)

dta$nameFrom = gsub(pattern = '->.*', replacement = '', x = dta$name)
dta$nameTo = gsub(pattern = '.*->', replacement = '', x = dta$name)

row.names(dta) <- 1:nrow(dta) 
dta = dta[sort.list(dta$freq, decreasing = T), ]

dta$freqOri = dta$freq
# dta$freq[3] <- 20

dta$freq = 1:nrow(dta)
#dta$freq = as.array(log(dta$freq))

dta$freq = 1 - dta$freq

quartz()
par(mar = c(2, 25, 2, 25) ) 
par(bg = 'floralwhite')
plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '', main = "Women's report on men's report", family = 'Garamond')

for(i in 1:nrow(dta)){
  plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '')
  abline(h = dta$freq[i])
  axis(2, at = dta$freq[i], labels = dta$freqOri[i], las=2, tick = F, line = 12, cex.axis=1, family = 'Garamond')
  axis(2, at = dta$freq[i], labels = dta$nameFrom[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(4, at = dta$freq[i], labels = dta$nameTo[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(2, at = -10, labels = 'Mean Minutes', las=3, line = 15, tick = F, cex.axis=1, family = 'Garamond', cex.axis=2)
  par(new = T)
}


# bbbb 
dta = as.data.frame((table(bbbb)[-1] * 10) / 806) 
colnames(dta) = c('freq')

dta$freq = round(dta$freq, 1)
dta$name = rownames(dta)

dta$nameFrom = gsub(pattern = '->.*', replacement = '', x = dta$name)
dta$nameTo = gsub(pattern = '.*->', replacement = '', x = dta$name)

row.names(dta) <- 1:nrow(dta) 
dta = dta[sort.list(dta$freq, decreasing = T), ]

dta$freqOri = dta$freq
# dta$freq[3] <- 20

dta$freq = 1:nrow(dta)
#dta$freq = as.array(log(dta$freq))

dta$freq = 1 - dta$freq

quartz()
par(mar = c(2, 25, 2, 25) ) 
par(bg = 'floralwhite')
plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '', main = "Women's report on men's report", family = 'Garamond')

for(i in 1:nrow(dta)){
  plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '')
  abline(h = dta$freq[i])
  axis(2, at = dta$freq[i], labels = dta$freqOri[i], las=2, tick = F, line = 12, cex.axis=1, family = 'Garamond')
  axis(2, at = dta$freq[i], labels = dta$nameFrom[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(4, at = dta$freq[i], labels = dta$nameTo[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(2, at = -10, labels = 'Mean Minutes', las=3, line = 15, tick = F, cex.axis=1, family = 'Garamond', cex.axis=2)
  par(new = T)
}

# cccc 
dta = as.data.frame((table(cccc)[-1] * 10) / 806) 
colnames(dta) = c('freq')

dta$freq = round(dta$freq, 1)
dta$name = rownames(dta)

dta$nameFrom = gsub(pattern = '->.*', replacement = '', x = dta$name)
dta$nameTo = gsub(pattern = '.*->', replacement = '', x = dta$name)

row.names(dta) <- 1:nrow(dta) 
dta = dta[sort.list(dta$freq, decreasing = T), ]

dta$freqOri = dta$freq
# dta$freq[3] <- 20

dta$freq = 1:nrow(dta)
#dta$freq = as.array(log(dta$freq))

dta$freq = 1 - dta$freq

quartz()
par(mar = c(2, 25, 2, 25) ) 
par(bg = 'floralwhite')
plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '', main = "Women's report on men's report", family = 'Garamond')

for(i in 1:nrow(dta)){
  plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '')
  abline(h = dta$freq[i])
  axis(2, at = dta$freq[i], labels = dta$freqOri[i], las=2, tick = F, line = 12, cex.axis=1, family = 'Garamond')
  axis(2, at = dta$freq[i], labels = dta$nameFrom[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(4, at = dta$freq[i], labels = dta$nameTo[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(2, at = -10, labels = 'Mean Minutes', las=3, line = 15, tick = F, cex.axis=1, family = 'Garamond', cex.axis=2)
  par(new = T)
}

# dddd  
dta = as.data.frame((table(dddd)[-1] * 10) / 806) 
colnames(dta) = c('freq')

dta$freq = round(dta$freq, 1)
dta$name = rownames(dta)

dta$nameFrom = gsub(pattern = '->.*', replacement = '', x = dta$name)
dta$nameTo = gsub(pattern = '.*->', replacement = '', x = dta$name)

row.names(dta) <- 1:nrow(dta) 
dta = dta[sort.list(dta$freq, decreasing = T), ]

dta$freqOri = dta$freq
# dta$freq[3] <- 20

dta$freq = 1:nrow(dta)
#dta$freq = as.array(log(dta$freq))

dta$freq = 1 - dta$freq

quartz()
par(mar = c(2, 25, 2, 25) ) 
par(bg = 'floralwhite')
plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '', main = "Women's report on men's report", family = 'Garamond')

for(i in 1:nrow(dta)){
  plot(0, ylim = c( min(dta$freq), max(dta$freq)), bty = 'n', type = 'n', axes = F, ylab = '',  xlab = '')
  abline(h = dta$freq[i])
  axis(2, at = dta$freq[i], labels = dta$freqOri[i], las=2, tick = F, line = 12, cex.axis=1, family = 'Garamond')
  axis(2, at = dta$freq[i], labels = dta$nameFrom[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(4, at = dta$freq[i], labels = dta$nameTo[i], las=2, line = 0.5, tick = F, cex.axis=1, family = 'Garamond')
  axis(2, at = -10, labels = 'Mean Minutes', las=3, line = 15, tick = F, cex.axis=1, family = 'Garamond', cex.axis=2)
  par(new = T)
}






