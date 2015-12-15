
# Bit 1 


k = dlm(formula = varSum ~ iage_rec + numchild_rec + married + econact3.x + sector + 
          hiqual5 + income_rec + income_na 
        + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndWomen, ], cat = "i True Missing", df = dtc[Row_WeekEndWomen, ] ) 

l = dlm(formula = varSum ~ iage_rec + numchild_rec + married + econact3.x + sector 
        + hiqual5 + income_rec + income_na 
        + iethnic + gorpaf2.x, 
        data = SequenceWithWhomWeekend[Row_WeekEndMen, ], cat = "i True Missing", df = dtc[Row_WeekEndMen, ] ) 


k
l

plot()

library(mtusRlocal)
reg = read.csv('/Users/giacomovagni/ukavecqui2/doc/regression.csv')

reg$kolm = factor(reg$kolm, labels = c('grey', 'red'), levels = c('black', 'blue')) 
reg$kolf = factor(reg$kolf, labels = c('grey', 'red'), levels = c('black', 'blue')) 

# reg$variables = paste(reg$var, reg$cat)
reg$estm = as.numeric(as.character(reg$estm)) 
reg$estf = as.numeric(as.character(reg$estf)) 

quartz()
par(bg = 'floralwhite')
dotchart(reg$estm, labels=reg$cat, groups = reg$var, cex.axis = 0.6, 
         color = as.character(reg$kolm), 
         gcolor="black", 
         pch = 19, main = 'Women - OLS Estimates', family="Adobe Hebrew", xlab = 'Estimates Minutes') 
abline(v = 0)
legend('topleft', legend = 'R2 = 0.05', bty = 'n') 

par(bg = 'floralwhite')
dotchart(reg$estf, labels=reg$cat, groups = reg$var, cex.axis = 0.6, 
         color = as.character(reg$kolf), 
         gcolor="black", 
         pch = 19, main = 'Men - OLS Estimates', family="Adobe Hebrew", xlab = 'Estimates Minutes') 
abline(v = 0)
legend('topleft', legend = 'R2 = 0.04', bty = 'n') 





quartz()
par(bg = 'floralwhite')
dotchart(reg$estf, labels=reg$cat, groups = reg$var, 
         color = as.character(reg$kolf), 
         gcolor="black", 
         pch = 16, main = 'Women - OLS Estimates', family="Adobe Hebrew", xlab = 'Estimates Minutes', cex = 0.8) 
abline(v = 0)
legend('topleft', legend = 'R2 = 0.04', bty = 'n') 




sigSymbols <- symnum(pvals, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("***", "**", "*", ".", " "))
