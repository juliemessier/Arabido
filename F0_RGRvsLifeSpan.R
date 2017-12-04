# (1) plot Lifespan-RGR trade-off in all F0s
# (2) plot Lifespan-RGR trade-off in selected F0s

# Plot Lifespan-RGR trade-off in all F0s####
#==========================================#

# Read F0 file 
F0<-read.csv("C:/Users/Julie/Desktop/Postdoc/Projet Arabido/F0_RGRvsLifeSpan.csv",dec='.',header=T)

# Plot RGR vs LifeSpan
plot(F0$T_repro,F0$RGRinf,type='n')
text(F0$T_repro,F0$RGRinf,labels=F0$accessionid,cex=0.7)

# Plot log(RGR) vs LifeSpan
plot(F0$T_repro,log(F0$RGRinf),type='n')
text(F0$T_repro,log(F0$RGRinf),labels=F0$accessionid,cex=0.7)

summary(lm(log(F0$RGRinf)~F0$T_repro,data=F0))
  # 
  # Coefficients:
  #               Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  5.1347877  0.0530486  96.794  < 2e-16 ***
  # F0$T_repro  -0.0054468  0.0007897  -6.897 1.68e-10 ***
  # ---
  # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 0.2081 on 140 degrees of freedom
  # Multiple R-squared:  0.2536,	Adjusted R-squared:  0.2483 
  # F-statistic: 47.57 on 1 and 140 DF,  p-value: 1.677e-10

# Plot log(RGR) vs log(LifeSpan)
plot(log(F0$T_repro),log(F0$RGRinf),type='n')
text(log(F0$T_repro),log(F0$RGRinf),labels=F0$accessionid,
     cex=0.6)

# What is the slope?
slope<-lm(log(F0$RGRinf)~log(F0$T_repro),data=F0)
summary(slope)
  # Coefficients:
  #                 Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)      6.48148    0.21867  29.640  < 2e-16 ***
  # log(F0$T_repro) -0.41305    0.05322  -7.762 1.59e-12 ***
  # ---
  # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # Residual standard error: 0.2014 on 140 degrees of freedom
  # Multiple R-squared:  0.3009,	Adjusted R-squared:  0.2959 
  # F-statistic: 60.24 on 1 and 140 DF,  p-value: 1.591e-12

  # slope = -0.41

# put RGR & T_Repro on same scale - 0 to 1
F0$rel.RGR<-F0$RGRinf/max(F0$RGRinf)
min(F0$rel.RGR) # 0.1664057
max(F0$rel.RGR) # 1

F0$rel.T_Repro<-F0$T_repro/max(F0$T_repro)
min(F0$rel.T_Repro) # 0.2254902
max(F0$rel.T_Repro) #1 

# Find points at upper half of slope by adding the two scores - correct for -0.41 slope (not a 1 slope)
F0$Ranking<-F0$rel.RGR+0.41*F0$rel.T_Repro
F0[,c('accessionid','Ranking')][order(F0$Ranking,decreasing = TRUE),]

dim(F0) #142   6

F0.sorted<-F0[order(F0$Ranking,decreasing = TRUE),]
F0.high.rank<-F0.sorted[1:71,]
dim(F0.high.rank) # 71 6

plot(log(F0$T_repro),log(F0$RGRinf),type='n')
text(log(F0$T_repro),log(F0$RGRinf),labels=F0$accessionid,
     cex=0.6)
text(log(F0.high.rank$T_repro),log(F0.high.rank$RGRinf),
     labels=F0.high.rank$accessionid,
     cex=0.6, col='red')

write.csv(F0.sorted, 
          file = "C:/Users/Julie/Desktop/Postdoc/Projet Arabido/Arabido/F0_RGRvsT_Repro_Sorted.csv")

# Plot Lifespan-RGR trade-off in selected F0s####
#===============================================#
# Read F0_sowing file 
F0.sowing<-read.csv("C:/Users/Julie/Desktop/Postdoc/Projet Arabido/Arabido/F0_Sowinglist.csv",dec='.',header=T)

names(F0.sowing)
dim(F0.sowing)
# 94 11

# Plot all F0s as background
plot(log(F0$T_repro),log(F0$RGRinf),type='n',main='94 Accessions selected for sowing')
text(log(F0$T_repro),log(F0$RGRinf),labels=F0$accessionid,
     cex=0.6)

# Plot sowing list F0s in red
text(log(F0.sowing$T_repro),log(F0.sowing$RGRinf),
     labels=F0.sowing$accessionid,
     cex=0.6, col='red',font=2)

text(log(F0.sowing[F0.sowing$accessionid==9769,'T_repro']),log(F0.sowing[F0.sowing$accessionid==9769,'RGRinf']),
     labels=F0.sowing[F0.sowing$accessionid==9769,'accessionid'],
     cex=0.6, col='purple',font=2)




 