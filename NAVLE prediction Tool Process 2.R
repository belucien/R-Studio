


rm(list = ls()); gc()
setwd("C:/Users/blucien/OneDrive - Western University of Health Sciences/Desktop/Admission data")
library(randomForest)

dat = read.csv("NAVLE.csv", head = T, stringsAsFactors = F)
fm = 'factor(First.Try.Pass.Fail)~SEM1FALLGPA+SEM2SPRINGGPA+
SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+ACT
CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+
CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+
CumulPharm+CumulPhys+CumulTox'

vars = c("First.Try.Pass.Fail", "SEM1FALLGPA", "SEM2SPRINGGPA", "SEM3FALLGPA", "SEM4SPRINGGPA", "SEM5FALLGPA", "SEM6SPRINGGPA", "ACT",
         "CumulAnatomy", "CumulBehavior", "CumulEpidem", "CumulGenetic", "CumulImmun", "CumulMicro", "CumulNutri", "CumulParasit", 
         "CumulPath", "CumulPharm", "CumulPhys", "CumulTox")
dat = na.omit(dat[, vars])

K = 100
n = nrow(dat)
ptrain = .8
n.train = round(n*ptrain); n.test = n - n.train
sensitivity.rf = specificity.rf = fpr.rf = fnr.rf = errRate.rf = rep(NA, K)
sensitivity.logit1 = specificity.logit1 = fpr.logit1 = fnr.logit1 = errRate.logit1 = rep(NA, K)
sensitivity.logit0 = specificity.logit0 = fpr.logit0 = fnr.logit0 = errRate.logit0 = rep(NA, K)
sensitivity.xgb = specificity.xgb = fpr.xgb = fnr.xgb = errRate.xgb = rep(NA, K)

ind1 = which(dat$First.Try.Pass.Fail == 1)
set.seed(1)
ind1 = ind1[sample(length(ind1), length(ind1))]
ind0 = which(dat$First.Try.Pass.Fail == 0)
ymat.xgb = ymat.rf = ymat.logit0 = ymat.logit1 = matrix(NA, n.test, K)


for (k in 1:K) {
  print(k)
  set.seed(k)
  ind11 = sample(ind1, n.train - round(length(ind0)*ptrain))
  ind00 = sample(ind0, round(length(ind0)*ptrain))
  ind.train = c(ind11, ind00)
  ind.test = setdiff(1:n, ind.train)
  
  #all variables used
  model0 = glm(First.Try.Pass.Fail ~ SEM1FALLGPA + SEM2SPRINGGPA + SEM3FALLGPA + 
                 SEM4SPRINGGPA + SEM5FALLGPA + SEM6SPRINGGPA + ACT + CumulAnatomy + 
                 CumulBehavior + CumulEpidem + CumulGenetic + CumulImmun + 
                 CumulMicro + CumulNutri + CumulParasit + CumulPath + CumulPharm + 
                 CumulPhys + CumulTox, data=dat[ind.train,], family = binomial)
  ymat.logit0[,k] = predict(model0, newdata = dat[ind.test,], type='response')
  
  # variables selected after feature selection 
  model1 = glm(First.Try.Pass.Fail ~ 
                 SEM3FALLGPA + SEM5FALLGPA + SEM6SPRINGGPA + CumulEpidem + CumulImmun + CumulNutri + 
                 CumulPath + CumulPharm + CumulTox, data=dat[ind.train,], family = binomial)
  ymat.logit1[,k] = predict(model1, newdata = dat[ind.test,], type='response')
  
  p0 = 1-0.1228733
  rf = randomForest(as.formula(fm), data = dat[ind.train, vars], cutoff = c(p0, 1-p0))
  
  ymat.rf[,k] = predict(rf, newdata = dat[ind.test, vars], type = 'prob')[,2]
 
}

yhat.rf = apply(ymat.rf > .82, 2, as.numeric)
yhat.logit0 = apply(ymat.logit0 > .9, 2, as.numeric)
yhat.logit1 = apply(ymat.logit1 > .9, 2, as.numeric)

library(pROC)
auc.logit0 = auc.logit1 = auc.rf = rep(NA, K)
#comparing the results of models 
for (k in 1:K) {
  set.seed(k)
  ind11 = sample(ind1, n.train - round(length(ind0)*ptrain))
  ind00 = sample(ind0, round(length(ind0)*ptrain))
  ind.train = c(ind11, ind00)
  ind.test = setdiff(1:n, ind.train)
  #RF
  tb = table(dat[ind.test,]$First.Try.Pass.Fail, yhat.rf[,k])
  sensitivity.rf[k] = tb[2,2] / sum(tb[2,])
  specificity.rf[k] = tb[1,1] / sum(tb[1,])
  fpr.rf[k] = tb[1,2] / sum(tb[1,])
  fnr.rf[k] = tb[2,1] / sum(tb[2,])
  errRate.rf[k] = (tb[1,2]+tb[2,1])/sum(tb[1:2,])
  auc.rf[k] = roc(dat[ind.test,]$First.Try.Pass.Fail, ymat.rf[,k])$auc
  #logit model with all variables 
  tb.logit = table(dat[ind.test,]$First.Try.Pass.Fail, yhat.logit0[,k])
  sensitivity.logit0[k] = tb.logit[2,2] / sum(tb.logit[2,])
  specificity.logit0[k] = tb.logit[1,1] / sum(tb.logit[1,])
  fpr.logit0[k] = tb.logit[1,2] / sum(tb.logit[1,])
  fnr.logit0[k] = tb.logit[2,1] / sum(tb.logit[2,])
  errRate.logit0[k] = (tb.logit[1,2]+tb.logit[2,1])/sum(tb.logit[1:2,])
  auc.logit0[k] = roc(dat[ind.test,]$First.Try.Pass.Fail, ymat.logit0[,k])$auc
  #logit model with variables selected 
  tb.logit = table(dat[ind.test,]$First.Try.Pass.Fail, yhat.logit1[,k])
  sensitivity.logit1[k] = tb.logit[2,2] / sum(tb.logit[2,])
  specificity.logit1[k] = tb.logit[1,1] / sum(tb.logit[1,])
  fpr.logit1[k] = tb.logit[1,2] / sum(tb.logit[1,])
  fnr.logit1[k] = tb.logit[2,1] / sum(tb.logit[2,])
  errRate.logit1[k] = (tb.logit[1,2]+tb.logit[2,1])/sum(tb.logit[1:2,])
  auc.logit1[k] = roc(dat[ind.test,]$First.Try.Pass.Fail, ymat.logit1[,k])$auc
  
  
}

out = rbind(c(mean(sensitivity.rf), mean(specificity.rf), mean(fpr.rf), mean(fnr.rf), mean(errRate.rf), mean(auc.rf)),
            c(mean(sensitivity.logit0), mean(specificity.logit0), mean(fpr.logit0), mean(fnr.logit0), mean(errRate.logit0), mean(auc.logit0)),
            c(mean(sensitivity.logit1), mean(specificity.logit1), mean(fpr.logit1), mean(fnr.logit1), mean(errRate.logit1), mean(auc.logit1)))
           
rownames(out) = c('RF', 'Logit0', 'Logit1')
colnames(out) = c('Sensitivity', 'Specificity', 'FPR', 'FNR', 'Error.Rate', 'AUC')
print(out)



out_1 = data.frame(sen = c(sensitivity.logit0, sensitivity.logit1), spe = c(specificity.logit0, specificity.logit1), 
                   fpr = c(fpr.logit0, fpr.logit1), fnr = c(fnr.logit0, fnr.logit1), ER = c(errRate.logit0, errRate.logit1), 
                   var.sel = c(rep(0, K), rep(1, K)))
boxplot(out_1$sen ~ out_1$var.sel, ylab = 'Sensitivity', xaxt='n',xlab = 'Variable selection or not')
axis(1, at=1:2, labels=c('Logit', 'Logit_VS'))
t.test(out_1$sen ~ out_1$var.sel)
boxplot(out_1$spe ~ out_1$var.sel, ylab = 'Specificity', xaxt='n',xlab = 'Variable selection or not')
axis(1, at=1:2, labels=c('Logit', 'Logit_VS'))
t.test(out_1$spe ~ out_1$var.sel)
boxplot(out_1$ER ~ out_1$var.sel, ylab = 'Error Rate', xaxt='n',xlab = 'Variable selection or not')
axis(1, at=1:2, labels=c('Logit', 'Logit_VS'))
t.test(out_1$ER ~ out_1$var.sel)

