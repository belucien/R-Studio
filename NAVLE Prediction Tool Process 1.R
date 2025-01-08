
rm(list = ls()); gc()
setwd("C:/Users/blucien/OneDrive - Western University of Health Sciences/Desktop/Admission data")
library(randomForest)
load("Navle Prediction Tool Workspace.RData")

fm = 'factor(First.Try.Pass.Fail)~SEM1FALLGPA+SEM2SPRINGGPA+
SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+
CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+
CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+
CumulPharm+CumulPhys+CumulTox'

vars = c("First.Try.Pass.Fail", "SEM1FALLGPA", "SEM2SPRINGGPA", "SEM3FALLGPA", "SEM4SPRINGGPA", "SEM5FALLGPA", "SEM6SPRINGGPA", "ACT",
         "CumulAnatomy", "CumulBehavior", "CumulEpidem", "CumulGenetic", "CumulImmun", "CumulMicro", "CumulNutri", "CumulParasit", 
         "CumulPath", "CumulPharm", "CumulPhys", "CumulTox")
dat = na.omit(dat[, vars])

K = 30
n = nrow(dat)
ptrain = .7
n.train = round(n*ptrain); n.test = n - n.train
sensitivity.rf = specificity.rf = fpr.rf = fnr.rf = errRate.rf = rep(NA, K)
sensitivity.logit = specificity.logit = fpr.logit = fnr.logit = errRate.logit = rep(NA, K)

ind1 = which(dat$First.Try.Pass.Fail == 1)
set.seed(1)
ind1 = ind1[sample(length(ind1), length(ind1))]
ind0 = which(dat$First.Try.Pass.Fail == 0)
ymat.rf = ymat.logit = matrix(NA, n.test, K)

sel.freq = matrix(0, 19, K)
rownames(sel.freq) = vars[-1]

#variable selection and model comparison
for (k in 1:K) {
  print(k)
  set.seed(k)
  ind11 = sample(ind1, n.train - round(length(ind0)*ptrain))
  ind00 = sample(ind0, round(length(ind0)*ptrain))
  ind.train = c(ind11, ind00)
  ind.test = setdiff(1:n, ind.train)
  
  model_logit_Test = glm(First.Try.Pass.Fail ~ SEM1FALLGPA + SEM2SPRINGGPA + SEM3FALLGPA + 
                           SEM4SPRINGGPA + SEM5FALLGPA + SEM6SPRINGGPA + ACT + CumulAnatomy + 
                           CumulBehavior + CumulEpidem + CumulGenetic + CumulImmun + 
                           CumulMicro + CumulNutri + CumulParasit + CumulPath + CumulPharm + 
                           CumulPhys + CumulTox, data=dat[ind.train,], family = binomial)
  #using forward variable selection 
  model0 = glm(First.Try.Pass.Fail ~ 1, data=dat[ind.train,], family = binomial)
  model.selected = step(model0, scope=list(lower=model0, upper=model_logit_Test), direction='forward')
  sel.freq[is.element(rownames(sel.freq), names(model.selected$coefficients[-1])),k] = 1
  
  ymat.logit[,k] = predict(model.selected, newdata = dat[ind.test,], type='response')
  
  p0 = 1-0.1228733
  rf = randomForest(as.formula(fm), data = dat[ind.train, vars], cutoff = c(p0, 1-p0))
  
  ymat.rf[,k] = predict(rf, newdata = dat[ind.test, vars], type = 'prob')[,2]
}

yhat.rf = apply(ymat.rf > .82, 2, as.numeric)
yhat.logit = apply(ymat.logit > .9, 2, as.numeric)

library(pROC)
auc.logit = auc.rf = rep(NA, K)
#evaluating the results of RF and logit models 
for (k in 1:K) {
  set.seed(k)
  ind11 = sample(ind1, n.train - round(length(ind0)*ptrain))
  ind00 = sample(ind0, round(length(ind0)*ptrain))
  ind.train = c(ind11, ind00)
  ind.test = setdiff(1:n, ind.train)
  
  tb = table(dat[ind.test,]$First.Try.Pass.Fail, yhat.rf[,k])
  sensitivity.rf[k] = tb[2,2] / sum(tb[2,])
  specificity.rf[k] = tb[1,1] / sum(tb[1,])
  fpr.rf[k] = tb[1,2] / sum(tb[1,])
  fnr.rf[k] = tb[2,1] / sum(tb[2,])
  errRate.rf[k] = (tb[1,2]+tb[2,1])/sum(tb[1:2,])
  auc.rf[k] = roc(dat[ind.test,]$First.Try.Pass.Fail, ymat.rf[,k])$auc
  
  tb.logit = table(dat[ind.test,]$First.Try.Pass.Fail, yhat.logit[,k])
  sensitivity.logit[k] = tb.logit[2,2] / sum(tb.logit[2,])
  specificity.logit[k] = tb.logit[1,1] / sum(tb.logit[1,])
  fpr.logit[k] = tb.logit[1,2] / sum(tb.logit[1,])
  fnr.logit[k] = tb.logit[2,1] / sum(tb.logit[2,])
  errRate.logit[k] = (tb.logit[1,2]+tb.logit[2,1])/sum(tb.logit[1:2,])
  auc.logit[k] = roc(dat[ind.test,]$First.Try.Pass.Fail, ymat.logit[,k])$auc
  
}

out = rbind(c(mean(sensitivity.rf), mean(specificity.rf), mean(fpr.rf), mean(fnr.rf), mean(errRate.rf), mean(auc.rf)),
            c(mean(sensitivity.logit), mean(specificity.logit), mean(fpr.logit), mean(fnr.logit), mean(errRate.logit), mean(auc.logit)))
rownames(out) = c('RF', 'Logit')
colnames(out) = c('Sensitivity', 'Specificity', 'FPR', 'FNR', 'Error.Rate', 'AUC')
print(out)

rowMeans(sel.freq)

out = data.frame(sen = c(sensitivity.rf, sensitivity.logit), spe = c(specificity.rf, specificity.logit), 
                 fpr = c(fpr.rf, fpr.logit), fnr = c(fnr.rf, fnr.logit), ER = c(errRate.rf, errRate.logit), RF = c(rep(1, K), rep(0, K)))
boxplot(out$sen ~ out$RF, ylab = 'Sensitivity', xlab = 'RF or not')
t.test(out$sen ~ out$RF)
boxplot(out$spe ~ out$RF, ylab = 'Specificity', xlab = 'RF or not')
t.test(out$spe ~ out$RF)
boxplot(out$ER ~ out$RF, ylab = 'Error Rate', xlab = 'RF or not')
t.test(out$ER ~ out$RF)