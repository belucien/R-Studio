rm(list = ls())
install.packages("pROC")
install.packages("rsq")
library(rsq)
library(pROC)
library("randomForest")
library(dplyr)
set.seed(42)
setwd("C:/Users/huiyang/OneDrive - Western University of Health Sciences/Desktop/Admission data")
dat = read.csv("Navle_Data.csv", head = T, stringsAsFactors = F)

dat1 = read.csv("Test_DataDVM2022.csv" , head = T, stringsAsFactors = F)
dat2 = read.csv("Test_DataDVM2023.csv" , head = T, stringsAsFactors = F)
# NAVLE, GPA, and admission data 
dat = read.csv("Navle_with_adm2020.csv", head = T, stringsAsFactors = F)
dat = na.omit(dat)
dat3 = read.csv("Navle_Data_with2021.csv", head = T, stringsAsFactors = F)
#newx = which(dat$Grad.Year==2020)
#new_dat = dat[newx,7]


dat[16:29] = lapply(dat[16:29], as.numeric)
dat_imputed = rfImpute(NAVLE.Score..First.Try. ~ .,data = dat2, iter = 6)
dat2_imputed = rfImpute(NAVLE.Score..First.Try. ~ SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+PreAdmissionGPA+PreAdmissionScienceGPA+GRE1_Revised.General.Verbal+GRE1_Revised.General.Quantitative+GRE1_Revised.General.Writing,data = dat2, iter = 6)
# convert all charcter variable into factor in one line
data_fac=dat2 %>% mutate_if(is.character, as.factor)
model_RF = randomForest(NAVLE.Score..First.Try. ~ SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA,data = dat,proximity = TRUE)
#Model1: Navle score vs. 4 semesters GPA
model1=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA,data=dat)
summary(model1)
#Model1: Navle score vs. 4 semesters GPA and admission data
model1_adm=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+PreAdmissionGPA+PreAdmissionScienceGPA+GRE1_Revised.General.Verbal+GRE1_Revised.General.Quantitative+GRE1_Revised.General.Writing,data=dat)
summary(model1_adm)

#Model1: Navle score vs. 4 semesters GPA + 12 comp and ACT 
model1=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+ACT+CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+CumulPharm+CumulPhys+CumulTox,data=dat)
summary(model1)

#replace charcter NA with NA
for (ii in 7:ncol(dat)) {
  ind = which(dat[,ii] == '#N/A')
  if (length(ind) > 0) {
    dat[ind,ii] = NA
    dat[,ii] = as.numeric(dat[,ii])
  }
}

dat1$SEM5FALLGPA=as.numeric(dat1$SEM5FALLGPA)


names = c("SEM5FALLGPA", "SEM6SPRINGGPA", "ACT",           "CumulAnatomy", 
 "CumulBehavior", "CumulEpidem",   "CumulGenetic",  "CumulImmun",    "CumulMicro",   
 "CumulNutri",    "CumulParasit",  "CumulPath",     "CumulPharm",    "CumulPhys",    
 "CumulTox"     )

dat1[,names] = as.numeric(unlist(dat1[,names]))
dat1[,9:ncol(dat1)] = 100*dat1[,9:ncol(dat1)]

dat$CumulAnatomy = as.numeric(dat$CumulAnatomy)

model1RF_adm=randomForest(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+PreAdmissionGPA+PreAdmissionScienceGPA+GRE1_Revised.General.Verbal+GRE1_Revised.General.Quantitative+GRE1_Revised.General.Writing, data = dat2_imputed,proximity = TRUE)
#model1A: Navle P/F vs. 4 semester GPA 
#0--Fail, 1--Pass
model1A=glm(First.Try.Pass.Fail~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA, family = binomial,data=dat)
summary(model1A)
yhat = predict(model1A, newdata = dat1, type='response')
plot(roc(dat$First.Try.Pass.Fail, yhat, direction="<"),
     col="blue",  main="ROC Curve of Logistic Regression Model")
roc(dat$First.Try.Pass.Fail, yhat, direction="<")

#model1A: Navle P/F vs. 6 semester GPA + 12 VBMS disciplines, and ACT scores
#0--Fail, 1--Pass
model1A=glm(First.Try.Pass.Fail~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+ACT+CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+CumulPharm+CumulPhys+CumulTox, family = binomial,data=dat)
summary(model1A)
yhat = predict(model1A, newdata = dat1, type='response')
plot(roc(dat1$First.Try.Pass.Fail, yhat, direction="<"),
     col="blue",  main="ROC Curve of Logistic Regression Model")
roc(dat$First.Try.Pass.Fail, yhat, direction="<")

#model1A1: Navle P/F vs. 4 semester GPA + 12 VBMS disciplines, and ACT scores
#0--Fail, 1--Pass
model1A1=glm(First.Try.Pass.Fail~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+ACT+CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+CumulPharm+CumulPhys+CumulTox, family = binomial,data=dat)
summary(model1A1)
yhat = predict(model1A1, newdata = dat2, type='response')
plot(roc(dat1$First.Try.Pass.Fail, yhat, direction="<"),
     col="blue",  main="ROC Curve of Logistic Regression Model")
roc(dat$First.Try.Pass.Fail, yhat, direction="<")

dat2_names = c("SEM3FALLGPA", "SEM4SPRINGGPA", "ACT", "CumulAnatomy",  "CumulBehavior", "CumulEpidem",  
               "CumulGenetic",  "CumulImmun",    "CumulMicro",    "CumulNutri",    "CumulParasit", 
                "CumulPath",     "CumulPharm",    "CumulPhys",     "CumulTox"     )
dat2[,dat2_names] = as.numeric(unlist(dat2[,dat2_names]))


dat2[,7:ncol(dat2)] = 100*dat2[,7:ncol(dat2)]


#Model2: Navle score vs. 5 semesters GPA
model2=lm(NAVLE.Score..First.Try.~ SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA,data=dat)
summary(model2)
# #Model2: Navle score vs. 5 semesters GPA and admission data
# model2_adm=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+PreAdmissionGPA+PreAdmissionScienceGPA+GRE1_Revised.General.Verbal+GRE1_Revised.General.Quantitative+GRE1_Revised.General.Writing,data=dat1)
# summary(model2_adm)

#Model2: Navle score vs. 5 semesters GPA + 12 comp and ACT
model2=lm(NAVLE.Score..First.Try.~ SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+ACT+CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+CumulPharm+CumulPhys+CumulTox,data=dat)
summary(model2)



#Model3: Navle score vs. 6 semesters GPA
model3=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA,data=dat)
summary(model3)

#Model3: Navle score vs. 6 semesters GPA and admission data
model3_adm=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+PreAdmissionGPA+PreAdmissionScienceGPA+GRE1_Revised.General.Verbal+GRE1_Revised.General.Quantitative+GRE1_Revised.General.Writing,data=dat1)
summary(model3_adm)

#Model3: Navle score vs. 6 semesters GPA + 12 comp and ACT
model3=lm(NAVLE.Score..First.Try.~SEM1FALLGPA+SEM2SPRINGGPA+SEM3FALLGPA+SEM4SPRINGGPA+SEM5FALLGPA+SEM6SPRINGGPA+ACT+CumulAnatomy+CumulBehavior+CumulEpidem+CumulGenetic+CumulImmun+CumulMicro+CumulNutri+CumulParasit+CumulPath+CumulPharm+CumulPhys+CumulTox,data=dat)
summary(model3)

#model4: Navle score vs. VBS Grade
dat$VBS_FinalGrade = as.factor(dat$VBS_Final_Score)
model4=lm(NAVLE.Score..First.Try.~ VBS_FinalGrade, data=dat)
summary(model4)

boxplot(NAVLE_SCORE ~ VBS_FinalGrade, col = c('lightgreen','skyblue1','darkorange2'))
VBS_FinalGrade= dat$VBS_Final_Score
NAVLE_SCORE= dat$NAVLE.Score..First.Try.


#model5:Navle P/F vs. VBS Grade
model5=glm(First.Try.Pass.Fail~VBS_FinalGrade, family = binomial,data=dat)
summary(model5)
yhat1 = predict(model5, newdata = dat, type='response')
plot(roc(dat$First.Try.Pass.Fail, yhat1, direction="<"),
     col="blue",  main="ROC Curve of Logistic Regression Model")
roc(dat$First.Try.Pass.Fail, yhat1, direction="<")


A_students=which(dat$VBS_Final_Score=="A")
B_students=which(dat$VBS_Final_Score=="B")
C_students=which(dat$VBS_Final_Score=="C")

dat2=dat[A_students,]
mean(dat2$NAVLE.Score..First.Try.)

model_predicted_Values = model1$fitted.values
NAVLE_Score=dat$NAVLE.Score..First.Try.
plot(NAVLE_Score,model_predicted_Values, col = 4, main = "R-squared = 0.51")
abline(0,1, col = 2)



#deleting missing values 
varnam = c('PreAdmissionGPA','PreAdmissionScienceGPA','GRE1_Revised.General.Verbal','GRE1_Revised.General.Quantitative','GRE1_Revised.General.Writing')
dat1 = na.omit(dat[,varnam])



model_predicted_Values = model2$fitted.values
NAVLE_Score=dat$NAVLE.Score..First.Try.
plot(NAVLE_Score,model_predicted_Values, col = 4, main = "R-squared = 0.55")
abline(0,1, col = 2)



model_predicted_Values = model3$fitted.values
NAVLE_Score=dat$NAVLE.Score..First.Try.
plot(NAVLE_Score,model_predicted_Values, col = 4, main = "R-squared = 0.59")
abline(0,1, col = 2)

# students who remodiated the VBMS course
t.test(dat$NAVLE.Score..First.Try. ~ dat$VBS_Remediated)


boxplot(dat$NAVLE.Score..First.Try. ~ dat$VBS_Remediated, main="P-value < 0.001", xaxt='n',  xlab = "VBMS_Remediated ", ylab = "NAVLE Score", col=c("lightblue", "orange"))
axis(1, at=1:2, labels=c('Non-remediated', 'Remediated'))



# predict the navle score of a new class
newPredict_con=predict(model3,newdata = dat1, interval = "confidence")
#DVM 2022
newPredict_pre=predict(model1A,newdata = dat1, interval = "prediction", type='response')
#DVM 2023
newPredict_pre_2023=predict(model1A1,newdata = dat2, interval = "prediction", type='response')
#newPredict1=predict(model1A,newdata = dat1,type='response')
View(newPredict)
View(newPredict_pre)

#DVM2022
write.csv(cbind.data.frame(dat1, Predicted_NavleScore = newPredict_pre), file = "TestData_DVM2022 with Navle Predicted.csv", row.names = F)
#DVM 2023
write.csv(cbind.data.frame(dat2, Prob_of_passing_NAVLE = newPredict_pre_2023), file = "TestData_DVM2023 with Navle Predicted.csv", row.names = F)
write.xlsx2(cbind.data.frame(dat1, Predicted_NavleScore = newPredict_con), file = "TestData_DVM2021 with Navle Predicted.xlsx", sheetName = "Confidence_interval", append = TRUE)
# Adding new sheet 
write.xlsx2(cbind.data.frame(dat1, Predicted_NavleScore = newPredict_pre), file = "TestData_DVM2021 with Navle Predicted.xlsx", sheetName = "Prediction_interval", append = TRUE)
plot(newPredict_pre_2023)
