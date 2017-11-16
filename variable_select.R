library(Boruta)
library(dplyr)
traindata = hematemesis
traindata[traindata == ""] <- NA # convert all missing valus into NA

summary(traindata)


traindata = complete.cases(traindata) #complete cases only

# convert all categorical variable into 

df = traindata %>% select(-c(`Sr. no`,GBS.SCORE,ROCKALL.SCORE,PNED.SCORE,AIMS65,DEATH_7,DEATH_7_28))

df_complete = df[complete.cases(df),]

boruta.train <- Boruta(DEATH~., data = df_complete, doTrace = 2)
print(boruta.train)

summary(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")


lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

boruta.train1 = final.boruta
plot(boruta.train1, xlab = "", xaxt = "n")


lz<-lapply(1:ncol(boruta.train1$ImpHistory),function(i)
  boruta.train1$ImpHistory[is.finite(boruta.train1$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train1$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train1$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)

print(boruta.df)

# randomeforest

library(randomForest)
library(caret)
modelFit <- train( DEATH~.,data=df_complete, method="rf", importance = TRUE)  
varImp(modelFit)


# http://rstatistics.net/variable-importance-of-predictors-that-contribute-most-significantly-to-a-response-variable-in-r/

glm(outcome30~.)

cor(df_complete)

library(psych)

polychoric(df_complete)

tetrachoric(df_complete)


library(tilting)

# multiple t test R
# https://stackoverflow.com/questions/9661469/r-t-test-over-all-columns


library(MASS)

df_new = df_complete %>% select(-c(outcome90))

fit <- glm(DEATH~.,data=df_complete,family = binomial())
step <- stepAIC(fit, direction="both")
step$anova # display results 


library(leaps)

leaps<-regsubsets(outcome30~.,data=df_new,nbest=10 ,really.big = T)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")

library(relaimpo)


lmMod <- glm(outcome30~ . , data = df_new,family=binomial())  # fit lm() model

relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100

sort(relImportance$lmg, decreasing=TRUE) 


# ROC curve from logistic Regression



library(rms)

library(pROC)

fit2 = glm(DEATH~ASA +age+SEX+ Bicarbonate +ICTERUS + DOHTS + ALBUMIN+SHOCK+Etiology+INR+ascites,data=hematemesis, family=binomial())

summary(fit2)

display(fit2)
resRoc <- roc(hematemesis$DEATH ~ fit2$fitted)
plot(resRoc, legacy.axes = TRUE)
summary(resRoc)
resRoc$auc

hematemesis.dd = datadist(hematemesis)
options(datadist="hematemesis.dd")

hematemesis2.dd= datadist(hematemesis2)


options(datadist="hematemesis2.dd")

fit5 = lrm(DEATH~ASA +age+SEX+ Bicarbonate +ICTERUS + DOHTS + ALBUMIN+SHOCK+Cause+ascites,data=hematemesis)
           
rm(fit3)

summary(hematemesis$DEATH)

hematemesis2 = hematemesis

hematemesis2$DEATH= recode_factor(hematemesis2$DEATH,'0'='1','1'='0',levels=c("0","1"))
t.test(hematemesis$CREATNINE~hematemesis$DEATH)

summary(fit4)
          plot(anova(fit4)) 
          plot(nomogram(fit5))

summary(fit2)

cor.test(hematemesis$Bicarbonate,hematemesis$CREATNINE)

library(arm)

display(fit2)
display(fit3)

fit4 = glm(DEATH~ASA +age+SEX+ BUN+lactate+SBP+ DOHTS + ENDOSCOPIC_ETIOLOGY+INR+ascites,data=hematemesis2,family=binomial())

display(fit4)


hematemesis$s


save.image()


library(rms)

hem = hematemesis
rm(dd)
dd = datadist(hem)
options(datadist="dd")

ddd <- datadist(hem)
options( datadist = "ddd" )
fit12 = lrm(DEATH~ASA +age+SEX+ Bicarbonate +ICTERUS + DOHTS + ALBUMIN+SHOCK+Etiology+INR+ascites,data=hem,x=T,y=T)

summary(fit12)

fastbw(fit12)

fit12
plot(anova(fit12))

fit13 = lrm(DEATH~ASA +ICTERUS + DOHTS + ALBUMIN+RF,data=hem,x=T,y=T)
fit13

plot(nomogram(fit13))

fastbw(fit13)

t.test(hem$CREATNINE~hem$DEATH)

summary(hem$DEATH)

hem$RF = ifelse(hem$BUN >50 | hem$CREATNINE >1.2,1,0)


save.image(file="bleed.RData")



