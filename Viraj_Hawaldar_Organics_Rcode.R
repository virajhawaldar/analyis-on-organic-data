setwd("C:/Users/HP/Desktop/Study documents")
getwd()
library(Hmisc)
library(rpart)
library(rpart.plot)
#library(rattle)
library(plyr)
library(readxl)
organics <- read_xlsx("C:/Users/viraj/Desktop/Study documents/database marketing/organics.xlsx")
row.names(organics) <- organics[,1]
organics <- organics[, -4]
organics <- organics[, -12]
organics <- organics[, -1]
organics$ID <- NULL
View(organics)
#imputing the missing values 
demaflmodel <- lm(DemAffl ~ DemAge + DemClusterGroup + DemGender + DemReg + DemTVReg + PromClass + PromSpend + PromTime + TargetBuy,data = organics)
summary(demaflmodel)
demaflmodel <- lm(DemAffl ~ DemAge + DemClusterGroup + factor(as.character(DemTVReg,DemGender,DemClusterGroup), exclude = c('Ulster','U','M','F','A','B','C','D','E')), data = organics)
summary(demaflmodel)
demagemodel <- lm( DemAge~ DemAffl + DemClusterGroup + DemGender + DemReg + DemTVReg + PromClass + PromSpend + PromTime + TargetBuy,data = organics)
summary(demagemodel)
demagemodel <- lm(DemAge ~ DemAffl + DemClusterGroup + factor(as.character(DemTVReg,DemGender,DemClusterGroup), exclude = c('Ulster','U','M','F','A','B','C','D','E')), data = organics)
summary(demagemodel)
sum(is.na(organics$DemAffl))
sum(is.na(organics$DemAge))
for(i in 1:nrow(organics)){
  if(is.na(organics[i, "DemAffl"])){
    organics[i, "DemAffl"] = predict(demaflmodel, newdata = organics[i, ])
  }
}
describe(organics$DemAffl)

for(i in 1:nrow(organics)){
  if(is.na(organics[i, "DemAge"])){
    organics[i, "DemAge"] = predict(demagemodel, newdata = organics[i, ])
  }
}
describe(organics$DemAge)

View(organics)
head(organics)
rand <- runif(nrow(organics))
organicsrand<-organics[order(rand),]
head(organicsrand)
organicstrain<-organicsrand[1:11111,]
organicstest<-organicsrand[11112:22223,]
count(organics,'TargetBuy')
hist(organics$TargetBuy)
set.seed(42)
organicstree <- rpart(TargetBuy ~ ., data = organicstrain, method = "class")
organicstree
rpart.plot(organicstree)
#creating a confusion matrix for training data set
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(model))))
  writeLines(paste("Target:", deparse(substitute(target))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = target, Predicted = dataset$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  dataset
}
organicstrain <- testModelPerformance(organicstree, organicstrain, organicstrain$TargetBuy)
#running the classification model on test dataset
organicstree <- rpart(TargetBuy ~ ., data = organicstest, method = "class")
organicstree
rpart.plot(organicstree)
#creating confusion matrix for test data set
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(model))))
  writeLines(paste("Target:", deparse(substitute(target))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = target, Predicted = dataset$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  dataset
}
organicstest <- testModelPerformance(organicstree, organicstest, organicstest$TargetBuy)
organicstrain$pred <- predict(organicstree, organicstrain, type = "class") #create a prediction using our tree
table(Actual = organicstrain$TargetBuy, Predicted = organicstrain$pred) #create a confusion matrix

organicstrain$correct <- organicstrain$TargetBuy == organicstrain$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
traincorrectcount <- length(which(organicstrain$correct))
trainincorrectcount <- nrow(organicstrain) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow(organicstrain)
trainaccuracy <- 1-trainerrorrate

#Now look at test
organicstest$pred <- predict(organicstree, organicstest, type = "class") #create a prediction using our tree
table(Actual = organicstest$TargetBuy, Predicted = organicstest$pred) #create a confusion matrix

organicstest$correct <- organicstest$TargetBuy== organicstest$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
testcorrectcount <- length(which(organicstest$correct))
testincorrectcount <- nrow(organicstest) - testcorrectcount
testerrorrate <- testincorrectcount/nrow(organicstest)
testaccuracy <- 1-testerrorrate

#Compare
paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")

#Applying logistic regression 
organics$treepred <- NULL
organics$treepredcorrect <- NULL

organicslogit <- glm(TargetBuy ~ DemAffl + DemAge + DemClusterGroup + DemGender + DemReg + DemTVReg + PromClass + PromSpend + PromTime, data = organics, family = binomial(link = "logit"))
summary(organicslogit)

#Applying logistic regression on training 
head(organics)
organicstrain$treepred <- NULL
organicstrain$treepredcorrect <- NULL

organicslogittrain <- glm(TargetBuy ~ DemAffl + DemAge + DemClusterGroup + DemGender + DemReg + DemTVReg + PromClass + PromSpend + PromTime, data = organicstrain, family = binomial(link = "logit"))
summary(organicslogittrain)

confint.default(organicslogit) #Build confidence intervals
exp(coef(organicslogit)) #Calculate odd sratio

#Applying logistic regression on test 
organicstest$treepred <- NULL
organicstest$treepredcorrect <- NULL

organicslogittest <- glm(TargetBuy ~ DemAffl + DemAge + DemClusterGroup + DemGender + DemReg + DemTVReg + PromClass + PromSpend + PromTime, data = organicstest, family = binomial(link = "logit"))
summary(organicslogittest)

#Calculate Chi-Square
devdiff <- with(organicslogit, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(organicslogit, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

#calculate chi-square for training dataset
devdiff <- with(organicslogittrain, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(organicslogittrain, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

#calculating chi-square for test dataset
devdiff <- with(organicslogittest, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(organicslogittest, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

organicstrain$TargetBuy <- predict(organicslogittrain, newdata = organicstrain, type = "response")
resid.dev <- 2 * llh(organicstrain$TargetBuy, organicstrain$TargetBuy)
null.dev <- 2 * llh(organicstrain$TargetBuy, mean(organicstrain$TargetBuy))
pr2 <- 1-(resid.dev/null.dev)
paste("Psuedo R2: ", pr2)


