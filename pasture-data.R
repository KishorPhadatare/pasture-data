df=read.csv("pasture-data.csv")
head(df)
tail(df)
str(df)

#Exploratory Analysis
pairs(df[2:5]) #Gives scatter plots
cor(df[2:5]) #Gives Correlation
summary(df[2:5])


#smooth scatter plots
scatter.smooth(x=df$X1,y=df$Y, main="Y ~ x1")
scatter.smooth(x=df$X2,y=df$Y, main="Y ~ x2")
scatter.smooth(x=df$X3,y=df$Y, main="Y ~ x3")

#To check outliers
par(mfrow=c(1, 4))
boxplot(df$X1, main="X1", sub=paste("Outlier rows: ", boxplot.stats(df$X1)$out))
boxplot(df$X2, main="X2", sub=paste("Outlier rows: ", boxplot.stats(df$X2)$out))
boxplot(df$X3, main="X3", sub=paste("Outlier rows: ", boxplot.stats(df$X3)$out))
boxplot(df$Y, main="Y", sub=paste("Outlier rows: ", boxplot.stats(df$Y)$out))

#To check the skewness
library(e1071)
par(mfrow=c(1, 4))  # divide graph area in 2 columns
plot(density(df$X1), main="Density Plot: X1", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(df$X1), col="red")
plot(density(df$X2), main="Density Plot: X2", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(df$X2), col="red")
plot(density(df$X3), main="Density Plot: X3", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(df$X3), col="red")
plot(density(df$Y), main="Density Plot: Y", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(df$Y), col="red")


cor(df[2:4], df$Y)

linearMod1=lm(Y ~ X1+X2+X3, data=df)
summary(linearMod1)
linearMod2=lm(Y~ X1+X2, data=df)
summary(linearMod2)
par(mfrow=c(2,2))
plot(linearMod2)
# Residuals vs fitted graph: We found equally spread residuals around a horizontal line without 
#distinct patterns, that is a good indication that we don't have non-linear relationships.

#Normal Q-Q graph: The residuals follow a straight line well & they dont deviate severely, then
# It is a good indication that we don't have non-linear relationships

#scale location graph: This plot shows if residuals are spread equally along the ranges of predictors.
#It helps to check  the assumption of equal variance (homoscedasticity).
#As there is horizontal line with equally (randomly) spread points, its good model.

#Residuals vs Leverage graph: In this model, there is no  influential case as we see cook's 
#distance line barely. It implies that all the cases are well inside of cook's distance.

#First we comapred P value in model1. But as P value for X3 variable was high, we removed it from 
#equation. Then we performed X1+ X2 analysis & we got small P values & R sqaured value=0.8481. 
#Therefore we decided this is the best model.

# STATISTIC	CRITERION TO SELECT THE BEST FIT MODEL
# R-Squared:	Higher the better (> 0.70)
# Adj R-Squared:	Higher the better
# F-Statistic:	Higher the better
# Std. Error:	Closer to zero the better
# t-statistic:	Should be greater 1.96 for p-value to be less than 0.05
# AIC:	Lower the better
# BIC:	Lower the better
# Mallows cp:	Should be close to the number of predictors in model
# MAPE (Mean absolute percentage error):	Lower the better
# MSE (Mean squared error):	Lower the better
# Min_Max Accuracy => mean(min(actual, predicted)/max(actual, predicted)): Higher the better

#Dividing data in Training & Test Data Set
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex= sample(1:nrow(df), 0.8*nrow(df))  # row indices for training data
trainData= df[trainingRowIndex, ]  # model training data
trainData
testData= df[-trainingRowIndex, ]
testData

#Training the model for trainData
linearMod3=lm(Y~ X1+X2, data=trainData)
summary(linearMod3)
par(mfrow=c(2,2))
plot(linearMod3)
# Residuals vs fitted graph: We found equally spread residuals around a horizontal line without 
#distinct patterns, that is a good indication that we don't have non-linear relationships.

#Normal Q-Q graph: The residuals follow a straight line well & they dont deviate severely, then
# It is a good indication that we don't have non-linear relationships

#scale location graph: This plot shows if residuals are spread equally along the ranges of predictors.
#It helps to check  the assumption of equal variance (homoscedasticity).
#As there is horizontal line with equally (randomly) spread points, its good model.

#Residuals vs Leverage graph: In this model, there is no  influential case as we see cook's 
#distance line barely. It implies that all the cases are well inside of cook's distance.

#First we comapred P value in model1. But as P value for X3 variable was high, we removed it from 
#equation. Then we performed X1+ X2 analysis & we got small P values & R sqaured value=0.8481. 
#Therefore we decided this is the best model.


#Predicting the model for testData
predTest= predict(linearMod3,testData)
head(predTest)

actualsPreds= data.frame(cbind(actuals=testData$X1, predicteds=predTest))
actualsPreds
correlationAccuracy=cor(actualsPreds)
correlationAccuracy

