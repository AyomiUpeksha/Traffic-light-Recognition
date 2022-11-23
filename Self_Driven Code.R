library(vars)
library(aTSA)
library(rugarch)
library(fUnitRoots)
library(tidyverse)

df1 <- read.csv("E:/1st-Sem/Time Series Analysis/PastPaper/Q1Data.csv")
head(df1)

plot(ts(df1))

adfTest(df1[,1],type = "nc")
adfTest(df1[,2],type = "nc")


coint.test(df1[,1], df1[,2], d =0, nlag = NULL, output = TRUE)


df2 <- read.csv("E:/1st-Sem/Time Series Analysis/PastPaper/Q2Data.csv")

head(df2)

mod1 <- arima(df2)

returnSeries <- ts(df2)
plot(returnSeries)

arch.test(mod1)


length(df2)
typeof(df2)
df2
trainingSet <- window(ts(df2),1,313)
testingSet <- window(ts(df2),314,368)
length(testingSet)

                                              

Ugarch_Spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
           mean.model = list(armaOrder = c(1, 1)), 
           distribution.model = "std")

 garch_Fit <- ugarchfit(spec = Ugarch_Spec, data = returnSeries, out.sample = 55)
 
 coef(garch_Fit)
 
 garch_Fit@fit$matcoef

print(garch_Fit)
