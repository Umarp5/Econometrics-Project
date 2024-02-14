#
# My Project
#
# Installing Packages
install.packages("readxl")
install.packages("car")
install.packages("prais")
install.packages("sandwich")
install.packages("lmtest")
install.packages("cardata")
install.packages("AER")
install.packages("aTSA")
install.packages("moments")
#
# Loading Packages
library("AER")
library("car")
library("aTSA")
library("moments")
#
# Running CAPM regressions
reg1=lm(ERAAPL~MKT, data = my_data)
reg1w=lm(ERAAPL~MKT+WINDOW+MKT*WINDOW, data=my_data)#
#
reg2=lm(ERMETA~MKT, data = my_data)
reg2w=lm(ERMETA~MKT+WINDOW+MKT*WINDOW, data=my_data)#
#
reg3=lm(ERMSFT~MKT, data = my_data)
reg3w=lm(ERMSFT~MKT+WINDOW+MKT*WINDOW, data=my_data)#
#
reg4=lm(ERGOOG~MKT, data = my_data)
reg4w=lm(ERGOOG~MKT+WINDOW+MKT*WINDOW, data=my_data)#
#
reg5=lm(ERAMZN~MKT, data = my_data)
reg5w=lm(ERAMZN~MKT+WINDOW+MKT*WINDOW, data=my_data)#
#
summary(reg1w)
summary(reg2w)
summary(reg3w)
summary(reg4w)
summary(reg5w)
#
# conducting F tests
linearHypothesis(reg1w, c("WINDOW", "MKT:WINDOW")) 
linearHypothesis(reg2w, c("WINDOW", "MKT:WINDOW"))
linearHypothesis(reg3w, c("WINDOW", "MKT:WINDOW"))
linearHypothesis(reg4w, c("WINDOW", "MKT:WINDOW"))
linearHypothesis(reg5w, c("WINDOW", "MKT:WINDOW"))
#
# Obtaining residuals
reg1e=residuals(reg1w)
reg2e=residuals(reg2w)
reg3e=residuals(reg3w)
reg4e=residuals(reg4w)
reg5e=residuals(reg5w)
#
par(mfrow=c(2,3))
plot(reg1e, type="l", main="AAPL REG ERRORS",
                            xlab="",
                            ylab="",
                            col="red")
#
plot(reg2e, type="l", main="META REG ERRORS",
     xlab="",
     ylab="",
     col="blue")
#
plot(reg3e, type="l", main="MSFT REG ERRORS",
     xlab="",
     ylab="",
     col="orange")
#
plot(reg4e, type="l", main="GOOG REG ERRORS",
     xlab="",
     ylab="",
     col="green")
#
plot(reg1e, type="l", main="AMZN REG ERRORS",
     xlab="",
     ylab="",
     col="purple")
#
acf(reg1e, main="AAPL REG ERRORS", ylim=c(-0.2,0.2))
acf(reg2e, main="META REG ERRORS", ylim=c(-0.2,0.2))
acf(reg3e, main="MSFT REG ERRORS", ylim=c(-0.2,0.2))
acf(reg4e, main="GOOG REG ERRORS", ylim=c(-0.2,0.2))
acf(reg5e, main="AMZN REG ERRORS", ylim=c(-0.2,0.2))
#
hist(reg1e, breaks = 50, main="AAPL REG ERRORS")
hist(reg2e, breaks = 50, main="META REG ERRORS")
hist(reg3e, breaks = 50, main="MSFT REG ERRORS")
hist(reg4e, breaks = 50, main="GOOG REG ERRORS")
hist(reg5e, breaks = 50, main="AMZN REG ERRORS")
#
acf(reg1e, main="AAPL reg errors autocorrelation figure") # serial correlation
hist(reg1e, main="AAPL reg errors histogram", breaks=20,
     ylim=c(0,20), xlim=c(-20,20), density=20, col=2) # normality
plot(reg1e^2, main="AAPL reg sqrd errors (variance)", type="l",
     xlab="sq. errors", ylab="t", col=2)              # heteroscedasticity
#
acf(reg2e, main="META reg errors autocorrelation figure") # serial correlation
hist(reg2e, main="META reg errors histogram", breaks=20,
     ylim=c(0,20), xlim=c(-20,20), density=20, col=2) # normality
plot(reg2e^2, main="META reg sqrd errors (variance)", type="l",
     xlab="sq. errors", ylab="t", col=2)              # heteroscedasticity
#
acf(reg3e, main="MSFT reg errors autocorrelation figure") # serial correlation
hist(reg3e, main="MSFT reg errors histogram", breaks=20,
     ylim=c(0,20), xlim=c(-20,20), density=20, col=2) # normality
plot(reg3e^2, main="MSFT reg sqrd errors (variance)", type="l",
     xlab="sq. errors", ylab="t", col=2)              # heteroscedasticity
#
acf(reg4e, main="GOOG reg errors autocorrelation figure") # serial correlation
hist(reg4e, main="GOOG reg errors histogram", breaks=20,
     ylim=c(0,20), xlim=c(-20,20), density=20, col=2) # normality
plot(reg4e^2, main="GOOG reg sqrd errors (variance)", type="l",
     xlab="sq. errors", ylab="t", col=2)              # heteroscedasticity
#
acf(reg5e, main="AMZN reg errors autocorrelation figure") # serial correlation
hist(reg5e, main="AMZN reg errors histogram", breaks=20,
     ylim=c(0,20), xlim=c(-20,20), density=20, col=2) # normality
plot(reg5e^2, main="AMZN reg sqrd errors (variance)", type="l",
     xlab="sq. errors", ylab="t", col=2)              # heteroscedasticity
#
coeftest(reg1w, vcov = vcovHC, type="HC0")
coeftest(reg2w, vcov = vcovHC, type="HC0")
coeftest(reg3w, vcov = vcovHC, type="HC0")
coeftest(reg4w, vcov = vcovHC, type="HC0")
coeftest(reg5w, vcov = vcovHC, type="HC0")
#
durbinWatsonTest(reg1w)
durbinWatsonTest(reg2w)
durbinWatsonTest(reg3w)
durbinWatsonTest(reg4w)
durbinWatsonTest(reg5w)
#
# Conduct a stationarity test
adf.test(my_data$AAPL, nlag = NULL, output = TRUE)
adf.test(my_data$META, nlag = NULL, output = TRUE)
adf.test(my_data$MSFT, nlag = NULL, output = TRUE)
adf.test(my_data$GOOG, nlag = NULL, output = TRUE)
adf.test(my_data$AMZN, nlag = NULL, output = TRUE)
#


