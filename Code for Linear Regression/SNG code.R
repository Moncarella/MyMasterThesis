SNG1fit <- lm(y1 ~ x1+x3+x4+x5+x6+x7+x9+x11+x12+x13+x14+x15+x20+x22+x24+x25+x27+x29+x31+x32+x33+x34, data=SNG1)

SNG2fit <- lm(y2 ~ x1+x3+x4+x5+x6+x7+x9+x11+x12+x13+x14+x15+x20+x22+x24+x25+x27+x29+x31+x32+x33+x34, data=SNG2)

library(corrplot)
library(Hmisc)
library(readxl)
library(lm.beta)
library(car)
library(lmtest)
library(PerformanceAnalytics)
library(ISLR)
library(dplyr)
library(writexl)

options(scipen=999)
options(max.print=1000000)


#_____________________________________RESULTS WITH Y0____________________________________________________#

SNG0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/SNG1Ly0.xlsx")

SNG <- SNG0[,-1]

summary(SNG)

# Checking the correlation between independent variables

RR <- cor(SNG, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(SNG))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

SNGlm <- lm(y0 ~ x1+x2+x3+x6+x7+x8+x9+x11+x13+x15+x16+x17+x19+x21+x22+x23+x26+x27+x28+x29+x31+x32+x33+x34, data = SNG)

summary(SNGlm)

# Checking the assumptions

vif(SNGlm)

plot(SNGlm$fitted.values, SNGlm$residuals)
SNGlm.stdres = rstandard(SNGlm)
plot(SNG$y0,SNGlm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(SNGlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(SNGlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(SNGlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(SNGlm)

SNG_stepfit <- step(SNGlm)
summary(SNG_stepfit)

par(mfrow = c(2, 2))
plot(SNG_stepfit)

shapiro.test(SNG_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

SNG01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/SNG1Ly1.xlsx")

SNG1 <- SNG01[,-1]

summary(SNG1)

# Checking the correlation between independent variables

RR <- cor(SNG1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(SNG1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

SNG1lm <- lm(y1 ~ x1+x3+x7+x8+x11+x13+x15+x17+x19+x21+x22+x23+x26+x27+x28+x31+x32+x33, data=SNG1)

summary(SNG1lm)

# Checking the assumptions

vif(SNG1lm)

plot(SNG1lm$fitted.values, SNG1lm$residuals)
SNG1lm.stdres = rstandard(SNG1lm)
plot(SNG1$y1,SNG1lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(SNG1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(SNG1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(SNG1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(SNG1lm)

par(mfrow = c(2, 2))
plot(SNG1lm)


SNG1_stepfit <- step(SNG1lm)
summary(SNG1_stepfit)


shapiro.test(SNG1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

SNG02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/SNG1Ly2.xlsx")

SNG2 <- SNG02[,-1]

summary(SNG2)

# Checking the correlation between independent variables

RR <- cor(SNG2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(SNG2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

SNG2lm <- lm(y2 ~ x1+x3+x4+x5+x6+x7+x9+x11+x12+x13+x14+x15+x20+x22+x24+x25+x27+x29+x31+x32+x33+x34, data = SNG2)

summary(SNG2lm)

# Checking the assumptions

vif(SNG2lm)

plot(SNG2lm$fitted.values, SNG2lm$residuals)
SNG2lm.stdres = rstandard(SNG2lm)
plot(SNG2$y2,SNG2lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(SNG2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(SNG2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(SNG2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(SNG2lm)

par(mfrow = c(2, 2))
plot(SNG2lm)


SNG2_stepfit <- step(SNG2lm)
summary(SNG2_stepfit)

par(mfrow = c(2, 2))
plot(SNG2_stepfit)

shapiro.test(SNG2_stepfit$residuals)


#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

SNGgranger01 <- subset(SNG0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(SNGgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/SNGgranger01.xlsx")

SNGgranger02 <- subset(SNG0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(SNGgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/SNGgranger02.xlsx")

SNGgranger11 <- subset(SNG01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(SNGgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/SNGgranger11.xlsx")

SNGgranger12 <- subset(SNG01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(SNGgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/SNGgranger12.xlsx")

SNGgranger21 <- subset(SNG02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(SNGgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/SNGgranger21.xlsx")

SNGgranger22 <- subset(SNG02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(SNGgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/SNGgranger22.xlsx")



