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

VBL0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/VBL1Ly0.xlsx")

VBL <- VBL0[,-1]

summary(VBL)

# Checking the correlation between independent variables

RR <- cor(VBL, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(VBL))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity
VBLlm <- lm(y0 ~ x3+x4+x6+x7+x17+x19+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33, data = VBL)

summary(VBLlm)

# Checking the assumptions

vif(VBLlm)

plot(VBLlm$fitted.values, VBLlm$residuals)
VBLlm.stdres = rstandard(VBLlm)
plot(VBL$y0,VBLlm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(VBLlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(VBLlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VBLlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(VBLlm)

VBL_stepfit <- step(VBLlm)
summary(VBL_stepfit)

par(mfrow = c(2, 2))
plot(VBL_stepfit)

shapiro.test(VBL_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

VBL01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/VBL1Ly1.xlsx")

VBL1 <- VBL01[,-1]

summary(VBL1)

# Checking the correlation between independent variables

RR <- cor(VBL1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(VBL1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

VBL1lm <- lm(y1 ~ x7+x11+x14+x16+x17+x21+x23+x26+x27+x28+x29+x30+x31+x32+x33, data = VBL1)

summary(VBL1lm)

# Checking the assumptions

vif(VBL1lm)

plot(VBL1lm$fitted.values, VBL1lm$residuals)
VBL1lm.stdres = rstandard(VBL1lm)
plot(VBL1$y1,VBL1lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(VBL1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(VBL1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VBL1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(VBL1lm)

par(mfrow = c(2, 2))
plot(VBL1lm)


VBL1_stepfit <- step(VBL1lm)
summary(VBL1_stepfit)


shapiro.test(VBL1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

VBL02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/VBL1Ly2.xlsx")

VBL2 <- VBL02[,-1]

summary(VBL2)

# Checking the correlation between independent variables

RR <- cor(VBL2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(VBL2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

VBL2lm <- lm(y2 ~ x7+x11+x14+x16+x17+x21+x23+x26+x27+x28+x29+x30+x31+x32+x33, data = VBL2)

summary(VBL2lm)

# Checking the assumptions

vif(VBL2lm)

plot(VBL2lm$fitted.values, VBL2lm$residuals)
VBL2lm.stdres = rstandard(VBL2lm)
plot(VBL2$y2,VBL2lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(VBL2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(VBL2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VBL2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(VBL2lm)

par(mfrow = c(2, 2))
plot(VBL2lm)


VBL2_stepfit <- step(VBL2lm)
summary(VBL2_stepfit)

par(mfrow = c(2, 2))
plot(VBL2_stepfit)

shapiro.test(VBL2_stepfit$residuals)



#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

VBLgranger01 <- subset(VBL0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(VBLgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VBLgranger01.xlsx")

VBLgranger02 <- subset(VBL0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(VBLgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VBLgranger02.xlsx")

VBLgranger11 <- subset(VBL01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VBLgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VBLgranger11.xlsx")

VBLgranger12 <- subset(VBL01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(VBLgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VBLgranger12.xlsx")

VBLgranger21 <- subset(VBL02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VBLgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VBLgranger21.xlsx")

VBLgranger22 <- subset(VBL02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(VBLgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VBLgranger22.xlsx")




