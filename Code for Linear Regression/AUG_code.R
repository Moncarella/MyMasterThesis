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

AUG0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/AUG1Ly0.xlsx")

AUG <- AUG0[,-1]

summary(AUG)


# Checking the correlation between independent variables

RR <- cor(AUG, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(AUG))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

AUGlm <- lm(y0 ~ x2+x3+x6+x7+x8+x10+x12+x13+x14+x17+x19+x21+x22+x23+x24+x26+x27+x28+x29+x31+x32+x33+x34, data=AUG)
summary(AUGlm)

# Checking the assumptions

vif(AUGlm)

plot(AUGlm$fitted.values, AUGlm$residuals)
AUGlm.stdres = rstandard(AUGlm)
plot(AUG$y0,AUGlm$residuals,ylab="Standardized residuals",xlab="Auga group share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(AUGlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(AUGlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(AUGlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(AUGlm)

par(mfrow = c(2, 2))
plot(AUGlm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(AUGlm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- AUG[names_of_influential,]
AUG_without_outliers <- AUG %>% anti_join(outliers)


AUGfit_without_outliers <- lm(y0 ~ x2+x3+x6+x7+x8+x10+x12+x13+x14+x17+x19+x21+x22+x23+x24+x26+x27+x28+x29+x31+x32+x33+x34, data=AUG_without_outliers)
summary(AUGfit_without_outliers)

AUG_stepfit <- step(AUGfit_without_outliers)
summary(AUG_stepfit)

par(mfrow = c(2, 2))
plot(AUG_stepfit)

outlierTest(AUGlm)


#_____________________________________RESULTS WITH Y1____________________________________________________#


AUG01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/AUG1Ly1.xlsx")

AUG1 <- AUG01[,-1]

summary(AUG1)


# Checking the correlation between independent variables

RR <- cor(AUG1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(AUG1))
CC.p <- CC$P
corrplot(CC.p, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

AUG1lm <- lm(y1 ~ x1+x2+x3+x6+x7+x8+x10+x12+x13+x14+x17+x19+x21+x22+x23+x24+x26+x27+x28+x29+x31+x32+x33+x34, data = AUG1)

summary(AUG1lm)

# Checking the assumptions

vif(AUG1lm)

plot(AUG1lm$fitted.values, AUG1lm$residuals)
AUG1lm.stdres = rstandard(AUG1lm)
plot(AUG1$y1,AUG1lm$residuals,ylab="Standardized residuals",xlab="Auga group share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(AUG1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(AUG1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(AUG1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(AUG1lm)

par(mfrow = c(2, 2))
plot(AUG1lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(AUG1lm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- AUG1[names_of_influential,]
AUG1_without_outliers <- AUG1 %>% anti_join(outliers)


AUG1fit_without_outliers <- lm(y1 ~ x1+x2+x3+x6+x7+x8+x10+x12+x13+x14+x17+x19+x21+x22+x23+x24+x26+x27+x28+x29+x31+x32+x33+x34, data = AUG1_without_outliers)
summary(AUG1fit_without_outliers)

AUG1_stepfit <- step(AUG1fit_without_outliers)
summary(AUG1_stepfit)

par(mfrow = c(2, 2))
plot(AUG1_stepfit)

shapiro.test(AUG1_stepfit$residuals)
outlierTest(AUG1_stepfit)



#_____________________________________RESULTS WITH Y2____________________________________________________#

AUG02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/AUG1Ly2.xlsx")

AUG2 <- AUG02[,-1]

summary(AUG2)

# Checking the correlation between independent variables

RR <- cor(AUG2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(AUG2))
CC.p <- CC$P
corrplot(CC.p, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

AUG2lm <- lm(y2 ~ x2+x3+x6+x7+x8+x9+x10+x12+x13+x14+x17+x19+x21+x22+x23+x26+x27+x28+x29+x31+x32+x33+x34, data = AUG2)

summary(AUG2lm)

# Checking the assumptions

vif(AUG2lm)

plot(AUG2lm$fitted.values, AUG2lm$residuals)
AUG2lm.stdres = rstandard(AUG2lm)
plot(AUG2$y2,AUG2lm$residuals,ylab="Standardized residuals",xlab="Auga group share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(AUG2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(AUG2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(AUG2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(AUG2lm)

# There are no outliers

par(mfrow = c(2, 2))
plot(AUG2lm)


AUG2_stepfit <- step(AUG2lm)
summary(AUG2_stepfit)

par(mfrow = c(2, 2))
plot(AUG2_stepfit)

shapiro.test(AUG2_stepfit$residuals)


#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

AUGgranger01 <- subset(AUG0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(AUGgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/AUGgranger01.xlsx")

AUGgranger02 <- subset(AUG0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(AUGgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/AUGgranger02.xlsx")

AUGgranger11 <- subset(AUG01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(AUGgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/AUGgranger11.xlsx")

AUGgranger12 <- subset(AUG01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(AUGgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/AUGgranger12.xlsx")

AUGgranger21 <- subset(AUG02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(AUGgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/AUGgranger21.xlsx")

AUGgranger22 <- subset(AUG02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(AUGgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/AUGgranger22.xlsx")

