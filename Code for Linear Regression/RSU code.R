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

RSU0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/RSU1Ly0.xlsx")

RSU <- RSU0[,-1]

summary(RSU)

# Checking the correlation between independent variables

RR <- cor(RSU, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(RSU))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

RSUlm <- lm(y0 ~ x2+x3+x4+x5+x7+x9+x17+x19+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33+x34, data = RSU)

summary(RSUlm)

# Checking the assumptions

vif(RSUlm)

plot(RSUlm$fitted.values, RSUlm$residuals)
RSUlm.stdres = rstandard(RSUlm)
plot(RSU$y0,RSUlm$residuals,ylab="Standardized residuals",xlab="Rokiškio sūris share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(RSUlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(RSUlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(RSUlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(RSUlm)

par(mfrow = c(2, 2))
plot(RSUlm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(RSUlm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- RSU[names_of_influential,]
RSU_without_outliers <- RSU %>% anti_join(outliers)


RSUfit_without_outliers <- lm(y0 ~ x2+x3+x4+x5+x7+x9+x17+x19+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=RSU_without_outliers)
summary(RSUfit_without_outliers)

RSU_stepfit <- step(RSUfit_without_outliers)
summary(RSU_stepfit)

par(mfrow = c(2, 2))
plot(RSU_stepfit)

shapiro.test(RSU_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

RSU01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/RSU1Ly1.xlsx")

RSU1 <- RSU01[,-1]

summary(RSU1)

# Checking the correlation between independent variables

RR <- cor(RSU1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(RSU1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

RSU1lm <- lm(y1 ~ x2+x3+x5+x6+x7+x8+x9+x11+x17+x18+x19+x21+x22+x24+x25+x26+x27+x28+x30+x31+x32+x33+x34, data = RSU1)

summary(RSU1lm)

# Checking the assumptions

vif(RSU1lm)

plot(RSU1lm$fitted.values, RSU1lm$residuals)
RSU1lm.stdres = rstandard(RSU1lm)
plot(RSU1$y1,RSU1lm$residuals,ylab="Standardized residuals",xlab="Rokiškio sūris share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(RSU1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(RSU1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(RSU1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(RSU1lm)

par(mfrow = c(2, 2))
plot(RSU1lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(RSU1lm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- RSU1[names_of_influential,]
RSU1_without_outliers <- RSU1 %>% anti_join(outliers)


RSU1fit_without_outliers <- lm(y1 ~ x2+x3+x5+x6+x7+x8+x9+x11+x17+x18+x19+x21+x22+x24+x25+x26+x27+x28+x30+x31+x32+x33+x34, data=RSU1_without_outliers)
summary(RSU1fit_without_outliers)

RSU1_stepfit <- step(RSU1fit_without_outliers)
summary(RSU1_stepfit)

par(mfrow = c(2, 2))
plot(RSU1_stepfit)

shapiro.test(RSU1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

RSU02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/RSU1Ly2.xlsx")

RSU2 <- RSU02[,-1]

summary(RSU2)

# Checking the correlation between independent variables

RR <- cor(RSU2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(RSU2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

RSU2lm <- lm(y2 ~ x2+x3+x5+x6+x7+x8+x11+x17+x18+x21+x22+x24+x25+x26+x27+x28+x30+x32+x33+x34, data = RSU2)

summary(RSU2lm)

# Checking the assumptions

vif(RSU2lm)

plot(RSU2lm$fitted.values, RSU2lm$residuals)
RSU2lm.stdres = rstandard(RSU2lm)
plot(RSU2$y2,RSU2lm$residuals,ylab="Standardized residuals",xlab="Rokiškio sūris share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(RSU2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(RSU2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(RSU2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(RSU2lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(RSU2lm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- RSU2[names_of_influential,]
RSU2_without_outliers <- RSU2 %>% anti_join(outliers)


RSU2fit_without_outliers <- lm(y2 ~ x2+x3+x5+x6+x7+x8+x11+x17+x18+x21+x22+x24+x25+x26+x27+x28+x30+x32+x33+x34, data=RSU2_without_outliers)
summary(RSU2fit_without_outliers)

RSU2_stepfit <- step(RSU2fit_without_outliers)
summary(RSU2_stepfit)

par(mfrow = c(2, 2))
plot(RSU2_stepfit)

shapiro.test(RSU2_stepfit$residuals)


#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

RSUgranger01 <- subset(RSU0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(RSUgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/RSUgranger01.xlsx")

RSUgranger02 <- subset(RSU0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(RSUgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/RSUgranger02.xlsx")

RSUgranger11 <- subset(RSU01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(RSUgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/RSUgranger11.xlsx")

RSUgranger12 <- subset(RSU01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(RSUgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/RSUgranger12.xlsx")

RSUgranger21 <- subset(RSU02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(RSUgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/RSUgranger21.xlsx")

RSUgranger22 <- subset(RSU02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(RSUgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/RSUgranger22.xlsx")
