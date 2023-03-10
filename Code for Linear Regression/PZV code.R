PZV2fit <- lm(y2 ~ x4+x7+x8+x11+x12+x13+x17+x18+x19+x20+x21+x23+x25+x27+x28+x29+x30+x31+x32+x33+x34, data=PZV2)

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

PZV0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/PZV1Ly0.xlsx")

PZV <- PZV0[,-1]

summary(PZV)

# Checking the correlation between independent variables

RR <- cor(PZV, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(PZV))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

PZVlm <- lm(y0 ~ x4+x7+x8+x11+x14+x16+x17+x18+x19+x21+x23+x25+x27+x28+x29+x30+x31+x32+x33+x34, data = PZV)

summary(PZVlm)

# Checking the assumptions

vif(PZVlm)

# plot(PZVlm$fitted.values, PZVlm$residuals)
# PZVlm.stdres = rstandard(PZVlm)
# plot(PZV$y0,PZVlm$residuals,ylab="Standardized residuals",xlab="Pieno žvaigždės share price")
# abline(0,0)

# Checking for heteroscedasticity
bptest(PZVlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(PZVlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(PZVlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(PZVlm)

par(mfrow = c(2, 2))
plot(PZVlm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(PZVlm)
influential <- cooksD[(cooksD > (6 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- PZV[names_of_influential,]
PZV_without_outliers <- PZV %>% anti_join(outliers)


PZVfit_without_outliers <- lm(y0 ~ x4+x7+x8+x11+x14+x16+x17+x18+x19+x21+x23+x25+x27+x28+x29+x30+x31+x32+x33+x34, data = PZV_without_outliers)
summary(PZVfit_without_outliers)

PZV_stepfit <- step(PZVfit_without_outliers)
summary(PZV_stepfit)

par(mfrow = c(2, 2))
plot(PZV_stepfit)

shapiro.test(PZV_stepfit$residuals)

#_____________________________________RESULTS WITH Y1____________________________________________________#
PZV01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/PZV1Ly1.xlsx")

PZV1 <- PZV01[,-1]

summary(PZV1)

# Checking the correlation between independent variables

RR <- cor(PZV1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(PZV1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

PZV1lm <- lm(y1 ~ x1+x7+x8+x11+x16+x17+x18+x19+x21+x23+x25+x27+x28+x29+x30+x31+x32+x33+x34, data = PZV1)

summary(PZV1lm)

# Checking the assumptions

vif(PZV1lm)

# plot(PZV1lm$fitted.values, PZV1lm$residuals)
# PZV1lm.stdres = rstandard(PZV1lm)
# plot(PZV1$y1,PZV1lm$residuals,ylab="Standardized residuals",xlab="Pieno žvaigždės share price")
# abline(0,0)

# Checking for heteroscedasticity
bptest(PZV1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(PZV1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(PZV1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(PZV1lm)

par(mfrow = c(2, 2))
plot(PZV1lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(PZV1lm)
influential <- cooksD[(cooksD > (5 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- PZV1[names_of_influential,]
PZV1_without_outliers <- PZV1 %>% anti_join(outliers)


PZV1fit_without_outliers <- lm(y1 ~ x1+x7+x8+x11+x16+x17+x18+x19+x21+x23+x25+x27+x28+x29+x30+x31+x32+x33+x34, data = PZV1_without_outliers)
summary(PZV1fit_without_outliers)

PZV1_stepfit <- step(PZV1fit_without_outliers)
summary(PZV1_stepfit)

par(mfrow = c(2, 2))
plot(PZV1_stepfit)

shapiro.test(PZV1_stepfit$residuals)

#_____________________________________RESULTS WITH Y2____________________________________________________#

PZV02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/PZV1Ly2.xlsx")

PZV2 <- PZV02[,-1]

summary(PZV2)

# Checking the correlation between independent variables

RR <- cor(PZV2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(PZV2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

PZV2lm <- lm(y2 ~ x4+x7+x8+x11+x12+x13+x17+x18+x19+x20+x21+x23+x25+x27+x28+x29+x30+x31+x32+x33+x34, data = PZV2)

summary(PZV2lm)

# Checking the assumptions

vif(PZV2lm)

# plot(PZVlm$fitted.values, PZVlm$residuals)
# PZVlm.stdres = rstandard(PZVlm)
# plot(PZV$y0,PZVlm$residuals,ylab="Standardized residuals",xlab="Pieno žvaigždės share price")
# abline(0,0)

# Checking for heteroscedasticity
bptest(PZV2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(PZV2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(PZV2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(PZV2lm)

par(mfrow = c(2, 2))
plot(PZV2lm)

PZV2_stepfit <- step(PZV2lm)
summary(PZV2_stepfit)

par(mfrow = c(2, 2))
plot(PZV2_stepfit)

shapiro.test(PZV2_stepfit$residuals)

#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

PZVgranger01 <- subset(PZV0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(PZVgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/PZVgranger01.xlsx")

PZVgranger02 <- subset(PZV0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(PZVgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/PZVgranger02.xlsx")

PZVgranger11 <- subset(PZV01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(PZVgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/PZVgranger11.xlsx")

PZVgranger12 <- subset(PZV01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(PZVgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/PZVgranger12.xlsx")

PZVgranger21 <- subset(PZV02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(PZVgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/PZVgranger21.xlsx")

PZVgranger22 <- subset(PZV02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(PZVgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/PZVgranger22.xlsx")


