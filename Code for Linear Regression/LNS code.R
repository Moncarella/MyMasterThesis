
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

LNS0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/LNS1Ly0.xlsx")

LNS <- LNS0[,-1]

summary(LNS)

# Checking the correlation between independent variables

RR <- cor(LNS, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(LNS))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

LNSlm <- lm(y0 ~ x1+x2+x7+x8+x9+x13+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x31+x32+x33+x34, data=LNS)

summary(LNSlm)

# Checking the assumptions

vif(LNSlm)

plot(LNSlm$fitted.values, LNSlm$residuals)
LNSlm.stdres = rstandard(LNSlm)
plot(LNS$y0,LNSlm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(LNSlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(LNSlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(LNSlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(LNSlm)

par(mfrow = c(2, 2))
plot(LNSlm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(LNSlm)
influential <- cooksD[(cooksD > (4 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- LNS[names_of_influential,]
LNS_without_outliers <- LNS %>% anti_join(outliers)


LNSfit_without_outliers <- lm(y0 ~ x1+x2+x7+x8+x9+x13+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x31+x32+x33+x34, data=LNS_without_outliers)
summary(LNSfit_without_outliers)

LNS_stepfit <- step(LNSfit_without_outliers)
summary(LNS_stepfit)

par(mfrow = c(2, 2))
plot(LNS_stepfit)

shapiro.test(LNS_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

LNS01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/LNS1Ly1.xlsx")

LNS1 <- LNS01[,-1]

summary(LNS1)

# Checking the correlation between independent variables

RR <- cor(LNS1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(LNS1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

LNS1lm <- lm(y1 ~ x1+x2+x5+x7+x8+x9+x11+x12+x13+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x31+x32+x33+x34, data=LNS1)

summary(LNS1lm)

# Checking the assumptions

vif(LNS1lm)

plot(LNS1lm$fitted.values, LNS1lm$residuals)
LNS1lm.stdres = rstandard(LNS1lm)
plot(LNS1$y1,LNS1lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(LNS1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(LNS1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(LNS1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(LNS1lm)

par(mfrow = c(2, 2))
plot(LNS1lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(LNS1lm)
influential <- cooksD[(cooksD > (8 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- LNS1[names_of_influential,]
LNS1_without_outliers <- LNS1 %>% anti_join(outliers)


LNS1fit_without_outliers <- lm(y1 ~ x1+x2+x5+x7+x8+x9+x11+x12+x13+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x31+x32+x33+x34, data=LNS1_without_outliers)
summary(LNS1fit_without_outliers)

LNS1_stepfit <- step(LNS1fit_without_outliers)
summary(LNS1_stepfit)

par(mfrow = c(2, 2))
plot(LNS1_stepfit)

shapiro.test(LNS1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

LNS02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/LNS1Ly2.xlsx")

LNS2 <- LNS02[,-1]

summary(LNS2)

# Checking the correlation between independent variables

RR <- cor(LNS2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(LNS2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

LNS2lm <- lm(y2 ~ x1+x2+x5+x7+x8+x9+x11+x12+x13+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x31+x32+x33+x34, data=LNS2)

summary(LNS2lm)

# Checking the assumptions

vif(LNS2lm)

plot(LNS2lm$fitted.values, LNS2lm$residuals)
LNS2lm.stdres = rstandard(LNS2lm)
plot(LNS2$y2,LNS2lm$residuals,ylab="Standardized residuals",xlab="Linas share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(LNS2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(LNS2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(LNS2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(LNS2lm)

par(mfrow = c(2, 2))
plot(LNS2lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(LNS2lm)
influential <- cooksD[(cooksD > (8 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- LNS2[names_of_influential,]
LNS2_without_outliers <- LNS2 %>% anti_join(outliers)


LNS2fit_without_outliers <- lm(y2 ~ x1+x2+x5+x7+x8+x9+x11+x12+x13+x18+x19+x21+x22+x23+x24+x25+x26+x27+x28+x29+x31+x32+x33+x34, data=LNS2_without_outliers)
summary(LNS2fit_without_outliers)

LNS2_stepfit <- step(LNS2fit_without_outliers)
summary(LNS2_stepfit)

par(mfrow = c(2, 2))
plot(LNS_stepfit)

shapiro.test(LNS2_stepfit$residuals)



#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

LNSgranger01 <- subset(LNS0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(LNSgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/LNSgranger01.xlsx")

LNSgranger02 <- subset(LNS0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(LNSgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/LNSgranger02.xlsx")

LNSgranger11 <- subset(LNS01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(LNSgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/LNSgranger11.xlsx")

LNSgranger12 <- subset(LNS01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(LNSgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/LNSgranger12.xlsx")

LNSgranger21 <- subset(LNS02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(LNSgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/LNSgranger21.xlsx")

LNSgranger22 <- subset(LNS02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(LNSgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/LNSgranger22.xlsx")