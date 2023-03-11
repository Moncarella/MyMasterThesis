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

ZMP0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/ZMP1Ly0.xlsx")

ZMP <- ZMP0[,-1]

summary(ZMP)

# Checking the correlation between independent variables

RR <- cor(ZMP, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(ZMP))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

ZMPlm <- lm(y0 ~ x1+x3+x5+x7+x8+x16+x22+x24+x25+x26+x28+x30+x31+x32+x33, data = ZMP)

summary(ZMPlm)

# Checking the assumptions

vif(ZMPlm)

plot(ZMPlm$fitted.values, ZMPlm$residuals)
ZMPlm.stdres = rstandard(ZMPlm)
plot(ZMP$y0,ZMPlm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(ZMPlm)
# Reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(ZMPlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(ZMPlm$residuals)

# Residuals seem not to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(ZMPlm)

par(mfrow = c(2, 2))
plot(ZMPlm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(ZMPlm)
influential <- cooksD[(cooksD > (7 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- ZMP[names_of_influential,]
ZMP_without_outliers <- ZMP %>% anti_join(outliers)


ZMPfit_without_outliers <- lm(y0 ~ x1+x3+x5+x7+x8+x16+x22+x24+x25+x26+x28+x30+x31+x32+x33, data=ZMP_without_outliers)
summary(ZMPfit_without_outliers)

ZMP_stepfit <- step(ZMPfit_without_outliers)
summary(ZMP_stepfit)

par(mfrow = c(2, 2))
plot(ZMP_stepfit)

shapiro.test(ZMP_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

ZMP01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/ZMP1Ly1.xlsx")

ZMP1 <- ZMP01[,-1]

summary(ZMP1)

# Checking the correlation between independent variables

RR <- cor(ZMP1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(ZMP1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

ZMP1lm <- lm(y1 ~ x5+x6+x7+x16+x22+x23+x26+x28+x29+x30+x32+x33+x34, data = ZMP1)

summary(ZMP1lm)

# Checking the assumptions

vif(ZMP1lm)

plot(ZMP1lm$fitted.values, ZMP1lm$residuals)
ZMP1lm.stdres = rstandard(ZMP1lm)
plot(ZMP1$y1,ZMP1lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(ZMP1lm)
# Reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(ZMP1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(ZMP1lm$residuals)

# Residuals seem not to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(ZMP1lm)

# Removing outliers
names_of_influential <- c("11", "2")
outliers <- ZMP1[names_of_influential,]
ZMP1_without_outliers <- ZMP1 %>% anti_join(outliers)


ZMP1fit_without_outliers <- lm(y1 ~ x5+x6+x7+x16+x22+x23+x26+x28+x29+x30+x32+x33+x34, data=ZMP1_without_outliers)
summary(ZMP1fit_without_outliers)

ZMP1_stepfit <- step(ZMP1fit_without_outliers)
summary(ZMP1_stepfit)


shapiro.test(ZMP1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

ZMP02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/ZMP1Ly2.xlsx")

ZMP2 <- ZMP02[,-1]

summary(ZMP2)

# Checking the correlation between independent variables

RR <- cor(ZMP2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(ZMP2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

ZMP2lm <- lm(y2 ~ x2+x5+x6+x7+x9+x11+x16+x21+x22+x23+x26+x28+x29+x30+x32+x33+x34, data = ZMP2)

summary(ZMP2lm)

# Checking the assumptions

vif(ZMP2lm)

plot(ZMP2lm$fitted.values, ZMP2lm$residuals)
ZMP2lm.stdres = rstandard(ZMP2lm)
plot(ZMP2$y2,ZMP2lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(ZMP2lm)
# Reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(ZMP2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(ZMP2lm$residuals)

# Residuals seem not to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(ZMP2lm)

par(mfrow = c(2, 2))
plot(ZMP2lm)

# Removing outliers
names_of_influential <- c("1", "8")
outliers <- ZMP2[names_of_influential,]
ZMP2_without_outliers <- ZMP2 %>% anti_join(outliers)


ZMP2fit_without_outliers <- lm(y2 ~ x2+x5+x6+x7+x9+x11+x16+x21+x22+x23+x26+x28+x29+x30+x32+x33+x34, data=ZMP2_without_outliers)
summary(ZMP2fit_without_outliers)

ZMP2_stepfit <- step(ZMP2fit_without_outliers)
summary(ZMP2_stepfit)

shapiro.test(ZMP2_stepfit$residuals)



#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

ZMPgranger01 <- subset(ZMP0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(ZMPgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/ZMPgranger01.xlsx")

ZMPgranger02 <- subset(ZMP0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(ZMPgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/ZMPgranger02.xlsx")

ZMPgranger11 <- subset(ZMP01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(ZMPgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/ZMPgranger11.xlsx")

ZMPgranger12 <- subset(ZMP01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(ZMPgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/ZMPgranger12.xlsx")

ZMPgranger21 <- subset(ZMP02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(ZMPgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/ZMPgranger21.xlsx")

ZMPgranger22 <- subset(ZMP02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(ZMPgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/ZMPgranger22.xlsx")

