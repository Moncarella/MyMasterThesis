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

GRG0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/GRG1Ly0.xlsx")

GRG <- GRG0[,-1]

summary(GRG)

# Checking the correlation between independent variables

RR <- cor(GRG, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(GRG))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

GRGlm <- lm(y0 ~ x2+x6+x7+x9+x10+x11+x12+x13+x22+x23+x25+x27+x28+x30+x32+x33+x34, data = GRG)

summary(GRGlm)

# Checking the assumptions

vif(GRGlm)

plot(GRGlm$fitted.values, GRGlm$residuals)
GRGlm.stdres = rstandard(GRGlm)
plot(GRG$y0,GRGlm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(GRGlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(GRGlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(GRGlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(GRGlm)

par(mfrow = c(2, 2))
plot(GRGlm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(GRGlm)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- GRG[names_of_influential,]
GRG_without_outliers <- GRG %>% anti_join(outliers)


GRGfit_without_outliers <- lm(y0 ~ x2+x6+x7+x9+x10+x11+x12+x13+x22+x23+x25+x27+x28+x30+x32+x33+x34, data=GRG_without_outliers)
summary(GRGfit_without_outliers)

GRG_stepfit <- step(GRGfit_without_outliers)
summary(GRG_stepfit)

par(mfrow = c(2, 2))
plot(GRG_stepfit)

shapiro.test(GRG_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

GRG01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/GRG1Ly1.xlsx")

GRG1 <- GRG01[,-1]

summary(GRG1)

# Checking the correlation between independent variables

RR <- cor(GRG1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(GRG1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

GRG1lm <- lm(y1 ~ x2+x6+x7+x8+x9+x10+x11+x12+x13+x22+x23+x25+x27+x28+x30+x31+x32+x33+x34, data = GRG1)

summary(GRG1lm)

# Checking the assumptions

vif(GRG1lm)

plot(GRG1lm$fitted.values, GRG1lm$residuals)
GRG1lm.stdres = rstandard(GRG1lm)
plot(GRG1$y1,GRG1lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(GRG1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(GRG1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(GRG1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(GRG1lm)

par(mfrow = c(2, 2))
plot(GRG1lm)


GRG1_stepfit <- step(GRG1lm)
summary(GRG1_stepfit)


shapiro.test(GRG1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

GRG02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/GRG1Ly2.xlsx")

GRG2 <- GRG02[,-1]

summary(GRG2)

# Checking the correlation between independent variables

RR <- cor(GRG2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(GRG2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

GRG2lm <- lm(y2 ~ x5+x7+x8+x9+x10+x11+x12+x13+x22+x23+x25+x27+x28+x30+x32+x33+x34, data = GRG2)

summary(GRG2lm)

# Checking the assumptions

vif(GRG2lm)

plot(GRG2lm$fitted.values, GRG2lm$residuals)
GRG2lm.stdres = rstandard(GRG2lm)
plot(GRG2$y2,GRG2lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(GRG2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(GRG2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(GRG2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(GRG2lm)

par(mfrow = c(2, 2))
plot(GRG2lm)


GRG2_stepfit <- step(GRG2lm)
summary(GRG2_stepfit)

par(mfrow = c(2, 2))
plot(GRG2_stepfit)

shapiro.test(GRG2_stepfit$residuals)


#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

GRGgranger01 <- subset(GRG0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(GRGgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/GRGgranger01.xlsx")

GRGgranger02 <- subset(GRG0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(GRGgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/GRGgranger02.xlsx")

GRGgranger11 <- subset(GRG01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(GRGgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/GRGgranger11.xlsx")

GRGgranger12 <- subset(GRG01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(GRGgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/GRGgranger12.xlsx")

GRGgranger21 <- subset(GRG02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(GRGgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/GRGgranger21.xlsx")

GRGgranger22 <- subset(GRG02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(GRGgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/GRGgranger22.xlsx")

