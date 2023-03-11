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

VLP0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/VLP1Ly0.xlsx")

VLP <- VLP0[,-1]

summary(VLP)

# Checking the correlation between independent variables

RR <- cor(VLP, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(VLP))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

VLPlm <- lm(y0 ~ x2+x4+x7+x9+x11+x16+x21+x22+x24+x25+x26+x27+x28+x29+x30+x32+x33, data = VLP)

summary(VLPlm)

# Checking the assumptions

vif(VLPlm)

plot(VLPlm$fitted.values, VLPlm$residuals)
VLPlm.stdres = rstandard(VLPlm)
plot(VLP$y0,VLPlm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(VLPlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(VLPlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VLPlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(VLPlm)

par(mfrow = c(2, 2))
plot(VLPlm)

VLP_stepfit <- step(VLPlm)
summary(VLP_stepfit)

par(mfrow = c(2, 2))
plot(VLP_stepfit)

shapiro.test(VLP_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

VLP01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/VLP1Ly1.xlsx")

VLP1 <- VLP01[,-1]

summary(VLP1)

# Checking the correlation between independent variables

RR <- cor(VLP1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(VLP1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

VLP1lm <- lm(y1 ~ x2+x4+x7+x9+x11+x16+x21+x22+x24+x25+x26+x27+x28+x29+x30+x32+x33, data=VLP1)

summary(VLP1lm)

# Checking the assumptions

vif(VLP1lm)

plot(VLP1lm$fitted.values, VLP1lm$residuals)
VLP1lm.stdres = rstandard(VLP1lm)
plot(VLP1$y1,VLP1lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(VLP1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(VLP1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VLP1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(VLP1lm)

par(mfrow = c(2, 2))
plot(VLP1lm)


VLP1_stepfit <- step(VLP1lm)
summary(VLP1_stepfit)


shapiro.test(VLP1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

VLP02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/VLP1Ly2.xlsx")

VLP2 <- VLP02[,-1]

summary(VLP2)

# Checking the correlation between independent variables

RR <- cor(VLP2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(VLP2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

VLP2lm <- lm(y2 ~ x2+x7+x11+x14+x16+x21+x23+x25+x27+x28+x30+x32+x33+x34, data = VLP2)

summary(VLP2lm)

# Checking the assumptions

vif(VLP2lm)

plot(VLP2lm$fitted.values, VLP2lm$residuals)
VLP2lm.stdres = rstandard(VLP2lm)
plot(VLP2$y2,VLP2lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(VLP2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(VLP2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VLP2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(VLP2lm)

par(mfrow = c(2, 2))
plot(VLP2lm)


VLP2_stepfit <- step(VLP2lm)
summary(VLP2_stepfit)

par(mfrow = c(2, 2))
plot(VLP2_stepfit)

shapiro.test(VLP2_stepfit$residuals)



#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

VLPgranger01 <- subset(VLP0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(VLPgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VLPgranger01.xlsx")

VLPgranger02 <- subset(VLP0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(VLPgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VLPgranger02.xlsx")

VLPgranger11 <- subset(VLP01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VLPgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VLPgranger11.xlsx")

VLPgranger12 <- subset(VLP01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(VLPgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VLPgranger12.xlsx")

VLPgranger21 <- subset(VLP02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VLPgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VLPgranger21.xlsx")

VLPgranger22 <- subset(VLP02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(VLPgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/VLPgranger22.xlsx")


