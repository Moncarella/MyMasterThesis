
UTR2fit <- lm(y2 ~ x1+x2+x4+x6+x7+x8+x9+x10+x16+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=UTR2)

remove.packages(rlang)
install.packages("rlang")
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

UTR0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/UTR1Ly0.xlsx")

UTR <- UTR0[,-1]

summary(UTR)

# Checking the correlation between independent variables

RR <- cor(UTR, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(UTR))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

UTRlm <- lm(y0 ~ x1+x2+x3+x5+x7+x9+x10+x11+x13+x21+x22+x26+x27+x28+x29+x30+x31+x32+x33, data = UTR)

summary(UTRlm)

# Checking the assumptions

vif(UTRlm)

plot(UTRlm$fitted.values, UTRlm$residuals)
UTRlm.stdres = rstandard(UTRlm)
plot(UTR$y0,UTRlm$residuals,ylab="Standardized residuals",xlab="Utenos trikotažas share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(UTRlm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(UTRlm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(UTRlm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(UTRlm)

par(mfrow = c(2, 2))
plot(UTRlm)

UTR_stepfit <- step(UTRlm)
summary(UTR_stepfit)

shapiro.test(UTR_stepfit$residuals)



#_____________________________________RESULTS WITH Y1____________________________________________________#

UTR01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/UTR1Ly1.xlsx")

UTR1 <- UTR01[,-1]

summary(UTR1)

# Checking the correlation between independent variables

RR <- cor(UTR1, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(UTR1))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

UTR1lm <- lm(y1 ~ x1+x2+x3+x5+x7+x9+x10+x11+x13+x21+x22+x26+x27+x28+x29+x30+x31+x32+x33, data = UTR1)

summary(UTR1lm)

# Checking the assumptions

vif(UTR1lm)

plot(UTR1lm$fitted.values, UTR1lm$residuals)
UTR1lm.stdres = rstandard(UTR1lm)
plot(UTR1$y1,UTR1lm$residuals,ylab="Standardized residuals",xlab="Utenos trikotažas share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(UTR1lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(UTR1lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(UTR1lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(UTR1lm)

par(mfrow = c(2, 2))
plot(UTR1lm)


UTR1_stepfit <- step(UTR1lm)
summary(UTR1_stepfit)


shapiro.test(UTR1_stepfit$residuals)



#_____________________________________RESULTS WITH Y2____________________________________________________#

UTR02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/UTR1Ly2.xlsx")

UTR2 <- UTR02[,-1]

summary(UTR2)

# Checking the correlation between independent variables

RR <- cor(UTR2, use = "complete.obs", method = "spearman")
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Checking significance of correlations

CC <- rcorr(as.matrix(UTR2))
CC.p <- CC$P
corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "circle")


# Taking out statistically not significant variables to avoid multicollinearity

UTR2lm <- lm(y2 ~ x1+x2+x7+x8+x9+x10+x16+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34, data = UTR2)

summary(UTR2lm)

# Checking the assumptions

vif(UTR2lm)

plot(UTR2lm$fitted.values, UTR2lm$residuals)
UTR2lm.stdres = rstandard(UTR2lm)
plot(UTR2$y2,UTR2lm$residuals,ylab="Standardized residuals",xlab="Grigeo share price")
abline(0,0)

# Checking for heteroscedasticity
bptest(UTR2lm)
# Fail to reject null hypothesis that residuals are homoscedastic


# Checking the standardized residual values for normality (graphically and statistically)

x = rstandard(UTR2lm) # Extracting the standardized residuals
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(UTR2lm$residuals)

# Residuals seem to be normally distributed

# Checking for outliers with the Bonferroni Outlier Test
outlierTest(UTR2lm)

# Filter outliers with significant leverage
cooksD <- cooks.distance(UTR2lm)
influential <- cooksD[(cooksD > (7 * mean(cooksD, na.rm = TRUE)))]
influential

# Removing outliers
names_of_influential <- names(influential)
outliers <- UTR2[names_of_influential,]
UTR2_without_outliers <- UTR2 %>% anti_join(outliers)


UTR2fit_without_outliers <- lm(y2 ~ x1+x2+x7+x8+x9+x10+x16+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=UTR2_without_outliers)
summary(UTR2fit_without_outliers)

UTR2_stepfit <- step(UTR2fit_without_outliers)
summary(UTR2_stepfit)


par(mfrow = c(2, 2))
plot(UTR2_stepfit)

shapiro.test(UTR2_stepfit$residuals)
outlierTest(UTR2_stepfit)


#_____________________________________GRANGER CAUSALITY ANALYSIS____________________________________________________#

UTRgranger01 <- subset(UTR0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(UTRgranger01, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/UTRgranger01.xlsx")

UTRgranger02 <- subset(UTR0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(UTRgranger02, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/UTRgranger02.xlsx")

UTRgranger11 <- subset(UTR01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(UTRgranger11, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/UTRgranger11.xlsx")

UTRgranger12 <- subset(UTR01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(UTRgranger12, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/UTRgranger12.xlsx")

UTRgranger21 <- subset(UTR02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(UTRgranger21, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/UTRgranger21.xlsx")

UTRgranger22 <- subset(UTR02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(UTRgranger22, "C:/Users/Monikos-PC/Desktop/Master thesis for Github/Data/Granger Causality/UTRgranger22.xlsx")

