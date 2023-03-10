# Uzkeliamos bibliotekos

library(corrplot)
library(Hmisc)
library(readxl)
library(lm.beta)
library(car)
library(lmtest)
library(PerformanceAnalytics)

options(scipen=999)
options(max.print=1000000)

UTRlm <- lm(y0 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=UTR)
summary(SNGlm)

lm.beta(SNGfitstep)

# Nubraižome liekamųjų paklaidų grafikus ir gauname, kad visuose grafikuose stebima tiesinė priklausomybė

avPlots(SNGfitstep)

#_____________________________________REZULTATAI SU Y0____________________________________________________#
# Uzkeliami duomenys

UTR0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/UTR1Ly0.xlsx")

UTR <- UTR0[,-1]

summary(UTR)

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(UTR), type="pearson")
corrplot(C, method = 'number') 


UTR0fit <- lm(y0 ~ x1+x2+x3+x5+x6+x7+x9+x10+x11+x13+x16+x21+x22+x26+x27+x28+x29+x30+x31+x32+x33, data=UTR)
vif(UTR0fit)
UTR0fitstep <- step(UTR0fit)
summary(UTR0fitstep)

lm.beta(UTR0fitstep)

vif(UTR0fitstep)

plot(UTR0fitstep$fitted.values, UTR0fitstep$residuals)
UTR0fitstep.stdres = rstandard(UTR0fitstep)
plot(UTR$y0,UTR0fitstep$residuals,ylab="Standardized residuals",xlab="Snaige akcijos kaina")
abline(0,0)

bptest(UTR0fitstep)

durbinWatsonTest(UTR0fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(UTR0fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(UTR0fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(UTR0fitstep)
cutoff <- 4/((nrow(UTR)-length(UTR0fitstep$coefficients)-2))
plot(UTR0fitstep, which=4, cook.levels = cutoff)
influencePlot(UTR0fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")


#_____________________________________REZULTATAI SU Y1____________________________________________________#
# Uzkeliami duomenys

UTR01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/UTR1Ly1.xlsx")

UTR1 <- UTR01[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(UTR1), type="pearson")
corrplot(C, method = 'number') 


UTR1fit <- lm(y1 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x11+x12+x16+x21+x22+x23+x24+x26+x27+x29+x30+x31+x34, data=UTR1)
vif(UTR1fit)
UTR1fitstep <- step(UTR1fit)
summary(UTR1fitstep)

vif(UTR1fitstep)

plot(UTR1fitstep$fitted.values, UTR1fitstep$residuals)
UTR1fitstep.stdres = rstandard(UTR1fitstep)
plot(UTR1$y0,UTR1fitstep$residuals,ylab="Standardized residuals",xlab="Utenos trikotazas akcijos kaina")
abline(0,0)

bptest(UTR1fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(UTR1fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(UTR1fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(UTR1fitstep)
cutoff <- 4/((nrow(UTR1)-length(UTR1fitstep$coefficients)-2))
plot(UTR1fitstep, which=4, cook.levels = cutoff)
influencePlot(UTR1fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")




#_____________________________________REZULTATAI SU Y2____________________________________________________#
# Uzkeliami duomenys

UTR02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/UTR1Ly2.xlsx")

UTR2 <- UTR02[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(UTR2), type="pearson")
corrplot(C, method = 'number') 


UTR2fit <- lm(y2 ~ x1+x2+x4+x6+x7+x8+x9+x10+x16+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=UTR2)
vif(UTR2fit)
UTR2fitstep <- step(UTR2fit)
summary(UTR2fitstep)

vif(UTR2fitstep)

plot(UTR2fitstep$fitted.values, UTR2fitstep$residuals)
UTR2fitstep.stdres = rstandard(UTR2fitstep)
plot(UTR2$y0,UTR2fitstep$residuals,ylab="Standardized residuals",xlab="Utenos trikotazas akcijos kaina")
abline(0,0)

bptest(UTR2fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(UTR2fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(UTR2fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(UTR2fitstep)
cutoff <- 4/((nrow(UTR2)-length(UTR2fitstep$coefficients)-2))
plot(UTR2fitstep, which=4, cook.levels = cutoff)
influencePlot(UTR2fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")




#_____________________________________GRANGERIO PRIEŽASTINGUMO ANALIZĖ____________________________________________________#

UTRgranger01 <- subset(UTR0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(UTRgranger01, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/UTRgranger01.xlsx")

UTRgranger02 <- subset(UTR0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(UTRgranger02, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/UTRgranger02.xlsx")

UTRgranger11 <- subset(UTR01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(UTRgranger11, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/UTRgranger11.xlsx")

UTRgranger12 <- subset(UTR01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(UTRgranger12, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/UTRgranger12.xlsx")

UTRgranger21 <- subset(UTR02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(UTRgranger21, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/UTRgranger21.xlsx")

UTRgranger22 <- subset(UTR02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(UTRgranger22, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/UTRgranger22.xlsx")

