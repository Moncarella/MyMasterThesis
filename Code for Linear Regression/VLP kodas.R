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

AUGlm <- lm(y0 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=AUG)
summary(AUGlm)

lm.beta(SNGfitstep)

# Nubraižome liekamųjų paklaidų grafikus ir gauname, kad visuose grafikuose stebima tiesinė priklausomybė

avPlots(SNGfitstep)

#_____________________________________REZULTATAI SU Y0____________________________________________________#
# Uzkeliami duomenys

VLP0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/VLP1Ly0.xlsx")

VLP <- VLP0[,-1]

summary(VLP)

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(VLP), type="pearson")
corrplot(C, method = 'number') 


# Iskarto isimamas x30, kadangi yra nuliai


VLP0fit <- lm(y0 ~ x2+x7+x11+x12+x13+x14+x16+x17+x18+x20+x21+x23+x25+x27+x28+x30+x31+x32+x33+x34, data=VLP)
vif(VLP0fit)
VLP0fitstep <- step(VLP0fit)
summary(VLP0fitstep)

vif(VLP0fitstep)

lm.beta(VLP0fitstep)

plot(VLP0fitstep$fitted.values, VLP0fitstep$residuals)
VLP0fitstep.stdres = rstandard(VLP0fitstep)
plot(PZV$y0,VLP0fitstep$residuals,ylab="Standardized residuals",xlab="Snaige akcijos kaina")
abline(0,0)

bptest(VLP0fitstep)
shapiro.test(VLP0fitstep$residuals)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(VLP0fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VLP0fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(VLP0fitstep)
cutoff <- 4/((nrow(VLP)-length(VLP0fitstep$coefficients)-2))
plot(VLP0fitstep, which=4, cook.levels = cutoff)
influencePlot(VLP0fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")



#_____________________________________REZULTATAI SU Y1____________________________________________________#
# Uzkeliami duomenys

VLP01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/VLP1Ly1.xlsx")

VLP1 <- VLP01[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(VLP1), type="pearson")
corrplot(C, method = 'number') 


VLP1fit <- lm(y1 ~ x2+x7+x11+x12+x13+x14+x16+x17+x18+x20+x21+x23+x25+x27+x28+x30+x31+x32+x33+x34, data=VLP1)
vif(VLP1fit)
VLP1fitstep <- step(VLP1fit)
summary(VLP1fitstep)

vif(VLP1fitstep)
lm.beta(VLP1fitstep)

plot(VLP1fitstep$fitted.values, VLP1fitstep$residuals)
VLP1fitstep.stdres = rstandard(VLP1fitstep)
plot(VLP1$y0,VLP1fitstep$residuals,ylab="Standardized residuals",xlab="AUGA akcijos kaina")
abline(0,0)

bptest(VLP1fitstep)
shapiro.test(VLP1fitstep$residuals)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(VLP1fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VLP1fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(VLP1fitstep)
cutoff <- 4/((nrow(VLP1)-length(VLP1fitstep$coefficients)-2))
plot(VLP1fitstep, which=4, cook.levels = cutoff)
influencePlot(VLP1fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")



#_____________________________________REZULTATAI SU Y2____________________________________________________#
# Uzkeliami duomenys

VLP02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/VLP1Ly2.xlsx")

VLP2 <- VLP02[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(VLP2), type="pearson")
corrplot(C, method = 'number') 


VLP2fit <- lm(y2 ~ x2+x7+x11+x14+x16+x21+x23+x25+x27+x28+x30+x31+x32+x33+x34, data=VLP2)
vif(VLP2fit)
VLP2fitstep <- step(VLP2fit)
summary(VLP2fitstep)

vif(VLP2fitstep)
lm.beta(VLP2fitstep)

plot(VLP2fitstep$fitted.values, VLP2fitstep$residuals)
VLP2fitstep.stdres = rstandard(VLP2fitstep)
plot(VLP2$y0,VLP2fitstep$residuals,ylab="Standardized residuals",xlab="AUGA trikotazas akcijos kaina")
abline(0,0)

bptest(VLP2fitstep)
shapiro.test(VLP2fitstep$residuals)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(VLP2fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VLP2fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(VLP2fitstep)
cutoff <- 4/((nrow(VLP2)-length(VLP2fitstep$coefficients)-2))
plot(VLP2fitstep, which=4, cook.levels = cutoff)
influencePlot(VLP2fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")



#_____________________________________GRANGERIO PRIEŽASTINGUMO ANALIZĖ____________________________________________________#

VLPgranger01 <- subset(VLP0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(VLPgranger01, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VLPgranger01.xlsx")

VLPgranger02 <- subset(VLP0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(VLPgranger02, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VLPgranger02.xlsx")

VLPgranger11 <- subset(VLP01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VLPgranger11, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VLPgranger11.xlsx")

VLPgranger12 <- subset(VLP01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(VLPgranger12, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VLPgranger12.xlsx")

VLPgranger21 <- subset(VLP02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VLPgranger21, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VLPgranger21.xlsx")

VLPgranger22 <- subset(VLP02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(VLPgranger22, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VLPgranger22.xlsx")


