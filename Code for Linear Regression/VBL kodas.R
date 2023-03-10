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

VBL0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/VBL1Ly0.xlsx")

VBL <- VBL0[,-1]

summary(VBL)

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(VBL), type="pearson")
corrplot(C, method = 'number') 


# Iskarto isimamas x30, kadangi yra nuliai
# Gerinam modeli x16


VBL0fit <- lm(y0 ~ x3+x11+x15+x17+x22+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33, data=VBL)
vif(VBL0fit)
VBL0fitstep <- step(VBL0fit)
summary(VBL0fitstep)

vif(VBL0fitstep)
bptest(VBL0fitstep)
lm.beta(VBL0fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(VBL0fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VBL0fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(VBL0fitstep)
cutoff <- 4/((nrow(VBL)-length(VBL0fitstep$coefficients)-2))
plot(VBL0fitstep, which=4, cook.levels = cutoff)
influencePlot(VBL0fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")



#_____________________________________REZULTATAI SU Y1____________________________________________________#
# Uzkeliami duomenys

VBL01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/VBL1Ly1.xlsx")

VBL1 <- VBL01[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(VBL1), type="pearson")
corrplot(C, method = 'number') 


VBL1fit <- lm(y1 ~ x2+x7+x11+x14+x16+x17+x21+x23+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=VBL1)
vif(VBL1fit)
VBL1fitstep <- step(VBL1fit)
summary(VBL1fitstep)

vif(VBL1fitstep)
bptest(VBL1fitstep)


# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(VBL1fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VBL1fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(VBL1fitstep)
cutoff <- 4/((nrow(VBL1)-length(VBL1fitstep$coefficients)-2))
plot(VBL1fitstep, which=4, cook.levels = cutoff)
influencePlot(VBL1fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")



#_____________________________________REZULTATAI SU Y2____________________________________________________#
# Uzkeliami duomenys

VBL02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/VBL1Ly2.xlsx")

VBL2 <- VBL02[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(VBL2), type="pearson")
corrplot(C, method = 'number') 


VBL2fit <- lm(y2 ~ x10+x17+x18+x21+x24+x25+x26+x27+x29+x30+x31+x32+x33, data=VBL2)
vif(VBL2fit)
VBL2fitstep <- step(VBL2fit)
summary(VBL2fitstep)

vif(VBL2fitstep)
shapiro.test(VBL2fitstep$residuals)
bptest(VBL2fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(VBL2fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(VBL2fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(VBL2fitstep)
cutoff <- 4/((nrow(VBL2)-length(VBL2fitstep$coefficients)-2))
plot(VBL2fitstep, which=4, cook.levels = cutoff)
influencePlot(VBL2fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")


#_____________________________________GRANGERIO PRIEŽASTINGUMO ANALIZĖ____________________________________________________#

VBLgranger01 <- subset(VBL0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(VBLgranger01, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VBLgranger01.xlsx")

VBLgranger02 <- subset(VBL0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(VBLgranger02, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VBLgranger02.xlsx")

VBLgranger11 <- subset(VBL01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VBLgranger11, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VBLgranger11.xlsx")

VBLgranger12 <- subset(VBL01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(VBLgranger12, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VBLgranger12.xlsx")

VBLgranger21 <- subset(VBL02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(VBLgranger21, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VBLgranger21.xlsx")

VBLgranger22 <- subset(VBL02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(VBLgranger22, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/VBLgranger22.xlsx")




