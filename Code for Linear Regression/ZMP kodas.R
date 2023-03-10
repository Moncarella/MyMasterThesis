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

ZMPlm <- lm(y0 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=ZMP)

lm.beta(SNGfitstep)

# Nubraižome liekamųjų paklaidų grafikus ir gauname, kad visuose grafikuose stebima tiesinė priklausomybė

avPlots(SNGfitstep)

#_____________________________________REZULTATAI SU Y0____________________________________________________#
# Uzkeliami duomenys

ZMP0 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/ZMP1Ly0.xlsx")

ZMP <- ZMP0[,-1]

summary(ZMP)

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(ZMP), type="pearson")
corrplot(C, method = 'number') 


ZMP0fit <- lm(y0 ~ x5+x7+x9+x10+x11+x16+x19+x21+x22+x23+x26+x27+x28+x29+x30+x31+x32+x33+x34, data=ZMPtest)
vif(ZMP0fit)
ZMP0fitstep <- step(ZMP0fit)
summary(ZMP0fitstep)

vif(ZMP0fitstep)

plot(ZMP0fitstep$fitted.values, ZMP0fitstep$residuals)
ZMP0fitstep.stdres = rstandard(ZMP0fitstep)
plot(ZMP$y0,ZMP0fitstep$residuals,ylab="Standardized residuals",xlab="Snaige akcijos kaina")
abline(0,0)

lm.beta(ZMP0fitstep)
bptest(ZMP0fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(ZMP0fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(ZMP0fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(ZMP0fitstep)
cutoff <- 4/((nrow(ZMPtest)-length(ZMP0fitstep$coefficients)-2))
plot(ZMP0fitstep, which=4, cook.levels = cutoff)
influencePlot(ZMP0fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")

# Tvarkau isskirtis:
cooksd <- cooks.distance(ZMP0fitstep)
sample_size <- nrow(ZMPtest)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/sample_size, col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")

# Removing outliers
# Removing top 2 outliers
top_x_outlier <- 2
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))

ZMPtest <- ZMP[-influential, ]



#_____________________________________REZULTATAI SU Y1____________________________________________________#
# Uzkeliami duomenys

ZMP01 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/ZMP1Ly1.xlsx")

ZMP1 <- ZMP01[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(ZMP1), type="pearson")
corrplot(C, method = 'number') 


ZMP1fit <- lm(y1 ~ x5+x6+x7+x9+x10+x11+x16+x19+x21+x22+x23+x26+x27+x28+x29+x30+x32+x33+x34, data=ZMP1test)
vif(ZMP1fit)
ZMP1fitstep <- step(ZMP1fit)
summary(ZMP1fitstep)

vif(ZMP1fitstep)

plot(ZMP1fitstep$fitted.values, ZMP1fitstep$residuals)
ZMP1fitstep.stdres = rstandard(ZMP1fitstep)
plot(ZMP1$y0,ZMP1fitstep$residuals,ylab="Standardized residuals",xlab="Utenos trikotazas akcijos kaina")
abline(0,0)

bptest(ZMP1fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(ZMP1fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(ZMP1fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(ZMP1fitstep)
cutoff <- 4/((nrow(ZMP1test)-length(ZMP1fitstep$coefficients)-2))
plot(ZMP1fitstep, which=4, cook.levels = cutoff)
influencePlot(ZMP1fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")

# Tvarkau isskirtis:
cooksd <- cooks.distance(ZMP1fitstep)
sample_size <- nrow(ZMP1)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/sample_size, col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")

# Removing outliers - du kartus
# Removing top 2 outliers
top_x_outlier <- 2
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))

ZMP1test <- ZMP1[-influential, ]




#_____________________________________REZULTATAI SU Y2____________________________________________________#
# Uzkeliami duomenys

ZMP02 <-read_xlsx("C:/Users/Monikos-PC/Desktop/Magdata/ZMP1Ly2.xlsx")

ZMP2 <- ZMP02[,-1]

# Tikriname koreliaciją tarp kintamųjų:

# RR <- cor(SNG, use = "complete.obs", method = "spearman")
# corrplot(RR, type="upper", order="hclust", tl.col="black", tl.srt=45, method = "number")

# Tikriname koreliacijų reikšmingumą:

rcorr(as.matrix(ZMP2), type="pearson")
corrplot(C, method = 'number') 


ZMP2fit <- lm(y2 ~ x2+x5+x6+x7+x9+x11+x15+x16+x19+x21+x22+x23+x26+x27+x28+x29+x30+x32+x33+x34, data=ZMP2test)
vif(ZMP2fit)
ZMP2fitstep <- step(ZMP2fit)
summary(ZMP2fitstep)

vif(ZMP2fitstep)

plot(ZMP2fitstep$fitted.values, ZMP2fitstep$residuals)
ZMP2fitstep.stdres = rstandard(ZMP2fitstep)
plot(ZMP2$y0,ZMP2fitstep$residuals,ylab="Standardized residuals",xlab="Utenos trikotazas akcijos kaina")
abline(0,0)

bptest(ZMP2fitstep)

# Tikrinamas standartizuotu liekamuju  paklaidu normalumas grafiskai ir statistiskai

x = rstandard(ZMP2fitstep) # Sukuriame paklaidu kintamaji
hist(x, freq=F, ylim=c(0,0.5))
curve(dnorm(x), add=TRUE, col="blue")

probDist <- pnorm(x)
plot(ppoints(length(x)), sort(probDist), main="PP Plot", xlab="Observed Probability", ylab="Expected Probability")
abline(0,1)

shapiro.test(ZMP2fitstep$residuals)

# Tikriname ar duomenyse nebuvo isskirciu su Bonferonio kriterijumi
outlierTest(ZMP2fitstep)
cutoff <- 4/((nrow(ZMP2)-length(ZMP2fitstep$coefficients)-2))
plot(ZMP2fitstep, which=4, cook.levels = cutoff)
influencePlot(ZMP2fitstep, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")

# Tvarkau isskirtis:
cooksd <- cooks.distance(ZMP2fitstep)
sample_size <- nrow(ZMP2test)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/sample_size, col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")

# Removing outliers - du kartus
# Removing top 2 outliers
top_x_outlier <- 2
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))

ZMP2test <- ZMP2test[-influential, ]


#_____________________________________GRANGERIO PRIEŽASTINGUMO ANALIZĖ____________________________________________________#

ZMPgranger01 <- subset(ZMP0, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x31, x32, x33, x34))
write_xlsx(ZMPgranger01, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/ZMPgranger01.xlsx")

ZMPgranger02 <- subset(ZMP0, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y0))
write_xlsx(ZMPgranger02, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/ZMPgranger02.xlsx")

ZMPgranger11 <- subset(ZMP01, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(ZMPgranger11, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/ZMPgranger11.xlsx")

ZMPgranger12 <- subset(ZMP01, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y1))
write_xlsx(ZMPgranger12, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/ZMPgranger12.xlsx")

ZMPgranger21 <- subset(ZMP02, select=-c(x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34))
write_xlsx(ZMPgranger21, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/ZMPgranger21.xlsx")

ZMPgranger22 <- subset(ZMP02, select=c(Date, x18,x19,x20, x21, x22, x23, x24, x25, x26, x27, x28,x29, x31, x32, x33, x34, y2))
write_xlsx(ZMPgranger22, "C:/Users/Monikos-PC/Desktop/Magdata/Grangeris/ZMPgranger22.xlsx")

