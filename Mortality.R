install.packages("readxl")
install.packages("graphics")

library(Hmisc) #Contents and Describe
library(leaps) #Variable selection
library(MASS)

getwd()

setwd("C:/Users/hitpr/Desktop/MSBA/1st semester/Business Analytics/Homework")

getwd()

mortality_data <- read.csv("mortality.csv", header=TRUE)

head(mortality_data)

mortality_data$City <- NULL   ##dropping the city variable

head(mortality_data)

model_mortality0 <- lm(Mortality ~JanTemp+ JulyTemp+ RelHum+ Rain+ Education+ PopDensity+ NW+ WC+ pop+ HHSiz+ income+ HCPot+ NOxPot+ S02Pot, data=mortality_data)

summary(model_mortality0)
cook1 <- cooks.distance(model_mortality0)

plot(cook1)

abline(h = 4*mean(cook1, na.rm=T), col="red")

text(x=1:length(cook1)+1, y=cook1, labels=ifelse(cook1>4*mean(cook1, na.rm=T),names(cook1),""), col="red")

outliers0 <- as.numeric(names(cook1)[(cook1 > 4*mean(cook1, na.rm=T))])  # outlier row numbers
head(mortality_data[outliers0, ])  # outlier observations.

mortality_data1 <-mortality_data[-c(31,36,58), ]
mortality_data1

model_mortality1 <- lm(Mortality ~JanTemp+ JulyTemp+ RelHum+ Rain+ Education+ PopDensity+ NW+ WC+ pop+ HHSiz+ income+ HCPot+ NOxPot+ S02Pot, data=mortality_data1)
summary(model_mortality1)
plot(model_mortality1)
summary(model_mortality1)
library(MASS)
model_mortality2<-stepAIC(model_mortality1, direction="both")
model_mortality2$anova
model_mortality3 <- lm(Mortality ~JanTemp + JulyTemp + Rain + PopDensity + NW + WC + HHSiz + HCPot + S02Pot,data=mortality_data1)
summary(model_mortality3)

mortality_data2<- na.omit(mortality_data1)
pcamortality<-mortality_data2

pcamortality
#remove dependent variable
pcamortality$Mortality <- NULL
#check for non nummeric variables
str(pcamortality)


head(pcamortality)
pca <- princomp(pcamortality, cor = TRUE)
summary(pca) # print variance accounted for
pca$loadings
plot(pca,type="lines")
pca$scores
pca$scores[, 1]
pca_mortality_data<-mortality_data2

pca_mortality_data$pc1<-pca$scores[, 1]
pca_mortality_data$pc2<-pca$scores[, 2]
pca_mortality_data$pc3<-pca$scores[, 3]
pca_mortality_data$pc4<-pca$scores[, 4]
pca_mortality_data$pc5<-pca$scores[, 5]
pca_mortality_data$pc6<-pca$scores[, 6]
pca_mortality_data$pc7<-pca$scores[, 7]
pca_mortality_data$pc8<-pca$scores[, 8]

head(pca_mortality_data)

model_mortality4 <- lm(Mortality ~pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 ,data=pca_mortality_data)
summary(model_mortality4)

model_mortality5<-stepAIC(model_mortality4, direction="both")

model_mortality6<- lm(Mortality ~pc1 + pc2 + pc3 + pc5 + pc6 + pc8 ,data=pca_mortality_data)
summary(model_mortality6)









