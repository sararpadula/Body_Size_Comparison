## Wing length differences and breeding success

#Modeling whether wing length difference influences breeding success
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(cowplot)
library(lme4)
library(lmerTest)
library(DHARMa)
library(glmmTMB)
library(car)
library(emmeans)
library(ggpubr)
library(ggdist)

### Loading data #######

breed <- read.csv("paired_SMI.csv")

breed$logElevation <- log(breed$Elevation)

#Get factors needed for modeling
breed <- breed %>%
  mutate(Year = as.factor(Year), MaleBander = as.factor(MaleBander), FemaleBander = as.factor(FemaleBander), as.factor(Location))


mochBreed <- subset(breed, Species == "MOCH")
bcchBreed <- subset(breed, Species == "BCCH")

mochBreed <- subset(mochBreed, Wing_Difference_Category %in% c("Slightly", "Much"))
bcchBreed <- subset(bcchBreed, Wing_Difference_Category %in% c("Slightly", "Much"))

#first egg date
#remove blank values
mochBreed.fe <- mochBreed %>% filter(!is.na(First.Egg))
bcchBreed.fe <- bcchBreed %>% filter(!is.na(First.Egg))

## Clutch Size
#Remove blank values
mochBreed.cs <- mochBreed %>% filter(!is.na(Egg_Number))
bcchBreed.cs <- bcchBreed %>% filter(!is.na(Egg_Number))

## Brood Size
#Remove Blank values
mochBreed.bs <- mochBreed %>% filter(!is.na(Nestling_Number))
bcchBreed.bs <- bcchBreed %>% filter(!is.na(Nestling_Number))

## Mean nestling mass
#Remove Blank values
mochBreed.mm <- mochBreed %>% filter(!is.na(Avg_Nestling_Weight))
bcchBreed.mm <- bcchBreed %>% filter(!is.na(Avg_Nestling_Weight))

## Female SMI
mochBreed.fS <- mochBreed %>% filter(!is.na(Female_SMI))
bcchBreed.fS <- bcchBreed %>% filter(!is.na(Female_SMI))

## Male SMI
mochBreed.mS <- mochBreed %>% filter(!is.na(Male_SMI))
bcchBreed.mS <- bcchBreed %>% filter(!is.na(Male_SMI))

## Numerically subsetting data #########################################
mochBreedNum <- subset(breed, Species == "MOCH")
bcchBreedNum <- subset(breed, Species == "BCCH")

#first egg date
#remove blank values
mochBreedN.fe <- mochBreedNum %>% filter(!is.na(First.Egg))
bcchBreedN.fe <- bcchBreedNum %>% filter(!is.na(First.Egg))

## Clutch Size
#Remove blank values
mochBreedN.cs <- mochBreedNum %>% filter(!is.na(Egg_Number))
bcchBreedN.cs <- bcchBreedNum %>% filter(!is.na(Egg_Number))

## Brood Size
#Remove Blank values
mochBreedN.bs <- mochBreedNum %>% filter(!is.na(Nestling_Number))
bcchBreedN.bs <- bcchBreedNum %>% filter(!is.na(Nestling_Number))

## Mean nestling mass
#Remove Blank values
mochBreedN.mm <- mochBreedNum %>% filter(!is.na(Avg_Nestling_Weight))
bcchBreedN.mm <- bcchBreedNum %>% filter(!is.na(Avg_Nestling_Weight))

## Female SMI
mochBreedN.fS <- mochBreedNum %>% filter(!is.na(Female_SMI))
bcchBreedN.fS <- bcchBreedNum %>% filter(!is.na(Female_SMI))

## Male SMI
mochBreedN.mS <- mochBreedNum %>% filter(!is.na(Male_SMI))
bcchBreedN.mS <- bcchBreedNum %>% filter(!is.na(Male_SMI))

#### Model categorical wing length data ##### #MOCH

fe.ca1 <- lm(First.Egg ~ Wing_Difference_Category + scale(Elevation)*Year, mochBreed.fe)
fe.ca2 <- lm(First.Egg ~ Wing_Difference_Category + scale(Elevation) + Year, mochBreed.fe)

AIC(fe.ca1,fe.ca2)

##Check residuals
fe.ca2r = simulateResiduals(fe.ca2)
plot(fe.ca2r)
#Look ok

Anova(fe.ca2,test.statistic = "Chisq",type="III")
#Wing length difference not important for first egg date when used as a categorical variable F=0.467, p=0.7
summary(fe.ca2) #interaction not significant so took out

#### Model categorical wing length data ##### BCCH

fe.ca1 <- lm(First.Egg ~ Wing_Difference_Category + scale(Elevation)*Year, bcchBreed.fe)
fe.ca2 <- lm(First.Egg ~ Wing_Difference_Category + scale(Elevation) +Year, bcchBreed.fe)

AIC(fe.ca1, fe.ca2) #

##Check residuals
fe.ca2r = simulateResiduals(fe.ca2)
plot(fe.ca2r)
#looks good

Anova(fe.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for first egg date when used as a categorical variable X2=0.764, p=0.858
summary(fe.ca2)


### Modeling first lay numerically #########################################
#MOCH
fe.ca1 <- lm(First.Egg ~ Wing_Difference + scale(Elevation)*Year, mochBreedN.fe)
fe.ca2 <- lm(First.Egg ~ Wing_Difference + scale(Elevation) + Year, mochBreedN.fe)

AIC(fe.ca1,fe.ca2)

##Check residuals
fe.ca2r = simulateResiduals(fe.ca2)
plot(fe.ca1r)
#Look ok

Anova(fe.ca2,test.statistic = "Chisq",type="III")
#Wing length difference not important for first egg date when used as a categorical variable X2=0.764, p=0.858
summary(fe.ca2)

#BCCH
fe.ca1 <- lm(First.Egg ~ Wing_Difference + scale(Elevation)*Year, bcchBreedN.fe)
fe.ca2 <- lm(First.Egg ~ Wing_Difference + scale(Elevation) + Year, bcchBreedN.fe)

AIC(fe.ca1,fe.ca2)

##Check residuals
fe.ca2r = simulateResiduals(fe.ca2)
plot(fe.ca1r)
#Look ok

Anova(fe.ca2,test.statistic = "Chisq",type="III")
#Wing length difference not important for first egg date when used as a categorical variable X2=0.764, p=0.858
summary(fe.ca2)

### Model clutch size categorical
cs.ca1 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) + Year + scale(First.Egg), data=mochBreed.cs, family = "genpois")
cs.ca2 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) * Year + scale(First.Egg), data=mochBreed.cs, family = "genpois")
cs.ca3 <- glmmTMB(Egg_Number ~ Wing_Difference_Category*scale(Elevation) + scale(Elevation) * Year + scale(First.Egg), data=mochBreed.cs, family = "genpois")

anova(cs.ca1,cs.ca2) #New interaction does not improve
anova(cs.ca1,cs.ca3) #New interaction does not improve

#Check residuals
cs.ca1r = simulateResiduals(cs.ca1)
plot(cs.ca1r)
#look ok

#Results
Anova(cs.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for clutch size when used as a categorical variable X2=0.27, p=0.604
summary(cs.ca1)

### Model clutch size categorical #BCCH
cs.ca1 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) + Year + scale(First.Egg), data=bcchBreed.cs, family = "genpois")
cs.ca2 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) * Year + scale(First.Egg), data=bcchBreed.cs, family = "genpois")
cs.ca3 <- glmmTMB(Egg_Number ~ Wing_Difference_Category*scale(Elevation) + scale(Elevation) * Year + scale(First.Egg), data=bcchBreed.cs, family = "genpois")

anova(cs.ca1,cs.ca2) #New interaction does not improve
anova(cs.ca1,cs.ca3) #New interaction does not improve

#Check residuals
cs.ca1r = simulateResiduals(cs.ca1)
plot(cs.ca1r)
#look good

#Results
Anova(cs.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for clutch size when used as a categorical variable X2=0.27, p=0.604
summary(cs.ca1)

## MODEL CLUTCH SIZE NUMERIC MOCH
cs.ca1 <- glmmTMB(Egg_Number ~ Wing_Difference + scale(Elevation) + Year + scale(First.Egg), data=mochBreedN.cs, family = "genpois")
cs.ca2 <- glmmTMB(Egg_Number ~ Wing_Difference + scale(Elevation) * Year + scale(First.Egg), data=mochBreedN.cs, family = "genpois")
cs.ca3 <- glmmTMB(Egg_Number ~ Wing_Difference*scale(Elevation) + scale(Elevation) * Year + scale(First.Egg), data=mochBreedN.cs, family = "genpois")

anova(cs.ca1,cs.ca2) #New interaction does not improve
anova(cs.ca1,cs.ca3) #New interaction does not improve

#Check residuals
cs.ca1r = simulateResiduals(cs.ca1)
plot(cs.ca1r)
#look ok

#Results
Anova(cs.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for clutch size when used as a categorical variable X2=0.27, p=0.604
summary(cs.ca1)

## MODEL CLUTCH SIZE NUMERIC BCCH
cs.ca1 <- glmmTMB(Egg_Number ~ Wing_Difference + scale(Elevation) + Year + scale(First.Egg), data=bcchBreedN.cs, family = "genpois")
cs.ca2 <- glmmTMB(Egg_Number ~ Wing_Difference + scale(Elevation) * Year + scale(First.Egg), data=bcchBreedN.cs, family = "genpois")
cs.ca3 <- glmmTMB(Egg_Number ~ Wing_Difference*scale(Elevation) + scale(Elevation) * Year + scale(First.Egg), data=bcchBreedN.cs, family = "genpois")

anova(cs.ca1,cs.ca2) #New interaction does not improve
anova(cs.ca1,cs.ca3) #New interaction does not improve

#Check residuals
cs.ca1r = simulateResiduals(cs.ca1)
plot(cs.ca1r)
#look ok

#Results
Anova(cs.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for clutch size when used as a categorical variable X2=0.27, p=0.604
summary(cs.ca1)

#model brood size categorical moch
bs.ca1 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation)*Year, data=mochBreed.bs, family = "genpois")
bs.ca2 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation) + Year, data=mochBreed.bs, family = "genpois")
anova(bs.ca1,bs.ca2)

##Check residuals
bs.ca2r = simulateResiduals(bs.ca2)
plot(bs.ca2r)
#Not bad

##Results
Anova(bs.ca2,test.statistic = "Chisq",type="III")
#No effect of wing length on brood size at low X2=6.23, p=0.1
summary(bs.ca2)

#model brood size categorical bcch
bs.ca1 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation)*Year, data=bcchBreed.bs, family = "genpois")
bs.ca2 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation) + Year, data=bcchBreed.bs, family = "genpois")
anova(bs.ca1,bs.ca2)

##Check residuals
bs.ca2r = simulateResiduals(bs.ca2)
plot(bs.ca2r)
#look good

##Results
Anova(bs.ca2,test.statistic = "Chisq",type="III")
#No effect of wing length on brood size at low X2=6.23, p=0.1
summary(bs.ca2)

#MODEL BROOD SIZE NUMERIC moch
bs.ca1 <- glmmTMB(Nestling_Number ~ Wing_Difference + scale(Elevation)*Year, data=mochBreedN.bs, family = "genpois")
bs.ca2 <- glmmTMB(Nestling_Number ~ Wing_Difference + scale(Elevation) + Year, data=mochBreedN.bs, family = "genpois")
anova(bs.ca1,bs.ca2)

##Check residuals
bs.ca2r = simulateResiduals(bs.ca2)
plot(bs.ca2r)
#Not bad

##Results
Anova(bs.ca2,test.statistic = "Chisq",type="III")
#No effect of wing length on brood size at low X2=6.23, p=0.1
summary(bs.ca2)

#MODEL BROOD SIZE NUMERIC bcch
bs.ca1 <- glmmTMB(Nestling_Number ~ Wing_Difference + scale(Elevation)*Year, data=bcchBreedN.bs, family = "genpois")
bs.ca2 <- glmmTMB(Nestling_Number ~ Wing_Difference + scale(Elevation) + Year, data=bcchBreedN.bs, family = "genpois")
anova(bs.ca1,bs.ca2)

##Check residuals
bs.ca1r = simulateResiduals(bs.ca1)
plot(bs.ca1r)
#Not bad

##Results
Anova(bs.ca2,test.statistic = "Chisq",type="III")
#No effect of wing length on brood size at low X2=6.23, p=0.1
summary(bs.ca1)


### Model mean mass categorical moch
mm.ca1 <- lm(Avg_Nestling_Weight ~ Wing_Difference_Category + scale(Elevation)*Year, data=mochBreed.mm)
mm.ca2 <- lm(Avg_Nestling_Weight ~ Wing_Difference_Category + scale(Elevation)+Year, data=mochBreed.mm)

AIC(mm.ca1, mm.ca2)

#Check Residuals
mm.ca1r = simulateResiduals(mm.ca1)
plot(mm.ca1r)
#look good

#Results
summary(mm.ca1)

### Model mean mass categorical bcch
mm.ca1 <- lm(Avg_Nestling_Weight ~ Wing_Difference_Category + scale(Elevation)*Year, data=bcchBreed.mm)
mm.ca2 <- lm(Avg_Nestling_Weight ~ Wing_Difference_Category + scale(Elevation)+Year, data=bcchBreed.mm)

AIC(mm.ca1, mm.ca2)

#Check Residuals
mm.ca2r = simulateResiduals(mm.ca2)
plot(mm.ca2r)
#look good

#Results
summary(mm.ca2)

#MODELING MEAN MASS NUMERICALLY MOCH
mm.ca1 <- lm(Avg_Nestling_Weight ~ Wing_Difference + scale(Elevation)*Year, data=mochBreedN.mm)
mm.ca2 <- lm(Avg_Nestling_Weight ~ Wing_Difference + scale(Elevation)+Year, data=mochBreedN.mm)

AIC(mm.ca1, mm.ca2)

#Check Residuals
mm.ca1r = simulateResiduals(mm.ca1)
plot(mm.ca1r)
#look good

#Results
summary(mm.ca1)

#MODELING MEAN MASS NUMERICALLY MOCH
mm.ca1 <- lm(Avg_Nestling_Weight ~ Wing_Difference + scale(Elevation)*Year, data=bcchBreedN.mm)
mm.ca2 <- lm(Avg_Nestling_Weight ~ Wing_Difference + scale(Elevation)+Year, data=bcchBreedN.mm)

AIC(mm.ca1, mm.ca2)

#Check Residuals
mm.ca1r = simulateResiduals(mm.ca1)
plot(mm.ca1r)
#look good

#Results
summary(mm.ca1)

##Model Female SMI moch
fs.ca1 <- lm(Female_SMI ~ Wing_Difference_Category + scale(Elevation)*Year, data=mochBreed.fS)
fs.ca2 <- lm(Female_SMI ~ Wing_Difference_Category + scale(Elevation)+Year, data=mochBreed.fS)

AIC(fs.ca1, fs.ca2)

#Check Residuals
fs.ca2r = simulateResiduals(fs.ca2)
plot(fs.ca2r)
#look good

#Results
summary(fs.ca2)

##Model Male SMI moch
ms.ca1 <- lm(Male_SMI ~ Wing_Difference_Category + scale(Elevation)*Year, data=mochBreed.mS)
ms.ca2 <- lm(Male_SMI ~ Wing_Difference_Category + scale(Elevation)+Year, data=mochBreed.mS)

AIC(ms.ca1, ms.ca2)

#Check Residuals
ms.ca2r = simulateResiduals(ms.ca2)
plot(ms.ca2r)
#look good

#Results
summary(ms.ca2)

##Model Female SMI bcch
fs.ca1 <- lm(Female_SMI ~ Wing_Difference_Category + scale(Elevation)*Year, data=bcchBreed.fS)
fs.ca2 <- lm(Female_SMI ~ Wing_Difference_Category + scale(Elevation)+Year, data=bcchBreed.fS)

AIC(fs.ca1, fs.ca2)

#Check Residuals
fs.ca2r = simulateResiduals(fs.ca2)
plot(fs.ca2r)
#look good

#Results
summary(fs.ca2)

##Model Male SMI bcch
ms.ca1 <- lm(Male_SMI ~ Wing_Difference_Category + scale(Elevation)*Year, data=bcchBreed.mS)
ms.ca2 <- lm(Male_SMI ~ Wing_Difference_Category + scale(Elevation)+Year, data=bcchBreed.mS)

AIC(ms.ca1, ms.ca2)

#Check Residuals
ms.ca2r = simulateResiduals(ms.ca2)
plot(ms.ca2r)
#look good

#Results
summary(ms.ca2)

#NUMERIC FEMALE BODY CONDITION MOCH
fs.ca1 <- lm(Female_SMI ~ Wing_Difference + scale(Elevation)*Year, data=mochBreedN.fS)
fs.ca2 <- lm(Female_SMI ~ Wing_Difference + scale(Elevation)+Year, data=mochBreedN.fS)

AIC(fs.ca1, fs.ca2)

#Check Residuals
fs.ca2r = simulateResiduals(fs.ca2)
plot(fs.ca2r)
#look good

#Results
summary(fs.ca2)

#NUMERIC FEMALE BODY CONDITION BCCH
fs.ca1 <- lm(Female_SMI ~ Wing_Difference + scale(Elevation)*Year, data=bcchBreedN.fS)
fs.ca2 <- lm(Female_SMI ~ Wing_Difference + scale(Elevation)+Year, data=bcchBreedN.fS)

AIC(fs.ca1, fs.ca2)

#Check Residuals
fs.ca2r = simulateResiduals(fs.ca2)
plot(fs.ca2r)
#look good

#Results
summary(fs.ca2)

#NUMERIC MALE BODY CONDITION MOCH
ms.ca1 <- lm(Male_SMI ~ Wing_Difference + scale(Elevation)*Year, data=mochBreedN.mS)
ms.ca2 <- lm(Male_SMI ~ Wing_Difference + scale(Elevation)+Year, data=mochBreedN.mS)

AIC(ms.ca1, ms.ca2)

#Check Residuals
ms.ca2r = simulateResiduals(ms.ca2)
plot(ms.ca2r)
#look good

#Results
summary(ms.ca2)
#NUMERIC MALE BODY CONDITION BCCH
ms.ca1 <- lm(Male_SMI ~ Wing_Difference + scale(Elevation)*Year, data=bcchBreedN.mS)
ms.ca2 <- lm(Male_SMI ~ Wing_Difference + scale(Elevation)+Year, data=bcchBreedN.mS)

AIC(ms.ca1, ms.ca2)

#Check Residuals
ms.ca1r = simulateResiduals(ms.ca1)
plot(ms.ca1r)
#look good

#Results
summary(ms.ca2)

#Plots of Categorical ##### MOCH #######

#First egg date
head(mochBreed.fe)
mochBreed.fe.plot <- subset(mochBreed.fe, Wing_Difference_Category %in% c("Slightly", "Much"))


ggplot(data=mochBreed.fe.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=First.Egg)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("First egg date")

#Clutch Size
mochBreed.cs.plot <- subset(mochBreed.cs, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=mochBreed.cs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Egg_Number)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Clutch size")

#Brood size
mochBreed.bs.plot <- subset(mochBreed.bs, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=mochBreed.bs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Nestling_Number)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Brood size")

#Mean nestling mass
mochBreed.mm.plot <- subset(mochBreed.mm, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=mochBreed.mm.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Avg_Nestling_Weight)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Avg nestling mass")


#Female SMI
mochBreed.fs.plot <- subset(mochBreed.fS, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=mochBreed.fs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Female_SMI)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Female body condition")

#Male SMI
mochBreed.ms.plot <- subset(mochBreed.mS, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=mochBreed.ms.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Male_SMI)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Male body condition")


#Plots of Categorical ##### BCCH #######

#First egg date
head(bcchBreed.fe)
bcchBreed.fe.plot <- subset(bcchBreed.fe, Wing_Difference_Category %in% c("Slightly", "Much"))


ggplot(data=bcchBreed.fe.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=First.Egg)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("First egg date")

#Clutch Size
bcchBreed.cs.plot <- subset(bcchBreed.cs, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=bcchBreed.cs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Egg_Number)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Clutch size")

#Brood size
bcchBreed.bs.plot <- subset(bcchBreed.bs, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=bcchBreed.bs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Nestling_Number)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Brood size")

#Mean nestling mass
bcchBreed.mm.plot <- subset(bcchBreed.mm, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=bcchBreed.mm.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Avg_Nestling_Weight)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Avg nestling mass")

#Female SMI
bcchBreed.fs.plot <- subset(bcchBreed.fS, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=bcchBreed.fs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Female_SMI)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Female body condition")

#Male SMI
bcchBreed.ms.plot <- subset(bcchBreed.mS, Wing_Difference_Category %in% c("Slightly", "Much"))

ggplot(data=bcchBreed.ms.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Male_SMI)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Male body condition")

##Plotting numeric data
#first lay MOCH
ggplot(data = mochBreedN.fe, aes(x=Wing_Difference, y= First.Egg))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Day of first lay")

#first lay BCCH
ggplot(data = bcchBreedN.fe, aes(x=Wing_Difference, y= First.Egg))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Day of first lay")

#Clutch size MOCH
ggplot(data = mochBreedN.cs, aes(x=Wing_Difference, y= Egg_Number))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Clutch size")

#Clutch size BCCH
ggplot(data = bcchBreedN.cs, aes(x=Wing_Difference, y= Egg_Number))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Clutch size")

#Brood size MOCH
ggplot(data = mochBreedN.bs, aes(x=Wing_Difference, y= Nestling_Number))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Brood size")


#Brood size BCCH
ggplot(data = bcchBreedN.bs, aes(x=Wing_Difference, y= Nestling_Number))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Brood size")

#Mean nestling mass MOCH
ggplot(data = mochBreedN.mm, aes(x=Wing_Difference, y= Avg_Nestling_Weight))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Mean nestling mass")

#Mean nestling mass BCCH
ggplot(data = bcchBreedN.mm, aes(x=Wing_Difference, y= Avg_Nestling_Weight))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Mean nestling mass")

#Female SMI MOCH
ggplot(data = mochBreedN.fS, aes(x=Wing_Difference, y= Female_SMI))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Female body condition")

#Female SMI BCCH
ggplot(data = bcchBreedN.fS, aes(x=Wing_Difference, y= Female_SMI))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Female body condition")

#Male SMI MOCH
ggplot(data = mochBreedN.mS, aes(x=Wing_Difference, y= Male_SMI))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Male body condition")

#Male BCCH
ggplot(data = bcchBreedN.mS, aes(x=Wing_Difference, y= Male_SMI))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing length difference")+
  ylab("Male body condition")
