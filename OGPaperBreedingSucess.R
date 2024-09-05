## Wing length differences and breeding success

#Modeling whether wing length difference influences breeding success

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
summary(fe.ca1)

#### Model categorical wing length data ##### BCCH

fe.ca1 <- lm(First.Egg ~ Wing_Difference_Category + scale(Elevation)*Year, bcchBreed.fe)
fe.ca2 <- lm(First.Egg ~ Wing_Difference_Category + Year, bcchBreed.fe)

AIC(fe.ca1, fe.ca2) #

##Check residuals
fe.ca2r = simulateResiduals(fe.ca2)
plot(fe.ca2r)
#doesn't look good

Anova(fe.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for first egg date when used as a categorical variable X2=0.764, p=0.858
summary(fe.ca1)


### Model clutch size categorical
cs.ca1 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) + Year + scale(First.Egg), data=mochBreed.cs, family = "genpois")
cs.ca2 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) * Year + scale(First.Egg), data=mochBreed.cs, family = "genpois")
cs.ca3 <- glmmTMB(Egg_Number ~ Wing_Difference_Category*scale(Elevation) + scale(Elevation) * Year + scale(First.Egg), data=mochBreed.cs, family = "genpois")

anova(cs.ca1,cs.ca2) #New interaction does not improve
anova(cs.ca1,cs.ca3) #New interaction does not improve

#Check residuals
cs.ca2r = simulateResiduals(cs.ca2)
plot(cs.ca2r)

#Results
Anova(cs.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for clutch size when used as a categorical variable X2=0.27, p=0.604
summary(cs.ca1)

### Model clutch size categorical
cs.ca1 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) + Year + scale(First.Egg), data=bcchBreed.cs, family = "genpois")
cs.ca2 <- glmmTMB(Egg_Number ~ Wing_Difference_Category + scale(Elevation) * Year + scale(First.Egg), data=bcchBreed.cs, family = "genpois")
cs.ca3 <- glmmTMB(Egg_Number ~ Wing_Difference_Category*scale(Elevation) + scale(Elevation) * Year + scale(First.Egg), data=bcchBreed.cs, family = "genpois")

anova(cs.ca1,cs.ca2) #New interaction does not improve
anova(cs.ca1,cs.ca3) #New interaction does not improve

#Check residuals
cs.ca1r = simulateResiduals(cs.ca1)
plot(cs.ca1r)

#Results
Anova(cs.ca1,test.statistic = "Chisq",type="III")
#Wing length difference not important for clutch size when used as a categorical variable X2=0.27, p=0.604
summary(cs.ca1)


#model brood size categorical moch
bs.ca1 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation)*Year, data=mochBreed.bs, family = "genpois")
bs.ca2 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation) + Year, data=mochBreed.bs, family = "genpois")
anova(bs.ca1,bs.ca2)

##Check residuals
bs.ca1r = simulateResiduals(bs.ca1)
plot(bs.ca1r)
#Not bad

##Results
Anova(bs.ca1,test.statistic = "Chisq",type="III")
#No effect of wing length on brood size at low X2=6.23, p=0.1
summary(bs.ca1)

#model brood size categorical bcch
bs.ca1 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation)*Year, data=bcchBreed.bs, family = "genpois")
bs.ca2 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category + scale(Elevation) + Year, data=bcchBreed.bs, family = "genpois")
anova(bs.ca1,bs.ca2)

##Check residuals
bs.ca1r = simulateResiduals(bs.ca1)
plot(bs.ca1r)
#Not bad

##Results
Anova(bs.ca1,test.statistic = "Chisq",type="III")
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
mm.ca1r = simulateResiduals(mm.ca1)
plot(mm.ca1r)
#look good

#Results
summary(mm.ca1)

#Plots of Categorical
head(mochBreed.fe)
mochBreed.fe.plot = mochBreed.fe %>% mutate(Wing_Difference_Category = factor(Wing_Difference_Category, levels = c("Slightly","Much")))

a = ggplot(data=mochBreed.fe.plot,aes(x=Wing_Difference_Category,y=First.Egg)) +
  geom_boxplot(aes(fill=wing.diff.c),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("light blue","maroon")) +
  xlab("") + ylab("First egg date")
