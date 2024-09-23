#Code by Sara Padula 
#Sep 13, 2024
#Does female wing length predict male wing length with a quadratic equation

#Also wing length differences and breeding success with species as interaction with wing length difference and doing year as a random effect

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


## Loading data  #########
breed <- read.csv("paired_SMI.csv")

## Scaling elevation variable for each species
breedMOCH <- breed %>%
  filter(Species == "MOCH") %>%
  mutate(scale_ele = scale(Elevation))

breedBCCH <- breed %>%
  filter(Species == "BCCH") %>%
  filter(Elevation < 2000) %>%
  mutate(scale_ele = scale(Elevation))

breed <- rbind(breedMOCH, breedBCCH)

#BREEDING SUCCESS ANALYSES numerical

#first egg, removing blank values
breed.fe <- breed %>% filter(!is.na(First.Egg))

#clutch size, removing blank values
breed.cs <- breed %>% filter(!is.na(Egg_Number))

#brood size, removing blank values
breed.bs <- breed %>% filter(!is.na(Nestling_Number))

#nestling size, removing blank values
breed.mm <- breed %>% filter(!is.na(Avg_Nestling_Weight))

#Female SMI, removing blank values
breed.fs <- breed %>% filter(!is.na(Female_SMI))

#Male SMI, removing blank values
breed.ms <- breed %>% filter(!is.na(Male_SMI))

#Provisioning
breed.p <- breed %>% filter(!is.na(Provisioning))


## Model first egg ######
fe1 <- lmer(First.Egg ~ Wing_Difference*Species + Species*scale_ele + (1|Year), data = breed.fe)
fe2 <- lmer(First.Egg ~ Wing_Difference*Species + scale_ele + (1|Year), data = breed.fe)

AIC(fe1,fe2) #fe1 lower AIC
anova(fe1,fe2) #significant

#Check residuals
fe1r = simulateResiduals(fe1)
plot(fe1r)
#quantile deviation

Anova(fe1,test.statistic = "Chisq",type="III")
summary(fe1) # species, elevation and species*elevation significant


## Model clutch size #######
cs1 <- glmmTMB(Egg_Number ~ Wing_Difference*Species + Species*scale_ele, data =breed.cs,family = "genpois")
cs2 <- glmmTMB(Egg_Number ~ Wing_Difference*Species + scale_ele, data=breed.cs, family="genpois") #got error when including (1|Year) and variance of the random effect was very low

AIC(cs1, cs2) #cs2 lower AIC
anova(cs1,cs2) # not significant

#Check residuals
cs2r = simulateResiduals(cs2)
plot(cs2r)
#look okay

Anova(cs2,test.statistic = "Chisq",type="III")
summary(cs2) #species significant


##Model brood size ######
bs1 <- glmmTMB(Nestling_Number ~ Wing_Difference*Species + Species*scale_ele, data = breed.bs, family="genpois")
bs2 <- glmmTMB(Nestling_Number ~ Wing_Difference*Species + scale_ele, data = breed.bs, family = "genpois") #got error when including (1|Year) and variance of the random effect was very low

AIC(bs1,bs2) #bs2 lower AIC
anova(bs1,bs2) #not significant

#check residuals
bs2r = simulateResiduals(bs2)
plot(bs2r)
#look good

#Results
Anova(bs2,test.statistic = "Chisq",type="III")
summary(bs2) #species and wing_difference*species significant

##Model nestling mass #####
mm1 <- lmer(Avg_Nestling_Weight ~ Wing_Difference*Species + Species*scale_ele + Nestling_Number + (1|Year), data = breed.mm)
mm2 <- lmer(Avg_Nestling_Weight ~ Wing_Difference*Species + scale_ele + Nestling_Number + (1|Year), data = breed.mm)
mm3 <- lmer(Avg_Nestling_Weight ~ Wing_Difference*Species + Species*scale_ele + (1|Year), data = breed.mm)
mm4 <- lmer(Avg_Nestling_Weight ~ Wing_Difference*Species + scale_ele + (1|Year), data = breed.mm)

AIC(mm1, mm2, mm3, mm4) #mm3 lowest AIC
anova(mm1,mm2, mm3, mm4) #significant

#check residuals
mm3r = simulateResiduals(mm3)
plot(mm3r)
#some deviation

#results
Anova(mm3,test.statistic = "Chisq",type="III")
summary(mm3) #wing_difference*species significant


## Model Female SMI ##
fs1 <- lmer(Female_SMI ~ Wing_Difference*Species + Species*scale_ele + (1|Year), data =  breed.fs)
fs2 <- lmer(Female_SMI ~ Wing_Difference*Species + scale_ele + (1|Year), data = breed.fs)

AIC(fs1, fs2) #fs1 lower AIC
anova(fs1,fs2) #significant

#check residuals
fs1r = simulateResiduals(fs1)
plot(fs1r)
#look okay

#results
Anova(fs1,test.statistic = "Chisq",type="III")
summary(fs1) #species*elevation significant, elevation almost significant

## Model Male SMI ##
ms1 <- lmer(Male_SMI ~ Wing_Difference*Species + Species*scale_ele + (1|Year), data =  breed.ms)
ms2 <- lmer(Male_SMI ~ Wing_Difference*Species + scale_ele + (1|Year), data = breed.ms)

AIC(ms1, ms2) #ms2 lower AIC
anova(ms1,ms2) #not significant

#check residuals
ms2r = simulateResiduals(ms2)
plot(ms2r)
#look good

#results
Anova(ms2,test.statistic = "Chisq",type="III")
summary(ms2) #wing_difference significant

## Model Provisioning ##
p1 <- lmer(Provisioning ~ Wing_Difference*Species + Species*scale_ele + (1|Year), data =  breed.p)
p2 <- lmer(Provisioning ~ Wing_Difference*Species + scale_ele + (1|Year), data = breed.p)

AIC(p1, p2) #p1 lower AIC
anova(p1,p2) #not significant

#check residuals
p1r = simulateResiduals(p1)
plot(p1r)
#quantile deviations

#results
Anova(p1,test.statistic = "Chisq",type="III")
summary(p1) #nothing significant



### Determining if female wing length predicts male wing length within pairs

#subsetting data
mochBreed <- subset(breed, Species == "MOCH")
bcchBreed <- subset(breed, Species == "BCCH")

#create new variable for female wing chord

mochBreed$Female.Wing.Chord2 <- mochBreed$Female.Wing.Chord^2

bcchBreed$Female.Wing.Chord2 <- bcchBreed$Female.Wing.Chord^2

breed$Female.Wing.Chord2 <- breed$Female.Wing.Chord^2

#Does female wing predict male wing in MOCH

quadraticMoch <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Female.Wing.Chord2, data = mochBreed)
lmMoch <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data = mochBreed)
nullMoch <- lm(Male.Wing.Chord ~ 1, data = mochBreed)

summary(quadraticMoch)
summary(lmMoch)
AIC(quadraticMoch,lmMoch) #quadratic has a higher AIC score
anova(quadraticMoch,lmMoch)
anova(nullMoch,quadraticMoch)

#lm and quadratic both not significant

quadraticBcch <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Female.Wing.Chord2, data = bcchBreed)
lmBcch <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data = bcchBreed)
nullBcch <- lm(Male.Wing.Chord ~ 1, data = bcchBreed)
summary(quadraticBcch)
summary(lmBcch)
AIC(quadraticBcch, lmBcch) #quadratic has a lower AIC score
anova(lmBcch,quadraticBcch)

#lm and quadratic both not significant

################# Running quadratic on both species
quadraticboth <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Female.Wing.Chord2, data = breed)

lmboth <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data = breed)

AIC(quadraticboth,lmboth) #quadratic lower
anova(quadraticboth,lmboth) #not significant

summary(quadraticboth) #nothing significant



######## redoing breeding success models but with categorical wing difference ###################

breedCat <- subset(breed, Wing_Difference_Category %in% c("Slightly", "Much"))

#first egg, removing blank values
breedCat.fe <- breedCat %>% filter(!is.na(First.Egg))

#clutch size, removing blank values
breedCat.cs <- breedCat %>% filter(!is.na(Egg_Number))

#brood size, removing blank values
breedCat.bs <- breedCat %>% filter(!is.na(Nestling_Number))

#nestling size, removing blank values
breedCat.mm <- breedCat %>% filter(!is.na(Avg_Nestling_Weight))

#Female SMI, removing blank values
breedCat.fs <- breedCat %>% filter(!is.na(Female_SMI))

#Male SMI, removing blank values
breedCat.ms <- breedCat %>% filter(!is.na(Male_SMI))

#Provisioning
breedCat.p <- breedCat %>% filter(!is.na(Provisioning))

## Model first egg ######
fec1 <- lmer(First.Egg ~ Wing_Difference_Category*Species + Species*scale_ele + (1|Year), data = breedCat.fe)
fec2 <- lmer(First.Egg ~ Wing_Difference_Category*Species + scale_ele + (1|Year), data = breedCat.fe)

AIC(fec1,fec2) #fe1 lower AIC
anova(fec1,fec2) #significant

#Check residuals
fec1r = simulateResiduals(fec1)
plot(fec1r)
#looks good

Anova(fec1,test.statistic = "Chisq",type="III")
summary(fec1) #species, elevation, and species*elevation significant
