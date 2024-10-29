#Code by Sara Padula 
#Sep 29, 2024
#Does female tarsus length predict male tarsus length with a quadratic equation

#does tarsus difference predict breeding variables

library(dplyr)
library(ggplot2)
install.packages("cowplot")
library(cowplot)
library(lme4)
install.packages("lmerTest")
library(lmerTest)
install.packages("DHARMa")
library(DHARMa)
install.packages("glmmTMB")
library(glmmTMB)
install.packages("car")
library(car)
install.packages("emmeans")
library(emmeans)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggpdist")
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
head(breed)

nrow(breed)

nrow(breedMOCH)

nrow(breedBCCH)

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
fe1 <- lm(First.Egg ~ Tarsus_Difference*Species + Species*scale_ele, data = breed.fe)
fe2 <- lm(First.Egg ~ Tarsus_Difference*Species + scale_ele, data = breed.fe)

AIC(fe1,fe2) #fe1 lower AIC
anova(fe1,fe2) #significant

#Check residuals
fe1r = simulateResiduals(fe1)
plot(fe1r)
#quantile deviation

summary(fe1) # species*elevation significant


## Model clutch size #######
cs1 <- glmmTMB(Egg_Number ~ Tarsus_Difference*Species + Species*scale_ele, data =breed.cs,family = "genpois")
cs2 <- glmmTMB(Egg_Number ~ Tarsus_Difference*Species + scale_ele, data=breed.cs, family="genpois") #got error when including (1|Year) and variance of the random effect was very low

AIC(cs1, cs2) #cs2 lower AIC
anova(cs1,cs2) # not significant

#Check residuals
cs1r = simulateResiduals(cs1)
plot(cs1r)
#look okay

Anova(cs2,test.statistic = "Chisq",type="III")
summary(cs2) #species significant


##Model brood size ######
bs1 <- glmmTMB(Nestling_Number ~ Tarsus_Difference*Species + Species*scale_ele, data = breed.bs, family="genpois")
bs2 <- glmmTMB(Nestling_Number ~ Tarsus_Difference*Species + scale_ele, data = breed.bs, family = "genpois") #got error when including (1|Year) and variance of the random effect was very low

AIC(bs1,bs2) #bs2 lower AIC
anova(bs1,bs2) #not significant

#check residuals
bs2r = simulateResiduals(bs2)
plot(bs2r)
#look good

#Results
Anova(bs2,test.statistic = "Chisq",type="III")
summary(bs2) #species significant

##Model nestling mass #####
mm1 <- lmer(Avg_Nestling_Weight ~ Tarsus_Difference*Species + Species*scale_ele + Nestling_Number + (1|Year), data = breed.mm)
mm2 <- lmer(Avg_Nestling_Weight ~ Tarsus_Difference*Species + scale_ele + Nestling_Number + (1|Year), data = breed.mm)
mm3 <- lmer(Avg_Nestling_Weight ~ Tarsus_Difference*Species + Species*scale_ele + (1|Year), data = breed.mm)
mm4 <- lmer(Avg_Nestling_Weight ~ Tarsus_Difference*Species + scale_ele + (1|Year), data = breed.mm)

AIC(mm1, mm2, mm3, mm4) #mm3 lowest AIC
anova(mm1,mm2, mm3, mm4) #significant

#check residuals
mm3r = simulateResiduals(mm3)
plot(mm3r)
#some deviation

#results
Anova(mm3,test.statistic = "Chisq",type="III")
summary(mm3) #species significant


## Model Female SMI ##
fs1 <- lmer(Female_SMI ~ Tarsus_Difference*Species + Species*scale_ele + (1|Year), data =  breed.fs)
fs2 <- lmer(Female_SMI ~ Tarsus_Difference*Species + scale_ele + (1|Year), data = breed.fs)

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
ms1 <- lmer(Male_SMI ~ Tarsus_Difference*Species + Species*scale_ele + (1|Year), data =  breed.ms)
ms2 <- lmer(Male_SMI ~ Tarsus_Difference*Species + scale_ele + (1|Year), data = breed.ms)

AIC(ms1, ms2) #ms2 lower AIC
anova(ms1,ms2) #not significant

#check residuals
ms2r = simulateResiduals(ms2)
plot(ms2r)
#look good

#results
Anova(ms2,test.statistic = "Chisq",type="III")
summary(ms2) #nothing significant

## Model Provisioning ##
p1 <- lmer(Provisioning ~ Tarsus_Difference*Species + Species*scale_ele + (1|Year), data =  breed.p)
p2 <- lmer(Provisioning ~ Tarsus_Difference*Species + scale_ele + (1|Year), data = breed.p)

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

mochBreed$Female.Tarsus2 <- mochBreed$Female.Tarsus^2

bcchBreed$Female.Tarsus2 <- bcchBreed$Female.Tarsus^2

breed$Female.Tarsus2 <- breed$Female.Tarsus^2

#Does female wing predict male wing in MOCH

quadraticMoch <- lm(Male.Tarsus ~ Female.Tarsus + Female.Tarsus2 + MaleBander, data = mochBreed)
lmMoch <- lm(Male.Tarsus ~ Female.Tarsus + MaleBander, data = mochBreed)
nullMoch <- lm(Male.Tarsus ~ 1, data = mochBreed)

summary(quadraticMoch)
summary(lmMoch)
AIC(quadraticMoch,lmMoch) #quadratic has a lower AIC score
anova(quadraticMoch,lmMoch)
anova(nullMoch,quadraticMoch)
plot(mochBreed$Female.Wing.Chord, mochBreed$Male.Wing.Chord)
range(mochBreed$Female.Tarsus)
#plotting the quadratic model
fwingvalues <- seq(14.3,21.5,0.1)

predictedValue <- predict(quadraticMoch,list(Female.Tarsus = fwingvalues, Female.Tarsus2 = fwingvalues^2))

plot(mochBreed$Female.Tarsus, mochBreed$Male.Tarsus, pch=16, xlab="Female Wing Chord (mm)", ylab="Male Wing Chord (mm)", cex.lab=1.3)

lines(fwingvalues,predictedValue,lwd=3)
#lm and quadratic both not significant

quadraticBcch <- lm(Male.Tarsus ~ Female.Tarsus + Female.Tarsus2 + MaleBander, data = bcchBreed)
lmBcch <- lm(Male.Tarsus ~ Female.Tarsus + MaleBander, data = bcchBreed)
nullBcch <- lm(Male.Tarsus ~ 1, data = bcchBreed)
summary(quadraticBcch)
summary(lmBcch)
AIC(quadraticBcch, lmBcch) #quadratic has a lower AIC score
anova(lmBcch,quadraticBcch)
range(bcchBreed$Female.Wing.Chord)

#plotting the quadratic model
fwingvalues1 <- seq(60,70,0.1)

predictedValue1 <- predict(quadraticBcch,list(Female.Wing.Chord = fwingvalues1, Female.Wing.Chord2 = fwingvalues1^2))

plot(bcchBreed$Female.Wing.Chord, bcchBreed$Male.Wing.Chord, pch=16, xlab="Female Wing Chord (mm)", ylab="Male Wing Chord (mm)", cex.lab=1.3)

lines(fwingvalues1,predictedValue1,lwd=3)
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

## Model clutch size #######
cs1 <- glmmTMB(Egg_Number ~ Wing_Difference_Category*Species + Species*scale_ele, data =breedCat.cs,family = "genpois")
cs2 <- glmmTMB(Egg_Number ~ Wing_Difference_Category*Species + scale_ele, data=breedCat.cs, family="genpois") #got error when including (1|Year) and variance of the random effect was very low

AIC(cs1, cs2) #cs2 lower AIC
anova(cs1,cs2) # not significant

#Check residuals
cs2r = simulateResiduals(cs2)
plot(cs2r)
#look okay

Anova(cs2,test.statistic = "Chisq",type="III")
summary(cs2) #species significant

##Model brood size ######
bs1 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category*Species + Species*scale_ele, data = breedCat.bs, family="genpois")
bs2 <- glmmTMB(Nestling_Number ~ Wing_Difference_Category*Species + scale_ele, data = breedCat.bs, family = "genpois") #got error when including (1|Year) and variance of the random effect was very low

AIC(bs1,bs2) #bs2 lower AIC
anova(bs1,bs2) #not significant

#check residuals
bs2r = simulateResiduals(bs2)
plot(bs2r)
#look ok

#Results
Anova(bs2,test.statistic = "Chisq",type="III")
summary(bs1) #nothing significant


##Model nestling mass #####

mm3 <- lmer(Avg_Nestling_Weight ~ Wing_Difference_Category*Species + Species*scale_ele + (1|Year), data = breedCat.mm)
mm4 <- lmer(Avg_Nestling_Weight ~ Wing_Difference_Category*Species + scale_ele + (1|Year), data = breedCat.mm)

AIC(mm3, mm4) #mm3 lowest AIC
anova( mm3, mm4) #significant

#check residuals
mm3r = simulateResiduals(mm3)
plot(mm3r)
#some deviation

#results
Anova(mm3,test.statistic = "Chisq",type="III")
summary(mm3) #wing_difference*species significant

## Model Female SMI ##
fs1 <- lmer(Female_SMI ~ Wing_Difference_Category*Species + Species*scale_ele + (1|Year), data =  breedCat.fs)
fs2 <- lmer(Female_SMI ~ Wing_Difference_Category*Species + scale_ele + (1|Year), data = breedCat.fs)

AIC(fs1, fs2) #fs1 lower AIC
anova(fs1,fs2) #significant

#check residuals
fs1r = simulateResiduals(fs1)
plot(fs1r)
#look okay

#results
Anova(fs1,test.statistic = "Chisq",type="III")
summary(fs1) #species*elevation significant

## Model Male SMI ##
ms1 <- lmer(Male_SMI ~ Wing_Difference_Category*Species + Species*scale_ele + (1|Year), data =  breedCat.ms)
ms2 <- lmer(Male_SMI ~ Wing_Difference_Category*Species + scale_ele + (1|Year), data = breedCat.ms)

AIC(ms1, ms2) #ms2 lower AIC
anova(ms1,ms2) #not significant

#check residuals
ms2r = simulateResiduals(ms2)
plot(ms2r)
#look good

#results
Anova(ms2,test.statistic = "Chisq",type="III")
summary(ms2) #wing_difference almost significant

## Model Provisioning ##
p1 <- lmer(Provisioning ~ Wing_Difference_Category*Species + Species*scale_ele + (1|Year), data =  breedCat.p)
p2 <- lmer(Provisioning ~ Wing_Difference_Category*Species + scale_ele + (1|Year), data = breedCat.p)

AIC(p1, p2) #p1 lower AIC
anova(p1,p2) #not significant

#check residuals
p1r = simulateResiduals(p1)
plot(p1r)
#quantile deviations

#results
Anova(p1,test.statistic = "Chisq",type="III")
summary(p1) #nothing significant


#making box plot comparisons
#moch brood size ##
mochBreed.bs.plot <- subset(breed.bs, Wing_Difference_Category %in% c("Slightly", "Much")) %>%
  subset(Species %in% c("MOCH"))

ggplot(data=mochBreed.bs.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Nestling_Number)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Brood size")


#moch mean nestling mass ##
mochBreed.mm.plot <- subset(breed.mm, Wing_Difference_Category %in% c("Slightly", "Much")) %>%
  subset(Species %in% c("MOCH"))

ggplot(data=mochBreed.mm.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Avg_Nestling_Weight)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Mean nestling mass")


#moch male SMI ##
mochBreed.ms.plot <- subset(breed.ms, Wing_Difference_Category %in% c("Slightly", "Much")) %>%
  subset(Species %in% c("MOCH"))

ggplot(data=mochBreed.ms.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Male_SMI)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Male body condition")


#bcch male SMI ##
bcchBreed.ms.plot <- subset(breed.ms, Wing_Difference_Category %in% c("Slightly", "Much")) %>%
  subset(Species %in% c("BCCH"))

ggplot(data=bcchBreed.ms.plot,aes(x=factor(Wing_Difference_Category, levels = c("Slightly","Much")),y=Male_SMI)) +
  geom_boxplot(aes(fill=Wing_Difference_Category),outlier.alpha = 0) +
  geom_point(position=position_jitter(width=0.1,height=0),alpha=0.6) +
  theme_cowplot() + theme(legend.position = "") + scale_fill_manual(values=c("gray","orange")) +
  xlab("") + ylab("Male body condition")


#plot breed.bs
moch.bs.plot <- subset(breed.bs, Species %in% c("MOCH"))

ggplot(data = moch.bs.plot, aes(x=Wing_Difference, y= Nestling_Number))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing difference")+
  ylab("Brood size")

#plot mean mass
moch.mm.plot <- subset(breed.mm, Species %in% c("MOCH"))

ggplot(data = moch.mm.plot, aes(x=Wing_Difference, y= Avg_Nestling_Weight))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing difference")+
  ylab("Mean nestling mass (g)")

#plot mean mass
moch.ms.plot <- subset(breed.ms, Species %in% c("MOCH"))

ggplot(data = moch.ms.plot, aes(x=Wing_Difference, y= Male_SMI))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing difference")+
  ylab("Male body condition")

#plot mean mass
bcch.ms.plot <- subset(breed.ms, Species %in% c("BCCH"))

ggplot(data = bcch.ms.plot, aes(x=Wing_Difference, y= Male_SMI))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Wing difference")+
  ylab("Male body condition")

