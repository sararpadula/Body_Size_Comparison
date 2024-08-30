#reading in the data
bodySize <- read.csv("paired_SMI.csv")
bodySizeSexPop <- read.csv("population_SMI.csv")

#how many unique pairs are there?
nrow(bodySize)
length(unique(bodySize$Pair.ID))

#how many unique individuals are there in the population we sampled?
nrow(bodySizeSexPop)
length(unique(bodySizeSexPop$ID))

#First let's determine if there is truly a sexual size dimorphism within black-capped and mountain chickadees
#ensuring categorical variables are read in as factors
library(lme4)
bodySizeSexPop$Bander <- as.factor(bodySizeSexPop$Bander)
bodySizeSexPop$Year <- as.factor(bodySizeSexPop$Year)
bodySizeSexPop$ID <- as.factor(bodySizeSexPop$ID)
bodySizeSexPop$Location <- as.factor(bodySizeSexPop$Location)

#linear mixed model to determine if Wing length is predicted by sex and species with ID as random effect and bander, year, location as fixed effects. Population level analysis
head(bodySizeSexPop)
wingPop1 <- lmer(Wing.Chord ~ Sex + Species + Bander + Year + Location + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop2 <- lmer(Wing.Chord ~ Sex*Species + Bander + Year + Location + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop3 <- lmer(Wing.Chord ~ Sex + Species + Bander + Location + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop4 <- lmer(Wing.Chord ~ Sex*Species + Bander + Location + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop5 <- lmer(Wing.Chord ~ Sex + Species + Year + Location + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop6 <- lmer(Wing.Chord ~ Sex*Species + Year + Location + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop7 <- lmer(Wing.Chord ~ Sex*Species + (1|ID), data = bodySizeSexPop, REML = FALSE)
wingPop8 <- lmer(Wing.Chord ~Sex*Species + Bander + (1|ID), data = bodySizeSexPop, REML = FALSE)

pop.null <- lmer(Wing.Chord ~ Bander + (1|ID), data = bodySizeSexPop, REML = FALSE)

aic_results <- AIC(wingPop1, wingPop2, wingPop3, wingPop4, wingPop5, wingPop6, wingPop7, wingPop8)
print(aic_results)

summary(wingPop8)
summary(pop.null)

anova(pop.null, wingPop8)

#check the residuals on these models
hist(resid(wingPop8))

plot(wingPop2)

#there is a sexual size dimorphism in black-capped and mountain chickadees and MOCH have longer wings than BCCH
#now I want to know if there is a relationship between female wing length and male wing length within pairs, which would inform if fmelaes specifically mate with males with longer wings
#subset by species

#ensure categorical variables are factors
bodySize$MaleBander <- as.factor(bodySize$MaleBander)
bodySize$FemaleBander <- as.factor(bodySize$FemaleBander)
bodySize$Elevation <- as.numeric(bodySize$Elevation)
bodySize$Species <- as.factor(bodySize$Species)

femaleWing1 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Species + MaleBander + FemaleBander + Elevation, data = bodySize)
femaleWing2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Species + Elevation, data = bodySize)
femaleWing3 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Species + MaleBander +FemaleBander, data = bodySize)
femalewing.null <- lm(Male.Wing.Chord ~ Elevation, data = bodySize)

summary(femaleWing1)
summary(femaleWing2)
summary(femaleWing3)

aic_results <- AIC(femaleWing1, femaleWing2, femaleWing3)

print(aic_results)

anova(femalewing.null, femaleWing2)

#checking the fit for the residuals
fwingresids <- femaleWing2$residuals
{
  qqnorm(fwingresids)
  qqline(fwingresids)
}

#creating a graph to represent this relationship
library(ggplot2)
library(ggpubr)

female_predict_male <- ggplot(data = bodySize, aes(x=Male.Wing.Chord, y=Female.Wing.Chord))+
  geom_point(aes(fill = Species), color = "black", position = "jitter", shape=21, size=4)+
  geom_abline(slope = femaleWing2$coefficients[2], intercept = femaleWing2$coefficients[1], linewidth = 2)+
  theme_classic()+
  xlab("Female wing length (mm)")+
  ylab("Male wing length (mm)")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5))+
  scale_fill_manual(values=c("#0072b2", "#cc79a7"))+
  scale_color_manual(values=c("#0072b2", "#cc79a7"))+
  theme(legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.title = element_text(size=13),
        legend.text = element_text(size =12))+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5))+
  theme(text = element_text(family = "sans"))+
  stat_regline_equation(label.x=62, label.y=66, size = 8)

#seeing if there is a stronger relationship if I split by species

MOCH <- subset(bodySize, Species == "MOCH")
BCCH <- subset(bodySize, Species == "BCCH")
nrow(MOCH)
nrow(BCCH)

#ensure categorical variables are factors
MOCH$MaleBander <- as.factor(MOCH$MaleBander)
MOCH$FemaleBander <- as.factor(MOCH$FemaleBander)
MOCH$Elevation <- as.numeric(MOCH$Elevation)

BCCH$MaleBander <- as.factor(BCCH$MaleBander)
BCCH$FemaleBander <- as.factor(BCCH$FemaleBander)
BCCH$Elevation <- as.numeric(BCCH$Elevation)


mochFemale1 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander + FemaleBander + Elevation, data = MOCH)
mochFemale2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Elevation, data = MOCH)
mochFemale3 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander + FemaleBander, data = MOCH)
mochFemale4 <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data = MOCH)

summary(mochFemale1)
summary(mochFemale2)
summary(mochFemale3)
summary(mochFemale4)

bcchFemale1 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander + FemaleBander + Elevation, data = BCCH)
bcchFemale2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Elevation, data = BCCH)
bcchFemale3 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander + FemaleBander, data = BCCH)
bcchFemale4 <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data = BCCH)

summary(bcchFemale1)
summary(bcchFemale2)
summary(bcchFemale3)
summary(bcchFemale4)

aic_results <- AIC(bcchFemale1, bcchFemale2, bcchFemale3, bcchFemale4)

print(aic_results)

#in these above analyses, I subsetted the data based on species and I found that there is the hint of a significant relationship for black capped chickadees but no relationship at all for MOCH... this supports the pattern of moch pairing with males slightly larger and bcch pairing with males much larger... interesting

bcch_f_predict_m <- ggplot(data = BCCH, aes(x=Male.Wing.Chord, y=Female.Wing.Chord))+
  geom_point(color = "black", position = "jitter", shape=21, size=4)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  xlab("Female wing length (mm)")+
  ylab("Male wing length (mm)")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5))+
  theme(text = element_text(family = "sans"))

moch_f_predict_m <- ggplot(data = MOCH, aes(x=Male.Wing.Chord, y=Female.Wing.Chord))+
  geom_point(color = "black", position = "jitter", shape=21, size=4)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic()+
  xlab("Female wing length (mm)")+
  ylab("Male wing length (mm)")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5))+
  theme(text = element_text(family = "sans"))

#now let's look at the wing difference

#calculating median diff between male and female wing length
medianWing <- median(bodySize$Wing_Difference)

wing_difference_by_species <-ggplot(bodySize, aes(x=Wing_Difference, fill=Species))+
  geom_density(alpha=0.5)+
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=Species), linetype="dashed")+
  labs (x="Difference Between Male and Female Wing Chord (mm)", y="Density")+
  theme_classic()+
  scale_fill_manual(values=c("#0072b2", "#cc79a7"))+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_continuous(limits=c(-6,12), breaks=seq(-6,12,2))+
  geom_vline(aes(xintercept =0), color = "black", linewidth = 1)+
  geom_vline(aes(xintercept =medianWing), color = "black", linewidth = 1, linetype="dashed")+
  theme(legend.position = c(0.9, 0.6))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 16))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.title = element_text(size=13),
        legend.text = element_text(size =12))

wing_difference_by_species


# seeing if adult wing length and adult tarsus length predict nestling wing length / tarsus length
head(bodySize)
MOCH <- subset(bodySize, Species == "MOCH")
BCCH <- subset(bodySize, Species == "BCCH")
mochwingPredictsNest <- lm(Avg.Nestling.Wing ~ Male.Wing.Chord+Female.Wing.Chord, data = MOCH)
bcchwingPredictsNest <- lm(Avg.Nestling.Wing ~ Male.Wing.Chord+Female.Wing.Chord, data = BCCH)
summary(mochwingPredictsNest)
summary(bcchwingPredictsNest)

maleTarsus <- lm(Male.Tarsus ~ MaleBander, data = MOCH)
femaleTarsus <- lm(Female.Tarsus ~ FemaleBander, data = MOCH)
summary(maleTarsus)
summary(femaleTarsus)

nestlingtarsusM <- lm(Avg.Nestling.Tarsus ~ Male.Tarsus + Female.Tarsus, data=MOCH)
nestlingtarsusB <- lm(Avg.Nestling.Tarsus ~ Male.Tarsus + Female.Tarsus, data=BCCH)
summary(nestlingtarsusM)
summary(nestlingtarsusB)


#let's see if wing length difference is a predictor of any variables
MOCH$Year <- as.factor(MOCH$Year)
BCCH$Year <- as.factor(BCCH$Year)
#first we must check that the variance is less than the mean (it is)
var(na.omit(bodySize$Egg_Number))
mean(na.omit(bodySize$Egg_Number))

library(glmmTMB)
mochEggNumber1 <- glmmTMB(Egg_Number ~ Wing_Difference + Elevation, family = poisson, data = MOCH)
mochEggNumber2 <- glmmTMB(Egg_Number ~ Wing_Difference + Year, family = poisson, data = MOCH)
mochEggNumber3 <- glmmTMB(Egg_Number ~ Wing_Difference, data = MOCH)

AIC(mochEggNumber1, mochEggNumber2, mochEggNumber3)
summary(mochEggNumber3)

bcchEggNumber1 <- glmmTMB(Egg_Number ~ Wing_Difference + Elevation, family = poisson, data = BCCH)
bcchEggNumber2 <- glmmTMB(Egg_Number ~ Wing_Difference + Year, family = poisson, data = BCCH)
bcchEggNumber3 <- glmmTMB(Egg_Number ~ Wing_Difference, data = BCCH)

AIC(bcchEggNumber1, bcchEggNumber2, bcchEggNumber3)
summary(bcchEggNumber3)

#Checking the residuals of the model
{
  plot(fitted(mochEggNumber3),residuals(mochEggNumber3,"deviance"))
  abline(h=0,lty=2)
}
{
  plot(fitted(mochEggNumber3), na.omit(MOCH$Egg_Number))
  abline(a=0,b=1)
  cor(fitted(mochEggNumber3), na.omit(MOCH$Egg_Number))
}

#^^ we find that there is no relationship between clutch size and the difference between female and male wing length between pairs. Now, I want to test if this is true for average nestling weight as well.

#Testing different models for the effect of wing difference on average nestling weight
mochAvgNWeight1 <- lm(Avg_Nestling_Weight ~ Wing_Difference, data = MOCH)
mochAvgNWeight2 <- lm(Avg_Nestling_Weight ~ Wing_Difference + Year, data = MOCH)
mochAvgNWeight3 <- lm(Avg_Nestling_Weight ~ Wing_Difference + Elevation, data = MOCH)
mochAvgNWeight4 <- lm(Avg_Nestling_Weight ~ Wing_Difference + Elevation + Year, data = MOCH)

summary(mochAvgNWeight1)
summary(mochAvgNWeight2)
summary(mochAvgNWeight3)
summary(mochAvgNWeight4)

mochAvgNWeight.null <- lm(Avg_Nestling_Weight ~ 1, data = MOCH)

anova(mochAvgNWeight.null,mochAvgNWeight3)

#Testing different models for the effect of wing difference on average nestling weight
bcchAvgNWeight1 <- lm(Avg_Nestling_Weight ~ Wing_Difference, data = BCCH)
bcchAvgNWeight2 <- lm(Avg_Nestling_Weight ~ Wing_Difference + Year, data = BCCH)
bcchAvgNWeight3 <- lm(Avg_Nestling_Weight ~ Wing_Difference + Elevation, data = BCCH)
bcchAvgNWeight4 <- lm(Avg_Nestling_Weight ~ Wing_Difference + Elevation + Year, data = BCCH)

summary(bcchAvgNWeight1)
summary(bcchAvgNWeight2)
summary(bcchAvgNWeight3)
summary(bcchAvgNWeight4)

bcchAvgNWeight.null <- lm(Avg_Nestling_Weight ~ 1, data = BCCH)

anova(bcchAvgNWeight.null,bcchAvgNWeight2)

#testing different models for the effect of wing difference on SMI
head(MOCH)
mochSMI1 <- lmer(Male_SMI ~ Wing_Difference +(1|Male.ID), data = MOCH)
mochSMI2 <- lmer(Male_SMI ~ Wing_Difference + Year +(1|Male.ID), data = MOCH)
mochSMI3 <- lmer(Male_SMI ~ Wing_Difference + Elevation +(1|Male.ID), data = MOCH)
mochSMI4 <- lmer(Male_SMI ~ Wing_Difference + Year + Elevation +(1|Male.ID), data = MOCH)

aic_results <- AIC(mochSMI1, mochSMI2, mochSMI3, mochSMI4)

print(aic_results)

summary(mochSMI1)


bcchSMI1 <- lm(Male_SMI ~ Wing_Difference, data = BCCH)
bcchSMI2 <- lm(Male_SMI ~ Wing_Difference + Year, data = BCCH)
bcchSMI3 <- lm(Male_SMI ~ Wing_Difference + Elevation, data = BCCH)
bcchSMI4 <- lm(Male_SMI ~ Wing_Difference + Year + Elevation, data = BCCH)

aic_results <- AIC(bcchSMI1, bcchSMI2, bcchSMI3, bcchSMI4)

print(aic_results)

summary(bcchSMI1)


#testing different models for the effect of wing difference on SMI
head(MOCH)
mochFSMI1 <- lmer(Female_SMI ~ Wing_Difference +(1|Female.ID), data = MOCH)
mochFSMI2 <- lmer(Female_SMI ~ Wing_Difference + Year +(1|Female.ID), data = MOCH)
mochFSMI3 <- lmer(Female_SMI ~ Wing_Difference + Elevation +(1|Female.ID), data = MOCH)
mochFSMI4 <- lmer(Female_SMI ~ Wing_Difference + Year + Elevation +(1|Female.ID), data = MOCH)

aic_results <- AIC(mochSMI1, mochSMI2, mochSMI3, mochSMI4)

print(aic_results)

summary(mochSMI1)


bcchFSMI1 <- lmer(Female_SMI ~ Wing_Difference + (1|Female.ID), data = BCCH)
bcchFSMI2 <- lmer(Female_SMI ~ Wing_Difference + Year+ (1|Female.ID), data = BCCH)
bcchFSMI3 <- lmer(Female_SMI ~ Wing_Difference + Elevation+ (1|Female.ID), data = BCCH)
bcchFSMI4 <- lmer(Female_SMI ~ Wing_Difference + Year + Elevation+ (1|Female.ID), data = BCCH)

aic_results <- AIC(bcchSMI1, bcchSMI2, bcchSMI3, bcchSMI4)

print(aic_results)

summary(bcchSMI1)


ggplot(data = MOCH, aes(x = Wing_Difference, y = Male_SMI))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = MOCH, aes(x = Wing_Difference, y = Female_SMI))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = BCCH, aes(x = Wing_Difference, y = Male_SMI))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = BCCH, aes(x = Wing_Difference, y = Female_SMI))+
  geom_point(color = "black",  shape=21, size=4)


ggplot(data = MOCH, aes(x = Wing_Difference, y = Egg_Number))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = MOCH, aes(x = Wing_Difference, y = Avg_Nestling_Weight))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = BCCH, aes(x = Wing_Difference, y = Egg_Number))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = BCCH, aes(x = Wing_Difference, y = Avg_Nestling_Weight))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = MOCH, aes(x = Wing_Difference, y = Nestling_Number))+
  geom_point(color = "black", shape=21, size=4)

ggplot(data = BCCH, aes(x = Wing_Difference, y = Nestling_Number))+
  geom_point(color = "black", shape=21, size=4)
