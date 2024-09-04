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

MOCHpop <- subset(bodySizeSexPop, Species = "MOCH")
BCCHpop <- subset(bodySizeSexPop, Species = "BCCH")
MOCHpop <- na.omit(MOCHpop)
BCCHpop <- na.omit(BCCHpop)

MOCHpop$lnElevation <- log(MOCHpop$Elevation)
BCCHpop$lnElevation <- log(BCCHpop$Elevation)

MOCH$lnElevation <- log(MOCH$Elevation)
BCCH$lnElevation <- log(BCCH$Elevation)
#linear mixed model to determine if Wing length is predicted by sex and species with ID as random effect and bander, year, location as fixed effects. Population level analysis

mochPop1 <- lmer(Wing.Chord ~ Sex + Bander + Year + Elevation + (1|ID), data = MOCHpop, REML = FALSE)
mochPop2 <- lmer(Wing.Chord ~ Sex + Bander + Elevation + (1|ID), data = MOCHpop, REML = FALSE)
mochPop3 <- lmer(Wing.Chord ~ Sex + Year + (1|ID), data = MOCHpop, REML = FALSE)
mochPop4 <- lmer(Wing.Chord ~ Sex + (1|ID), data = MOCHpop, REML = FALSE)

pop.null <- lmer(Wing.Chord ~ Bander + Elevation + (1|ID), data = MOCHpop, REML = FALSE)

aic_results <- AIC(mochPop1, mochPop2, mochPop3, mochPop4)
print(aic_results)

summary(mochPop2)
summary(pop.null)

anova(pop.null, mochPop2)

#check the residuals on these models
hist(resid(mochPop2))

plot(mochPop2)

#linear mixed model to determine if Wing length is predicted by sex and species with ID as random effect and bander, year, location as fixed effects. Population level analysis

bcchPop1 <- lmer(Wing.Chord ~ Sex + Bander + Year + Elevation + (1|ID), data = BCCHpop, REML = FALSE)
bcchPop2 <- lmer(Wing.Chord ~ Sex + Bander + Elevation + (1|ID), data = BCCHpop, REML = FALSE)
bcchPop3 <- lmer(Wing.Chord ~ Sex + Year + (1|ID), data = BCCHpop, REML = FALSE)
bcchPop4 <- lmer(Wing.Chord ~ Sex + (1|ID), data = BCCHpop, REML = FALSE)

bcchpop.null <- lmer(Wing.Chord ~ Bander + Elevation + (1|ID), data = BCCHpop, REML = FALSE)

aic_results <- AIC(bcchPop1, bcchPop2, bcchPop3, bcchPop4)
print(aic_results)

summary(bcchPop2)
summary(bcchpop.null)

anova(pop.null, bcchPop2)

#check the residuals on these models
hist(resid(mochPop2))

plot(mochPop2)

#there is a sexual size dimorphism in black-capped and mountain chickadees and MOCH have longer wings than BCCH
#now I want to know if there is a relationship between female wing length and male wing length within pairs, which would inform if fmelaes specifically mate with males with longer wings
#subset by species
MOCH <- subset(bodySize, Species == "MOCH")
BCCH <- subset(bodySize, Species == "BCCH")
#ensure categorical variables are factors
MOCH$MaleBander <- as.factor(MOCH$MaleBander)
MOCH$FemaleBander <- as.factor(MOCH$FemaleBander)
MOCH$Elevation <- as.numeric(MOCH$Elevation)


mochfemaleWing1 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander + FemaleBander + Elevation, data = MOCH)
mochfemaleWing2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Elevation, data = MOCH)
mochfemaleWing3 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander +FemaleBander, data = MOCH)
mochfemalewing.null <- lm(Male.Wing.Chord ~ Elevation, data = MOCH)

summary(mochfemaleWing1)
summary(mochfemaleWing2)
summary(mochfemaleWing3)

aic_results <- AIC(mochfemaleWing1, mochfemaleWing2, mochfemaleWing3)

print(aic_results)

anova(femalewing.null, femaleWing2)

#checking the fit for the residuals
fwingresids <- mochfemaleWing2$residuals
{
  qqnorm(fwingresids)
  qqline(fwingresids)
}

#ensure categorical variables are factors
BCCH$MaleBander <- as.factor(BCCH$MaleBander)
BCCH$FemaleBander <- as.factor(BCCH$FemaleBander)
BCCH$Elevation <- as.numeric(BCCH$Elevation)


bcchfemaleWing1 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander + FemaleBander + Elevation, data = BCCH)
bcchfemaleWing2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + Elevation, data = BCCH)
bcchfemaleWing3 <- lm(Male.Wing.Chord ~ Female.Wing.Chord + MaleBander +FemaleBander, data = BCCH)
bcchfemalewing.null <- lm(Male.Wing.Chord ~ Elevation, data = BCCH)

summary(bcchfemaleWing1)
summary(bcchfemaleWing2)
summary(bcchfemaleWing3)

aic_results <- AIC(bcchfemaleWing1, bcchfemaleWing2, bcchfemaleWing3)

print(aic_results)

anova(bcchfemalewing.null, bcchfemaleWing2)

#checking the fit for the residuals
fwingresids <- bcchfemaleWing2$residuals
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


#Saving the subsetted dataframe for permutations
write.csv(MOCH,"~/MOCH.csv", row.names = TRUE)
write.csv(BCCH,"~/BCCH.csv", row.names = TRUE)
head(BCCH)

#Analyzing the permutations
bcchPermutations <- read.csv("BCCHPermutations.csv")
mochPermutations <- read.csv("MOCHPermutations.csv")

nrow(BCCH)
median(BCCH$Wing_Difference)

#Subsetting observed BCCH data into female larger, male slightly larger, and male much larger categories
BCCHfemaleLarger <- subset(BCCH, Wing_Difference < 0)
BCCHmaleSlightly <- subset(BCCH, Wing_Difference > 0 & Wing_Difference < 3)
BCCHmaleMuch <- subset(BCCH, Wing_Difference >= 3)

#calculating the observed percentage of female larger, male slightly larger, and male much larger BCCH pairings
speciesBCCHFL <- nrow(BCCHfemaleLarger)/nrow(BCCH)
speciesBCCHMS <- nrow(BCCHmaleSlightly)/nrow(BCCH)
speciesBCCHMM <- nrow(BCCHmaleMuch)/nrow(BCCH)

head(bcchPermutations)
bcchPermutations$Female.larger <- as.numeric(bcchPermutations$Female.larger)
bcchPermutations$male.slightly <- as.numeric(bcchPermutations$male.slightly)
bcchPermutations$male.much <- as.numeric(bcchPermutations$male.much)


shapiro.test(bcchPermutations$Female.larger)
ggqqplot(bcchPermutations$Female.larger)
shapiro.test(bcchPermutations$male.slightly)
shapiro.test(bcchPermutations$male.much)

#getting ranges of permutation data, and conducting a one sample t test comparing the observed percentage of each category with the permutations

min(as.numeric(bcchPermutations$Female.larger))
max(as.numeric(bcchPermutations$Female.larger))

t.test(bcchPermutations$Female.larger, mu = speciesBCCHFL)

min(as.numeric(bcchPermutations$male.slightly))
max(as.numeric(bcchPermutations$male.slightly))

t.test(bcchPermutations$male.slightly, mu = speciesBCCHMS)

min(as.numeric(bcchPermutations$male.much))
max(as.numeric(bcchPermutations$male.much))

t.test(bcchPermutations$male.much, mu = speciesBCCHMM)

###############MOCH Permutations ################################

#subsetting observed MOCH data into female larger, male slightly larger, and male much larger 
median(MOCH$Wing_Difference)

MOCHfemaleLarger <- subset(MOCH, Wing_Difference < 0)
MOCHmaleSlightly <- subset(MOCH, Wing_Difference > 0 & Wing_Difference <= 3)
MOCHmaleMuch <- subset(MOCH, Wing_Difference >3)

#calculating the observed percentage of female larger, male slightly larger, and male much larger MOCH pairings
speciesMOCHFL <- nrow(MOCHfemaleLarger)/nrow(MOCH)
speciesMOCHMS <- nrow(MOCHmaleSlightly)/nrow(MOCH)
speciesMOCHMM <- nrow(MOCHmaleMuch)/nrow(MOCH)
mean(mochPermutations$male.much)
head(mochPermutations)

min(as.numeric(mochPermutations$Female.larger))
max(as.numeric(mochPermutations$Female.larger))

t.test(mochPermutations$Female.larger, mu = speciesMOCHFL)

min(as.numeric(mochPermutations$male.slightly))
max(as.numeric(mochPermutations$male.slightly))

t.test(mochPermutations$male.slightly, mu = speciesMOCHMS)

min(as.numeric(mochPermutations$male.much))
max(as.numeric(mochPermutations$male.much))

t.test(mochPermutations$male.much, mu = speciesMOCHMM)


bcchPermutations$Female.larger <- bcchPermutations$Female.larger*100
speciesBCCHFL <- speciesBCCHFL*100

bcchPermutations$male.slightly <- bcchPermutations$male.slightly*100
speciesBCCHMS <- speciesBCCHMS*100

bcchPermutations$male.much <- bcchPermutations$male.much*100
speciesBCCHMM <- speciesBCCHMM*100

mochPermutations$Female.larger <- mochPermutations$Female.larger*100
speciesMOCHFL <- speciesMOCHFL*100

mochPermutations$male.slightly <- mochPermutations$male.slightly*100
speciesMOCHMS <- speciesMOCHMS*100

mochPermutations$male.much <- mochPermutations$male.much*100
speciesMOCHMM <- speciesMOCHMM*100

#From the above analyses I find that the observed percentages are significantly different form the simulated percentages. Now let's visualize it!
library(patchwork)

BCCHFL <- ggplot(bcchPermutations, aes(x = Female.larger))+
  geom_histogram(fill = "#0072b2", bins = 10, color = "black")+
  geom_vline(aes(xintercept =speciesBCCHFL), color = "grey", linewidth = 2, )+
  theme_classic()+
  xlab("% of BCCH pairs with females\nlarger than males")+
  ylab("Count")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  ylim(0,400)

BCCHMS <- ggplot(bcchPermutations, aes(x = male.slightly))+
  geom_histogram(fill = "#0072b2", bins = 10, color = "black")+
  geom_vline(aes(xintercept =speciesBCCHMS), color = "grey", linewidth = 2)+
  theme_classic()+
  xlab("% of BCCH pairs with males slightly\n larger than females")+
  ylab("")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  ylim(0,400)

BCCHMM <- ggplot(bcchPermutations, aes(x = male.much))+
  geom_histogram(fill = "#0072b2", bins = 10, color = "black")+
  geom_vline(aes(xintercept =speciesBCCHMM), color = "gray", linewidth = 2)+
  theme_classic()+
  xlab("% of BCCH pairs with males much\n larger than females")+
  ylab("")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  ylim(0,400)

MOCHFL <- ggplot(mochPermutations, aes(x = Female.larger))+
  geom_histogram(fill = "#cc79a7", bins = 10, color = "black")+
  geom_vline(aes(xintercept =speciesMOCHFL), color = "gray", linewidth = 2)+
  theme_classic()+
  xlab("% of MOCH pairs with females\n larger than males")+
  ylab("Count")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  ylim(0,400)

MOCHMS <- ggplot(mochPermutations, aes(x = male.slightly))+
  geom_histogram(fill = "#cc79a7", bins = 10, color = "black")+
  geom_vline(aes(xintercept =speciesMOCHMS), color = "gray", linewidth = 2)+
  theme_classic()+
  xlab("% of MOCH pairs with males slightly\n larger than females")+
  ylab("")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  ylim(0,400)

MOCHMM <- ggplot(mochPermutations, aes(x = male.much))+
  geom_histogram(fill = "#cc79a7", bins = 10, color = "black")+
  geom_vline(aes(xintercept =speciesMOCHMM), color = "gray", linewidth = 2)+
  theme_classic()+
  xlab("% of MOCH pairs with males much \nlarger than females")+
  ylab("")+
  theme(axis.text = element_text(size=14, color = "black"))+
  theme(axis.title = element_text(size = 14))+
  theme(axis.title.x = element_text(margin = margin(t = 10)))+
  theme(axis.title.y = element_text(margin = margin(r = 10)))+
  theme(text = element_text(family = "sans"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 0.5))+
  ylim(0,400)

p1 <-(BCCHFL + BCCHMS + BCCHMM)
p2 <-(MOCHFL + MOCHMS + MOCHMM)

p1
p2

#First let's plot the data
#calculating the median diff between male and female wing length
meanWing <- mean(bodySize$Wing_Difference)

#Density plot of wing length difference colored based on species
wing_difference_by_species <- ggplot(bodySize, aes(x=Wing_Difference, fill=Species))+
  geom_density(alpha=0.5)+
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=Species), linetype="dashed")+
  labs (x="Difference Between Male and Female Wing Chord (mm)", y="Density")+
  theme_classic()+
  scale_fill_manual(values=c("#0072b2", "#cc79a7"))+
  theme(plot.title = element_text(hjust=0.5))+
  scale_x_continuous(limits=c(-6,12), breaks=seq(-6,12,2))+
  geom_vline(aes(xintercept =0), color = "black", linewidth = 1)+
  geom_vline(aes(xintercept =meanWing), color = "black", linewidth = 1, linetype="dashed")+
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

wing_difference_by_species/p1/p2


#making box plots of male and female pairs
library(dplyr)
bodySizePopID <- read.csv("population_SMI_nestid.csv")
MOCHID <- subset(bodySizePopID, Species == "MOCH")
BCCHID <- subset(bodySizePopID, Species == "BCCH")
head(MOCHID)

MOCH_count <- MOCHID %>%
  group_by(Sex, Wing.Chord) %>%
  summarize(count = n(), .groups = 'drop')

MOCHSexWing <- ggplot(MOCHID, aes(x = Sex, y = Wing.Chord))+
  geom_boxplot(color = "black", fill = "#cc79a7")+
  geom_line(aes(group=Nest.ID), color = "gray")+
  geom_point(data = MOCH_count, aes(size = count), alpha = 0.6)+
  scale_size_continuous(range = c(2,6))+
  theme_classic()+
  labs(x = "Sex", y = "Wing Length (mm)")+
  ylim(60,78)


BCCH_count <- BCCHID %>%
  group_by(Sex, Wing.Chord) %>%
  summarize(count = n(), .groups = 'drop')
head(BCCHID)

BCCHSexWing <- ggplot(BCCHID, aes(x = Sex, y = Wing.Chord))+
  geom_boxplot(color = "black", fill = "#0072b2")+
  geom_line(aes(group=Nest.ID), color = "gray")+
  geom_point(data = BCCH_count, aes(size = count), alpha = 0.6)+
  scale_size_continuous(range = c(2,6))+
  theme_classic()+
  labs(x = "Sex", y = "Wing Length (mm)")+
  ylim(60,78)+
  theme(
    axis.title.y = element_blank()
  )


wing_sex_comparison <- (MOCHSexWing + BCCHSexWing)
head(bodySize)
ggplot(bodySize, aes(x = Wing_Difference, y = Female_SMI, color = Species))+
  geom_point()+
  theme_classic()


#Testing some things
head(MOCH)
library(ggplot2)
ggplot(MOCH, aes(x = Male.Wing.Chord, y = Avg_Nestling_Weight))+
  geom_point()

nWeightWing <- lm(Avg_Nestling_Weight ~ Male.Wing.Chord + Elevation, MOCH)

summary(nWeightWing)

ggplot(BCCH, aes(x = Male.Wing.Chord, y = Avg_Nestling_Weight))+
  geom_point()

nWeightWing <- lm(Avg_Nestling_Weight ~ Male.Wing.Chord + Elevation, BCCH)

summary(nWeightWing)

ggplot(BCCH, aes(x = Elevation, y = Male.Wing.Chord))+
  geom_point()
ggplot(BCCH, aes(x = Elevation, y = Female.Wing.Chord))+
  geom_point()
ggplot(MOCH, aes(x = Elevation, y = Male.Wing.Chord))+
  geom_point()
ggplot(MOCH, aes(x = Elevation, y = Female.Wing.Chord))+
  geom_point()

ggplot(MOCH, aes(x= Elevation, y = Avg_Nestling_Weight))+
  geom_point()

lm <- lm(Avg_Nestling_Weight ~ Elevation, data = MOCH)

lm <- lm(Avg_Nestling_Weight ~ Elevation, data = BCCH)

summary(lm)

ggplot(BCCH, aes(x= Elevation, y = Avg_Nestling_Weight))+
  geom_point()

#just looking at elevation
BCCH$Year <- as.factor(BCCH$Year)
MOCH$Year <- as.factor(MOCH$Year)
elevationN <- lm(Avg_Nestling_Weight ~ Elevation + Year, BCCH)

summary(elevationN)
