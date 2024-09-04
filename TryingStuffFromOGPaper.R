#Reading in data
BS <- read.csv("paired_SMI.csv")
BSpop <- read.csv("population_SMI.csv")

#loading libraries

library(dplyr)
library(ggpubr)
library(ggplot2)
library(cowplot)
library(lme4)
library(lmerTest)
library(DHARMa)
library(climwin)

#Get each individual represented once for male-female population comparisons
head(BS)
head(BSpop)
nrow(BSpopI)
BSpopI <- BSpop %>% distinct(ID, .keep_all = T)

BSpopMOCH <- subset(BSpopI, Species == "MOCH")
BSpopBCCH <- subset(BSpopI, Species == "BCCH")

BSpopMOCH = BSpopMOCH %>% mutate(Nest.ID = paste(Nestbox, Year))
BSpopBCCH = BSpopBCCH %>% mutate(Nest.ID = paste(Nestbox, Year))

#plot variation by sex first for MOCH then for BCCH
BSPopplotMOCH = BSpopMOCH %>% group_by(Sex, Wing.Chord) %>% summarise(n=n()) %>% mutate(n2 = n/2)

BSPopplotBCCH = BSpopBCCH %>% group_by(Sex, Wing.Chord) %>% summarise(n=n()) %>% mutate(n2 = n/2)

ggplot(data= BSpopMOCH, aes(x = Sex, y = Wing.Chord))+
  geom_boxplot(aes(fill = Sex), alpha = 0.8, outlier.alpha = 0, width = 0.5)+
  geom_line(aes(x=Sex, y=Wing.Chord, group = Nest.ID), alpha=0.1, size=0.4)+
  geom_point(data=BSPopplotMOCH, aes(x=Sex, y=Wing.Chord,size=n2),alpha=0.5)+
  theme_cowplot() + scale_fill_manual(values=c("dark gray","dark gray"))+
  theme(legend.position="")+
  xlab("Sex")+
  ylab("Wing length (mm)")


ggplot(data= BSpopBCCH, aes(x = Sex, y = Wing.Chord))+
  geom_boxplot(aes(fill = Sex), alpha = 0.8, outlier.alpha = 0, width = 0.5)+
  geom_line(aes(x=Sex, y=Wing.Chord, group = Nest.ID), alpha=0.1, size=0.4)+
  geom_point(data=BSPopplotBCCH, aes(x=Sex, y=Wing.Chord,size=n2),alpha=0.5)+
  theme_cowplot() + scale_fill_manual(values=c("dark gray","dark gray"))+
  theme(legend.position="")+
  xlab("Sex")+
  ylab("Wing length (mm)")

#subsetting main dataset
head(BS)
BSMOCH <- subset(BS, Species == "MOCH")
BSBCCH <- subset(BS, Species == "BCCH")
#plot wing length 
ggplot(data = BSMOCH, aes(x=Female.Wing.Chord, y= Male.Wing.Chord))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Female Wing Length (mm)")+
  ylab("Male Wing Length (mm)")+
  stat_regline_equation(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")),label.x=66, label.y=72)

ggplot(data = BSBCCH, aes(x=Female.Wing.Chord, y= Male.Wing.Chord))+
  geom_point(size=3,alpha=0.3) +
  theme_cowplot()+
  geom_smooth(method="lm",color="black")+
  xlab("Female Wing Length (mm)")+
  ylab("Male Wing Length (mm)")+
  stat_regline_equation(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~~")),label.x=66, label.y=72)


#ensuring factors are factors
BSpopMOCH$Year <- as.factor(BSpopMOCH$Year)
BSpopBCCH$Year <- as.factor(BSpopBCCH$Year)
BSpopMOCH$Sex <- as.factor(BSpopMOCH$Sex)
BSpopBCCH$Sex <- as.factor(BSpopBCCH$Sex)
BSpopMOCH$Bander <- as.factor(BSpopMOCH$Bander)
BSpopBCCH$Bander <- as.factor(BSpopBCCH$Bander)
#modelling the population level
m.p4a <- lmer(Wing.Chord~Sex + Elevation + Bander + (1|Nest.ID), data = BSpopMOCH)

m.p4ar <- simulateResiduals(m.p4a)
plot(m.p4ar)

#looks good

#Results
anova(m.p4a) #sex is important
summary(m.p4a)

#modelling the population level
b.p4a <- lmer(Wing.Chord~Sex + Elevation + Bander + (1|Nest.ID), data = BSpopMOCH)

b.p4ar <- simulateResiduals(b.p4a)
plot(b.p4ar)
#looks good

#Results
anova(b.p4a) #sex is important
summary(b.p4a)


#Positive and negative assortment
#MOCH Female response variable
m.a1 <- lm(Female.Wing.Chord ~ Male.Wing.Chord, data=MOCH)

anova(m.a1)
summary(m.a1)

#no relationship btwn male and female wing length p = 0.631
#BCCH Female response variable
b.a1 <- lm(Female.Wing.Chord ~ Male.Wing.Chord, data=BCCH)

anova(b.a1)
summary(b.a1)

#no relationship btwn male and female wing length p = 0.258


#MOCH Female response variable
m.a2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data=MOCH)

anova(m.a2)
summary(m.a2)

#no relationship btwn male and female wing length p = 0.6307
#BCCH Female response variable
b.a2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data=BCCH)

anova(b.a2)
summary(b.a2)

#no relationship btwn male and female wing length p = 0.258

#How much data to work with within sex within band years and season for swaps? IN MOCH
#prep data
MOCH.m <- MOCH %>% select(Year, Location, Male.ID, Male.Wing.Chord, Pair.ID) %>% rename(BirdID = Male.ID, Wing.Chord = Male.Wing.Chord) %>% mutate(Sex="M")
MOCH.f <- MOCH %>% select(Year, Location, Female.ID, Female.Wing.Chord, Pair.ID) %>% rename(BirdID = Female.ID, Wing.Chord = Female.Wing.Chord) %>% mutate(Sex="F")
MOCH.mf <- rbind(MOCH.f,MOCH.m)

#How much data to work with within sex and year
swap.table <- MOCH.mf %>% group_by(Sex, Location) %>% summarize(n=n()) %>% print()


#Permutation using their methods
mochWing <- MOCH.mf %>% select(BirdID, Wing.Chord) %>%
  rename(BirdIDr = BirdID, Wing.Chordr = Wing.Chord) %>%
  distinct(BirdIDr,.keep_all = T)

#Get observed percentage of negative pairings
obs.n1 <- MOCH.mf %>% group_by(Pair.ID)%>% arrange(Sex) %>%
  summarise(wing.diff=diff(Wing.Chord)) %>% filter(wing.diff<0)
obs.n1 <- nrow(obs.n1)/(nrow(MOCH.mf)/2)


#Set up dataframe to store permutation results
rand.results1 <- matrix(nrow=1000, ncol=1)

#Set seed before randomizations
set.seed(25)

#Permutation
for(i in 1:nrow(rand.results1)){
  bpw.mfrr <- MOCH.mf %>% group_by(Sex) %>%
    mutate(BirdIDr = sample(BirdID, replace = F))
  bpw.mfrr2 <- bpw.mfrr %>% left_join(mochWing,by="BirdIDr")
  
  obs.nr <- bpw.mfrr2 %>% group_by(Pair.ID) %>% arrange(Sex) %>%
    summarise(wing.diff = diff(Wing.Chordr)) %>% filter(wing.diff <0)
  rand.results1[i] = nrow(obs.nr)/(nrow(bpw.mfrr2)/2)
}

##Plot results
rand.results1 = as.data.frame(rand.results1)
ggplot() + geom_histogram(data=rand.results1,aes(x=V1*100),binwidth = 0.5,fill="maroon",color="black",alpha=0.8) + 
  geom_vline(aes(xintercept=obs.n1*100),color="black",size=1.8) +
  geom_vline(aes(xintercept=obs.n1*100),color="#ADD8E6",size=1) + 
  theme_cowplot() + xlab("Percent female larger\npairings") + ylab("Count")
