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
m.a1 <- lm(Female.Wing.Chord ~ Male.Wing.Chord, data=BSMOCH)

anova(m.a1)
summary(m.a1)

#no relationship btwn male and female wing length p = 0.631
#BCCH Female response variable
b.a1 <- lm(Female.Wing.Chord ~ Male.Wing.Chord, data=BSBCCH)

anova(b.a1)
summary(b.a1)

#no relationship btwn male and female wing length p = 0.258


#MOCH Female response variable
m.a2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data=BSMOCH)

anova(m.a2)
summary(m.a2)

#no relationship btwn male and female wing length p = 0.6307
#BCCH Female response variable
b.a2 <- lm(Male.Wing.Chord ~ Female.Wing.Chord, data=BSBCCH)

anova(b.a2)
summary(b.a2)

#no relationship btwn male and female wing length p = 0.258

#How much data to work with within sex within band years and season for swaps? IN MOCH
#prep data
MOCH.m <- BSMOCH %>% select(Year, Location, Male.ID, Male.Wing.Chord, Pair.ID) %>% rename(BirdID = Male.ID, Wing.Chord = Male.Wing.Chord) %>% mutate(Sex="M")
MOCH.f <- BSMOCH %>% select(Year, Location, Female.ID, Female.Wing.Chord, Pair.ID) %>% rename(BirdID = Female.ID, Wing.Chord = Female.Wing.Chord) %>% mutate(Sex="F")
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

#Calculate significance
p = sum(obs.n1>rand.results1$V1)/nrow(rand.results1)
print(p)
#pairings are more negative than expected by chance
(obs.n1*100)-(mean(rand.results1$V1)*100)
#Observed pairings were about 4 percentage points more negative than expected by chance

### Permutation - Male slightly larger pairs vs chance MOCH

moch.pos <- BSMOCH %>% filter(Wing_Difference>0)
moch.pos.mf <- MOCH.mf %>% filter(Pair.ID %in% moch.pos$Pair.ID)
median(moch.pos$Wing_Difference) #3
moch.pos.msl <- moch.pos %>% filter(Wing_Difference <=3)
moch.pos.mml <- moch.pos %>% filter(Wing_Difference >3)
nrow(moch.pos.msl)
nrow(moch.pos.mml)
#Less than or equal to median for male slightly larger and greater than for male much larger gets closest cample size split
#38 msl vs 36 mml

#Get observed percentage of male much larger pairings
obs.n2 <- nrow(moch.pos.msl)/nrow(moch.pos) #51.35%

#set up dataframe to store permutation results
rand.results2 <- matrix(nrow=1000, ncol=1)

#set seed before randomizations
set.seed(25)

for(i in 1:nrow(rand.results2)){
  #Randomly resample BirdID within sex and season banded
  moch.pos.mfrr <- moch.pos.mf %>% group_by(Sex) %>% mutate(BirdIDr = sample(BirdID, replace = F))
  #Match up wing length to birdIDr
  moch.pos.mfrr2 <- moch.pos.mfrr %>% left_join(mochWing, by ="BirdIDr")
  #Get ratio of male slightly larger pairings in resampled data wing.lengthr
  obs.nr1 <- moch.pos.mfrr2 %>% group_by(Pair.ID) %>% arrange(Sex) %>% reframe(Wing_Difference=diff(Wing.Chordr)) %>% select(Wing_Difference) %>% filter(Wing_Difference >0)
  obs.nr2 <- obs.nr1 %>% filter(Wing_Difference <=3)
  rand.results2[i] = nrow(obs.nr2)/(nrow(obs.nr1))
  
}

#plot results
rand.results2 <- as.data.frame(rand.results2)
ggplot()+
  geom_histogram(data=rand.results2, aes(x=V1*100),binwidth=0.5,fill="maroon",color="black",alpha=0.8)+
  geom_vline(aes(xintercept=obs.n2*100),color="black",size=1.8) +
  geom_vline(aes(xintercept=obs.n2*100),color="#ADD8e6",size=1)+
  theme_cowplot()+
  xlab("Percent male slightly \nlarger pairings")+
  ylab("count")


#Calculate significance
p=1-sum(obs.n2>rand.results2$V1)/nrow(rand.results2)
print(p)
#see more slightly larger pairings than expected by chance p<0.001
(obs.n2*100) - (mean(rand.results2$V1)*100)
#See about 8.5 percent more male much larger pairings than expected by chance



#Permutation - Male much larger pairs vs chance ####

#First get median of positive/equal size pairings
median(moch.pos$Wing_Difference) #3
moch.pos.mml <- moch.pos %>% filter(Wing_Difference > 3)

#Get observed percentage of male much larger pairings
obs.n3 <- nrow(moch.pos.mml)/nrow(moch.pos) #48.6%

#Set up dataframe to store permutation results
rand.results3 <- matrix(nrow=1000,ncol=1)
#set seed before randomizations
set.seed(25)

#Permutation
for(i in 1:nrow(rand.results3)){
  #ranomdly resample BirdID within sex
  moch.pos.mfrr <- moch.pos.mf %>% group_by(Sex)%>% mutate(BirdIDr = sample(BirdID,replace = F))
  #Match up wing length to birdIDr
  moch.pos.mfrr2 <- moch.pos.mfrr %>% left_join(mochWing, by="BirdIDr")
  #Get ratio of negative pairings in resampled data - wing.lengthr
  obs.nr1 <- moch.pos.mfrr2 %>% group_by(Pair.ID) %>% arrange(Sex) %>% reframe(Wing_Difference = diff(Wing.Chordr)) %>% select(Wing_Difference) %>%
    filter(Wing_Difference > 0)
  obs.nr2 <- obs.nr1 %>% filter(Wing_Difference > 3)
  rand.results3[i] = nrow(obs.nr2)/(nrow(obs.nr1))
}


#Plot results
rand.results3 <- as.data.frame(rand.results3)
ggplot() +
  geom_histogram(data=rand.results3,aes(x=V1*100), binwidth=0.5,fill="maroon",color="black",alpha=0.8)+
  geom_vline(aes(xintercept=obs.n3*100),color="black",size=1.8)+
  geom_vline(aes(xintercept=obs.n3*100),color="#ADD8E6",size=1)+
  theme_cowplot()+
  xlab("Percent male much\nlarger pairings")+
  ylab("Count")

#Calculate significance
p= sum(obs.n3>rand.results3$V1)/nrow(rand.results3)
print(p) #0.014
(obs.n3*100) - (mean(rand.results3$V1)*100)
# see about 8 percent fewer male much larger pairings than expected by chance

###------------------------------------------------------########
#REDOING PERMUTATIONS BUT WITH THE BCCH DATA

#How much data to work with within sex within band years and season for swaps? IN MOCH
#prep data
BCCH.m <- BSBCCH %>% select(Year, Location, Male.ID, Male.Wing.Chord, Pair.ID) %>% rename(BirdID = Male.ID, Wing.Chord = Male.Wing.Chord) %>% mutate(Sex="M")
BCCH.f <- BSBCCH %>% select(Year, Location, Female.ID, Female.Wing.Chord, Pair.ID) %>% rename(BirdID = Female.ID, Wing.Chord = Female.Wing.Chord) %>% mutate(Sex="F")
BCCH.mf <- rbind(BCCH.f,BCCH.m)

#How much data to work with within sex and year
swap.table <- BCCH.mf %>% group_by(Sex, Location) %>% summarize(n=n()) %>% print()


#Permutation using their methods
bcchWing <- BCCH.mf %>% select(BirdID, Wing.Chord) %>%
  rename(BirdIDr = BirdID, Wing.Chordr = Wing.Chord) %>%
  distinct(BirdIDr,.keep_all = T)

#Get observed percentage of negative pairings
obs.n4 <- BCCH.mf %>% group_by(Pair.ID)%>% arrange(Sex) %>%
  reframe(wing.diff=diff(Wing.Chord)) %>% filter(wing.diff<0)
obs.n4 <- nrow(obs.n4)/(nrow(BCCH.mf)/2) #13.6


#Set up dataframe to store permutation results
rand.results4 <- matrix(nrow=1000, ncol=1)

#Set seed before randomizations
set.seed(27)

#Permutation
for(i in 1:nrow(rand.results4)){
  bpw.mfrr <- BCCH.mf %>% group_by(Sex) %>%
    mutate(BirdIDr = sample(BirdID, replace = F))
  bpw.mfrr2 <- bpw.mfrr %>% left_join(bcchWing,by="BirdIDr")
  
  obs.nr <- bpw.mfrr2 %>% group_by(Pair.ID) %>% arrange(Sex) %>%
    reframe(wing.diff = diff(Wing.Chordr)) %>% filter(wing.diff <0)
  rand.results4[i] = nrow(obs.nr)/(nrow(bpw.mfrr2)/2)
}

##Plot results
rand.results4 = as.data.frame(rand.results4)
ggplot() + geom_histogram(data=rand.results4,aes(x=V1*100),binwidth = 0.5,fill="maroon",color="black",alpha=0.8) + 
  geom_vline(aes(xintercept=obs.n4*100),color="black",size=1.8) +
  geom_vline(aes(xintercept=obs.n4*100),color="#ADD8E6",size=1) + 
  theme_cowplot() + xlab("Percent female larger\npairings") + ylab("Count")

#Calculate significance
p = sum(obs.n4>rand.results4$V1)/nrow(rand.results4)
print(p)
#pairings are not different from chance p=0.251
(obs.n4*100)-(mean(rand.results4$V1)*100)
#-1.18%

### Permutation - Male slightly larger pairs vs chance MOCH

bcch.pos <- BSBCCH %>% filter(Wing_Difference>0)
bcch.pos.mf <- BCCH.mf %>% filter(Pair.ID %in% bcch.pos$Pair.ID)
median(bcch.pos$Wing_Difference) #4
bcch.pos.msl <- bcch.pos %>% filter(Wing_Difference <4)
bcch.pos.mml <- bcch.pos %>% filter(Wing_Difference >=4)
nrow(bcch.pos.msl)
nrow(bcch.pos.mml)
#Less than median for male slightly larger and greater than or equal to for male much larger gets closest cample size split
#24 msl vs 31 mml

#Get observed percentage of male much larger pairings
obs.n5 <- nrow(bcch.pos.msl)/nrow(bcch.pos) #43.6%

#set up dataframe to store permutation results
rand.results5 <- matrix(nrow=1000, ncol=1)

#set seed before randomizations
set.seed(29)

for(i in 1:nrow(rand.results5)){
  #Randomly resample BirdID within sex and season banded
  bcch.pos.mfrr <- bcch.pos.mf %>% group_by(Sex) %>% mutate(BirdIDr = sample(BirdID, replace = F))
  #Match up wing length to birdIDr
  bcch.pos.mfrr2 <- bcch.pos.mfrr %>% left_join(bcchWing, by ="BirdIDr")
  #Get ratio of male slightly larger pairings in resampled data wing.lengthr
  obs.nr1 <- bcch.pos.mfrr2 %>% group_by(Pair.ID) %>% arrange(Sex) %>% reframe(Wing_Difference=diff(Wing.Chordr)) %>% select(Wing_Difference) %>% filter(Wing_Difference >0)
  obs.nr2 <- obs.nr1 %>% filter(Wing_Difference <4)
  rand.results5[i] = nrow(obs.nr2)/(nrow(obs.nr1))
  
}

#plot results
rand.results5 <- as.data.frame(rand.results5)
ggplot()+
  geom_histogram(data=rand.results5, aes(x=V1*100),binwidth=0.5,fill="maroon",color="black",alpha=0.8)+
  geom_vline(aes(xintercept=obs.n5*100),color="black",size=1.8) +
  geom_vline(aes(xintercept=obs.n5*100),color="#ADD8e6",size=1)+
  theme_cowplot()+
  xlab("Percent male slightly \nlarger pairings")+
  ylab("count")


#Calculate significance
p=1-sum(obs.n5>rand.results5$V1)/nrow(rand.results5)
print(p)
#pairings not different from chance p =0.107
(obs.n5*100) - (mean(rand.results5$V1)*100)
#See about 6.78 percent more (but not significant)



#Permutation - Male much larger pairs vs chance ####

#First get median of positive/equal size pairings
median(bcch.pos$Wing_Difference) #4
bcch.pos.mml <- bcch.pos %>% filter(Wing_Difference >= 4)

#Get observed percentage of male much larger pairings
obs.n6 <- nrow(bcch.pos.mml)/nrow(bcch.pos) #56.4%

#Set up dataframe to store permutation results
rand.results6 <- matrix(nrow=1000,ncol=1)
#set seed before randomizations
set.seed(45)

#Permutation
for(i in 1:nrow(rand.results6)){
  #ranomdly resample BirdID within sex
  bcch.pos.mfrr <- bcch.pos.mf %>% group_by(Sex)%>% mutate(BirdIDr = sample(BirdID,replace = F))
  #Match up wing length to birdIDr
  bcch.pos.mfrr2 <- bcch.pos.mfrr %>% left_join(bcchWing, by="BirdIDr")
  #Get ratio of negative pairings in resampled data - wing.lengthr
  obs.nr1 <- bcch.pos.mfrr2 %>% group_by(Pair.ID) %>% arrange(Sex) %>% reframe(Wing_Difference = diff(Wing.Chordr)) %>% select(Wing_Difference) %>%
    filter(Wing_Difference > 0)
  obs.nr2 <- obs.nr1 %>% filter(Wing_Difference >= 4)
  rand.results6[i] = nrow(obs.nr2)/(nrow(obs.nr1))
}


#Plot results
rand.results6 <- as.data.frame(rand.results6)
ggplot() +
  geom_histogram(data=rand.results6,aes(x=V1*100), binwidth=0.5,fill="maroon",color="black",alpha=0.8)+
  geom_vline(aes(xintercept=obs.n6*100),color="black",size=1.8)+
  geom_vline(aes(xintercept=obs.n6*100),color="#ADD8E6",size=1)+
  theme_cowplot()+
  xlab("Percent male much\nlarger pairings")+
  ylab("Count")

#Calculate significance
p= sum(obs.n6>rand.results6$V1)/nrow(rand.results6)
print(p) #0.097
(obs.n6*100) - (mean(rand.results6$V1)*100)
# see about 6.7 less than much larger, but not significant




