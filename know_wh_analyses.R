##Analyses for all studies in George, B. & Phillips, J. (Non)reducability of knowledge-wh: Experimental investigations

rm(list=ls())

source("C:/Users/Jonathan/Documents/currentProjects/R/jsp.useful.R")
setwd("C:/Users/Jonathan/Documents/currentProjects/Knowledge_wh/Know_wh_Materials")

#Load data --------------------------------------------------------------------------------
## Study 1: Basic Demonstration
d1 <- read.csv("know_wh_study1_Rdata.csv") # Knows where: Mixed vs. all True within-subjects
## Study 2: all True vs. Mixed vs. all False
d2.1 <- read.csv("know_wh_study2p1_Rdata.csv") # Knows where she can buy
d2.2 <- read.csv("know_wh_study2p2_Rdata.csv") # Doesn't know where she can buy
d2.3 <- read.csv("know_wh_study2p3_Rdata.csv") # Knows where she should buy
d2.4 <- read.csv("know_wh_study2p4_Rdata.csv") # Doesn't know where she should buy
d2.5 <- read.csv("know_wh_study2p5_Rdata.csv") # Knows where to buy
d2.6 <- read.csv("know_wh_study2p6_Rdata.csv") # Doesn't know where to buy
## Study 3: Knowledge that
d3 <- read.csv("know_wh_study3_Rdata.csv")
## Study 4: Relevance-Implicature
d4 <- read.csv("know_wh_study4_Rdata.csv")
##study 5: Generalization to other imbeded questions
d5.1 <- read.csv("know_wh_study5p1_Rdata.csv") # Knows how
d5.2 <- read.csv("know_wh_study5p2_Rdata.csv") # Knows who
##study 6: Porportionality Test
d6 <- read.csv("know_wh_study6_Rdata.csv")

#Study lablels
d1$Study <- "1"
d2.1$Study <- "2"
d2.2$Study <- "2"
d2.3$Study <- "2"
d2.4$Study <- "2"
d2.5$Study <- "2"
d2.6$Study <- "2"
d3$Study <- "3"
d4$Study <- "4"
d5.1$Study <- "5"
d5.2$Study <- "5"
d6$Study <- "6"

# Demographics -------------------------------------------------------

demog <- rbind(
    d1[d1$Order==1,c(1,7:18,20)], ## Study 1
    d2.1[d2.1$Order==1,c(1,8:10,12:20,23)], ## Study 2.1
    d2.2[d2.2$Order==1,c(1,8:10,12:20,23)], ## Study 2.2
    d2.3[d2.3$Order==1,c(1,8:10,12:20,23)], ## Study 2.3
    d2.4[d2.4$Order==1,c(1,8:10,12:20,23)], ## Study 2.4
    d2.5[d2.5$Order==1,c(1,8:10,12:20,23)], ## Study 2.5
    d2.6[d2.6$Order==1,c(1,8:10,12:20,23)], ## Study 2.6
    d3[d3$Order==1,c(1,8:10,12:20,23)], ## Study 3
    d4[,c(1,10:12,14:23)], ## Study 4
    d5.1[,c(1,6:8,10:19)], ## Study 5.1
    d5.2[,c(1,9:11,13:22)], ## Study 5.2
    d6[,c(1,7:9,11:20)]  ## Study 6
  )

Ethnicity <- matrix(rep(99,42),nrow=7,ncol=6)
colnames(Ethnicity) <- c("Black/African American","Hispanic/Latino","Asian/Pacific Islander",
                         "Native American/American Indian","White/Caucasian","Other")
rownames(Ethnicity) <- c("Study 1","Study 2","Study 3","Study 4","Study 5","Study 6","Overall")
for(i in 1:6){
     Ethnicity[i,1] <- sum(!is.na(demog$Ethnicity_1[demog$Study==i])) 
     Ethnicity[i,2] <- sum(!is.na(demog$Ethnicity_2[demog$Study==i])) 
     Ethnicity[i,3] <- sum(!is.na(demog$Ethnicity_3[demog$Study==i])) 
     Ethnicity[i,4] <- sum(!is.na(demog$Ethnicity_4[demog$Study==i])) 
     Ethnicity[i,5] <- sum(!is.na(demog$Ethnicity_5[demog$Study==i])) 
     Ethnicity[i,6] <- sum(!is.na(demog$Ethnicity_6[demog$Study==i])) 
  }
for (j in 1:6){
     Ethnicity[7,j] <- sum(Ethnicity[1:6,j])  
}
demog$Gender <- factor(c("Male","Female")[demog$Gender])
demog$Education <- factor(c("Grammar School","Highschool or Equivalent","Vocational/Technical School",
                                 "Some College","College Graduate (4 years)","Master's Degree",
                                 "Doctoral Degree (PhD)","Professional Degree (JD,MD,etc.)","Other")[demog$Education])
demog.age <- aggregate(Age~Study, demog[demog$Age>18,], #Anything under 18 is a typo as Amazon restricts mturk workers below 18
                       FUN=function(x) c(M =mean(x), SD =sd(x))) 
demog.gender <- aggregate(Gender~Study, demog, FUN=table)
demog.n <- aggregate(Subj~Study, demog, FUN=length)
demog.education <- aggregate(Education~Study, demog, FUN=table)

##Divided by demographic item
###Age and Gender
print(cbind(demog.age,demog.gender[,2],demog.n[,2])) #nb: where n > (female+male), n-(female+male) participants did not report gender
###Ethnicity
print(Ethnicity)
###Education
print(demog.education)

# Study 1 ------------------------------------------------------------
d1 <- read.csv("know_wh_study1_Rdata.csv")

d1$Subj <- factor(d1$Subj)
d1$Condition <- factor(c("All true","Mixed")[d1$Condition])
d1$Order <- factor(c("First Trial","Second Trial")[d1$Order])
d1$Lang <- factor(c("English","Not English")[d1$Lang-20])

## first trial only analyses
d1 <- subset(d1,Lang=="English" & Order=="First Trial") ## remove those who didn't speak English and select 1st trial data

var.test(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"])
print(ttest.1 <- t.test(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"]))
cohensD(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"])

## Analyses for both 1st and 2nd trials
d1 <- subset(d1,Lang=="English") ## remove only those who didn't speak English

var.test(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"])
print(ttest.1 <- t.test(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"],paired=TRUE, var.equal=TRUE))
cohensD(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"])

##Check for an interaction between Condition and Order
lm1.1 <- lmer(Know ~ Condition * Order + (1 | Subj), d1)
summary(lm1.1)
lm1.2 <- lmer(Know ~ Condition + Order + (1 | Subj), d1)
anova(lm1.1,lm1.2)
lm1.3 <- lmer(Know ~ Order + (1 | Subj), d1)
anova(lm1.2,lm1.3)
lm1.4 <- lmer(Know ~ Condition + (1 | Subj), d1)
anova(lm1.2,lm1.4)

## second trial only analyses
d1 <- subset(d1,Lang=="English" & Order=="Second Trial") ## remove those who didn't speak English and select 1st trial data

var.test(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"])
print(ttest.1 <- t.test(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"],var.equal=TRUE))
cohensD(d1$Know[d1$Condition=="All true"],d1$Know[d1$Condition=="Mixed"])

#Descriptives
print(d1.describe <- aggregate(Know ~ Condition * Order, d1, FUN=function(x) c(M =mean(x), SD =sd(x))))

#graph
d1.plot <- ggplot(d1, aes(x=Condition,y=Know,fill=Condition)) +
  ylab("Knowledge Ascription Agreement") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  #facet_grid(. ~ Order) +
  geom_boxplot() + scale_fill_discrete(name="Agent's Beliefs") +
  geom_jitter(aes(colour=Condition), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30, name="Agent's Beliefs") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.75)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
  )
d1.plot 
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig1.png")


# Study 2 ------------------------------------------------------------

#knows where compliment labels
d2.1$Comp <- "Can"
d2.2$Comp <- "Can"
d2.3$Comp <- "Should"
d2.4$Comp <- "Should"
d2.5$Comp <- "To"
d2.6$Comp <- "To"

#negation lables
d2.1$Negation <- "Knows where"
d2.2$Negation <- "Doesn't know where"
d2.3$Negation <- "Knows where"
d2.4$Negation <- "Doesn't know where"
d2.5$Negation <- "Knows where"
d2.6$Negation <- "Doesn't know where"

d2 <- rbind(d2.1,d2.2,d2.3,d2.4,d2.5,d2.6)

d2$Subj <- rep(1:length(d2$Consent)) ##wait... does this not fuck this up, isn't it already in long form? Assign subj # after subsetting?
d2$Condition <- factor(c("All true","Mixed","All false")[d2$Condition])
d2$Condition <- factor(d2$Condition, levels=c("All true","Mixed","All false"))
d2$Order <- factor(c("First Trial","Second Trial")[d2$Order])
d2$Lang <- factor(c("English","Not English")[d2$Lang-20])
d2$Comp <- factor(d2$Comp, levels=c("To","Can","Should"))
d2$Negation <- factor(d2$Negation, levels=c("Knows where","Doesn't know where"))

##first trial analyses

d2 <- subset(d2,Lang=="English" & Order=="First Trial")

## WARNING: Decide here if you want to reverse code the negated statements for all analyses. Otherwise, the main 
###effect of negation and interaction effect aren't particularly informative 
## If you decide to do so, you'll then need to switch back to get the graph display correctly
## And here's the code for doing it:
d2$Know[d2$Negation=="Doesn't know where"] <- 8 - d2$Know[d2$Negation=="Doesn't know where"]

## ovearll model
lm2.1 <- lmer(Know ~ Condition * Negation + (1|Comp), d2)
summary(lm2.1)
### Belief * Negation interaction effect 
lm2.2 <- lmer(Know ~ Condition + Negation + (1|Comp), d2)
anova(lm2.1,lm2.2)
### main effect of belief
lm2.3 <- lmer(Know ~ Negation + (1|Comp), d2)
anova(lm2.2,lm2.3)
### main effect of negation
lm2.4 <- lmer(Know ~ Condition + (1|Comp), d2)
anova(lm2.2,lm2.4)

## Standard: All true vs. mixed
ttest2.1 <- t.test(
                  d2$Know[d2$Negation=="Knows where" & d2$Condition=="All true"],
                  d2$Know[d2$Negation=="Knows where" & d2$Condition=="Mixed"]
                  )
print(ttest2.1)
CohensD2.1 <- cohensD(
                  d2$Know[d2$Negation=="Knows where" & d2$Condition=="All true"],
                  d2$Know[d2$Negation=="Knows where" & d2$Condition=="Mixed"]
                  )
print(CohensD2.1)

## Standard: Mixed vs. all false
ttest2.2 <- t.test(
  d2$Know[d2$Negation=="Knows where" & d2$Condition=="Mixed"],
  d2$Know[d2$Negation=="Knows where" & d2$Condition=="All false"]
)
print(ttest2.2)
CohensD2.2 <- cohensD(
  d2$Know[d2$Negation=="Knows where" & d2$Condition=="Mixed"],
  d2$Know[d2$Negation=="Knows where" & d2$Condition=="All false"]
)
print(CohensD2.2)

## Negated: All true vs. Mixed
ttest2.3 <- t.test(
                  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="All true"],
                  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="Mixed"]
                  )
print(ttest2.3)
CohensD2.3 <- cohensD(
                  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="All true"],
                  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="Mixed"]
                  )
print(CohensD2.3)

## Negated: Mixed vs. all false
ttest2.4 <- t.test(
  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="Mixed"],
  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="All false"]
)
print(ttest2.4)
CohensD2.4 <- cohensD(
  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="Mixed"],
  d2$Know[d2$Negation=="Doesn't know where" & d2$Condition=="All false"]
)
print(CohensD2.4)

print(d2.descip <- aggregate(Know ~ Condition*Negation, d2, FUN=function(x) c(M =mean(x), SD =sd(x))))
print(d2.descip <- aggregate(Know ~ Negation, d2, FUN=function(x) c(M =mean(x), SD =sd(x))))

## this is the interaction test (reverse coding negated statements and excluding all false cases)

d2.x <- subset(d2, d2$Condition !="All false")
d2.x$Know[d2.x$Negation=="Doesn't know where"] <- 8 - d2.x$Know[d2.x$Negation=="Doesn't know where"] 

lm2.x1 <- lmer(Know ~ Condition * Negation + (1|Comp), d2.x)
lm2.x2 <- lmer(Know ~ Condition + Negation + (1|Comp), d2.x)
anova(lm2.x1,lm2.x2)

d2.plot <- ggplot(d2, aes(x=Condition,y=Know,fill=Condition)) +
  ylab("Knowledge Ascription Agreement") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  facet_grid(~ Negation) +
  geom_boxplot() +  scale_fill_discrete(name="Agent's Beliefs") +
  geom_jitter(aes(colour=Condition), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30, name="Agent's Beliefs") +
  theme(
      plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.75)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
    ,strip.text = element_text(size=rel(1.5))
    ,panel.margin=unit(1,"lines")
  )
d2.plot
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig2.png")

# Study 3 Digression on Knowledge that

d3$Subj <- factor(d3$Subj)
d3$Condition <- factor(c("All true","Mixed","All false")[d3$Condition])
d3$Condition <- factor(d3$Condition, levels=c("All true",'Mixed',"All false"))
d3$Order <- factor(c("First Trial","Second Trial")[d3$Order])
d3$Lang <- factor(c("English","Not English")[d3$Lang-20])

d3 <- subset(d3,Lang=="English" & Order=="First Trial") ## remove those who didn't speak English

var.test(d3$Know[d3$Condition=="All true"],d3$Know[d3$Condition=="Mixed"])
print(ttest3.1 <- t.test(d3$Know[d3$Condition=="All true"],d3$Know[d3$Condition=="Mixed"]))
print(CohensD3.1 <- cohensD(d3$Know[d3$Condition=="All true"],d3$Know[d3$Condition=="Mixed"]))

#one sample ttest for all False 
shapiro.test(d3$Know[d3$Condition=="All false"])
print(ttest3.2 <- t.test(d3$Know[d3$Condition=="All false"],mu=4,alternative="less"))

print(d3.descip <- aggregate(Know ~ Condition, d3, FUN=function(x) c(M =mean(x), SD =sd(x))))

d3.plot <- ggplot(d3, aes(x=Condition,y=Know,fill=Condition)) +
  ylab("Know-that Acription") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  #facet_grid(. ~ Order) +
  geom_boxplot() + scale_fill_discrete(name="Agent's Beliefs") +
  geom_jitter(aes(colour=Condition), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30, name="Agent's Beliefs") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.25)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
  )
d3.plot 
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig3.png")

############ Add in the within-subjects comparisons here


# Study 4 Naive Relevance-Implicature Approach -------------------------------------------------
##d4 <- read.csv("know_wh_study4_Rdata.csv")

d4$Subj <- factor(d4$Subj)
d4$Condition <- factor(c("All true","Mixed","All false")[d4$Condition])
d4$Condition <- factor(d4$Condition, levels=c("All true",'Mixed',"All false"))
d4$Language <- factor(c("Shared","Unshared")[d4$Language])
d4$Language <- factor(d4$Language, levels = c("Shared","Unshared"))
d4$Lang <- factor(c("English","Not English")[d4$Lang-20])

d4 <- subset(d4,Lang=="English") ## remove those who didn't speak English
d4 <- d4[which((d4$Language=="Shared" & d4$Control_Bob==9)|(d4$Language=="Unshared" & d4$Control_Bob==10)),]

d4.shared <- d4[which(d4$Language=="Shared"),]
d4.noShared <- d4[which(d4$Language=="Unshared"),]

##Felicity analyses and graph

lm4.F <- lm(Felicity ~ Condition * Language, data=d4)
anova(lm4.F)
etaSquared(lm4.F)

d4.F.descrip <- aggregate(Felicity ~ Condition + Language, d4, FUN=function(x) c(M =mean(x), SD =sd(x)))

###Shared lang
print(ttest4.1 <- t.test(d4.shared$Felicity[which(d4.shared$Condition=="All true")],d4.shared$Felicity[which(d4.shared$Condition=="Mixed")]))
cohensD(d4.shared$Felicity[which(d4.shared$Condition=="All true")],d4.shared$Felicity[which(d4.shared$Condition=="Mixed")])

###No shared lang
print(ttest4.2 <- t.test(d4.noShared$Felicity[which(d4.noShared$Condition=="All true")],d4.noShared$Felicity[which(d4.noShared$Condition=="Mixed")]))
cohensD(d4.noShared$Felicity[which(d4.noShared$Condition=="All true")],d4.noShared$Felicity[which(d4.noShared$Condition=="Mixed")])

### all false
print(ttest4.3 <- t.test(d4.shared$Felicity[which(d4.shared$Condition=="All false")],d4.noShared$Felicity[which(d4.noShared$Condition=="All false")]))
cohensD(d4.shared$Felicity[which(d4.shared$Condition=="All false")],d4.noShared$Felicity[which(d4.noShared$Condition=="All false")])

d4.F.plot <- ggplot(d4, aes(x=Language,y=Felicity,fill=Language)) +
  ylab("Usefulness Rating") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  facet_grid(. ~ Condition) +
  geom_boxplot() +
  geom_jitter(aes(colour=Language), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30)+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.75)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
    ,strip.text = element_text(size=rel(1.5))
  )
d4.F.plot 
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig4a.png")


## Knowledge analyses and graph

lm4.K <- lm(Know~Condition*Language, d4)
anova(lm4.K)
etaSquared(lm4.K)

d4.K.descip <- aggregate(Know ~ Condition + Language, d4, FUN=function(x) c(M =mean(x), SD =sd(x)))

################ update based on variance tests

#var.test(d4.noShared$Know[which(d4.noShared$Condition=="All true")],d4.noShared$Know[which(d4.noShared$Condition=="Mixed")])
print(ttest4.4 <- t.test(d4.noShared$Know[which(d4.noShared$Condition=="All true")],d4.noShared$Know[which(d4.noShared$Condition=="Mixed")]))
cohensD(d4.noShared$Know[which(d4.noShared$Condition=="All true")],d4.noShared$Know[which(d4.noShared$Condition=="Mixed")])

#var.test(d4.shared$Know[which(d4.shared$Condition=="All true")],d4.shared$Know[which(d4.shared$Condition=="Mixed")])
print(ttest4.4 <- t.test(d4.shared$Know[which(d4.shared$Condition=="All true")],d4.shared$Know[which(d4.shared$Condition=="Mixed")]))
cohensD(d4.shared$Know[which(d4.shared$Condition=="All true")],d4.shared$Know[which(d4.shared$Condition=="Mixed")])

#var.test(d4.shared$Know[which(d4.shared$Condition=="Mixed")],d4.noShared$Know[which(d4.noShared$Condition=="Mixed")])
print(ttest4.5 <- t.test(d4.shared$Know[which(d4.shared$Condition=="Mixed")],d4.noShared$Know[which(d4.noShared$Condition=="Mixed")]))
cohensD(d4.shared$Know[which(d4.shared$Condition=="Mixed")],d4.noShared$Know[which(d4.noShared$Condition=="Mixed")])

d4.K.plot <- ggplot(d4, aes(x=Language,y=Know,fill=Language)) +
  ylab("Knowledge Ascription Agreement") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  facet_grid(. ~ Condition) +
  geom_boxplot() +
  geom_jitter(aes(colour=Language), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30)+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.75)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
    ,strip.text = element_text(size=rel(1.5))
  )
d4.K.plot
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig4b.png")

######## Analyzing with mixed linear models instead

##melt data 
d4.Long <- melt(d4[,c(1:4,8)],id.var=c("Subj","Condition","Language"))
colnames(d4.Long) <- c("Subject","Beliefs","Language","Question","Judgment")

lmr4.1 <- lmer(Judgment ~ Question * Language * Beliefs + (1|Subject), d4.Long)
lmr4.2 <- lmer(Judgment ~ Question*Language + Question*Beliefs + Language*Beliefs + (1|Subject), d4.Long)
anova(lmr4.1,lmr4.2)

# Study 5 Knows How/Who ------------------------------------------------------------

d5.1$Question <- "Knows How"
d5.2$Question <- "Knows Who"

### mention-some test for the knows who version.

d5.3 <- subset(d5.2, d5.2$Control==1 & d5.2$Condition==4) 
d5.3$Lang <- factor(c("English","Not English")[d5.3$Lang-20])
d5.3 <- subset(d5.3,Lang=="English") ##All were native?

t.test(d5.3$Know,mu=4, alternative="greater")

mean(d5.3$Know)
sd(d5.3$Know)

##exclude those in the know-who version who completed the mention some test or
###who failed the comprehension question, which asked them to indicate who actually had an adapter
d5.2 <- subset(d5.2, d5.2$Control==1 & d5.2$Condition!=4) 
## 51 participants completed the mention-some test
## 21 were excluded based on failing control question

d5 <- rbind(d5.1[,c(1:4,6,20)],d5.2[,c(1:4,9,23)])

d5$Condition <- factor(c("All true","Mixed","All false")[d5$Condition])
d5$Condition <- factor(d5$Condition, levels=c("All true","Mixed","All false"))
d5$Lang <- factor(c("English","Not English")[d5$Lang-20])

d5 <- subset(d5,Lang=="English")
## 16 were excluded because English was not their native langauge

lm5.1 <- lm(Know ~ Condition * Question, d5)
anova(lm5.1)
etaSquared(lm5.1)

print(d5.descip1 <- aggregate(Know ~ Condition, d5, FUN=function(x) c(M =mean(x), SD =sd(x))))

##Main effect of Condition
d5.allTrue <- subset(d5,d5$Condition=="All true")
d5.mixed <- subset(d5,d5$Condition=="Mixed")
d5.allFalse <- subset(d5,d5$Condition=="All false")

var.test(d5.allTrue$Know,d5.mixed$Know)
t.test(d5.allTrue$Know,d5.mixed$Know,equal.var=TRUE)
cohensD(d5.allTrue$Know,d5.mixed$Know)

var.test(d5.mixed$Know,d5.allFalse$Know)
t.test(d5.mixed$Know,d5.allFalse$Know)
cohensD(d5.mixed$Know,d5.allFalse$Know)

print(d5.descip2 <- aggregate(Know ~ Condition, d5, FUN=function(x) c(M =mean(x), SD =sd(x))))

##comparison of the knows-who mixed condition and the mention-some test
var.test(d5.who$Know[d5.who$Condition=="Mixed"],d5.3$Know)
t.test(d5.who$Know[d5.who$Condition=="Mixed"],d5.3$Know)
cohensD(d5.who$Know[d5.who$Condition=="Mixed"],d5.3$Know)
## Know-who descriptives
mean(d5.who$Know[d5.who$Condition=="Mixed"])
sd(d5.who$Know[d5.who$Condition=="Mixed"])

##Main effect of Question/Scenario
d5.how <- subset(d5,d5$Question=="Knows How")
d5.who <- subset(d5,d5$Question=="Knows Who")

var.test(d5.how$Know,d5.who$Know)
t.test(d5.how$Know,d5.who$Know, equal.var=TRUE)
cohensD(d5.how$Know,d5.who$Know)

print(d5.descip3 <- aggregate(Know ~ Question, d5, FUN=function(x) c(M =mean(x), SD =sd(x))))


d5.plot <- ggplot(d5, aes(x=Condition,y=Know,fill=Condition)) +
  ylab("Knowledge Ascription Agreement") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  facet_grid(. ~ Question) +
  geom_boxplot() + scale_fill_discrete(name="Agent's Beliefs") +
  geom_jitter(aes(colour=Condition), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30, name="Agent's Beliefs") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.25)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
    ,strip.text = element_text(size=rel(1.5))
    ,panel.margin=unit(1,"lines")
  )
d5.plot
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig5.png")

### Notes

d5$Version <- factor(c("Presentation 1","Presentation 2")) 
##
## in presentation 1: the store six miles south is mentioned second and wrong in the mixed condition
## in presentation 2: the store four miles NE is mentioned second and wrong in the mixed condition


##If you want to exclude participants who went too quickly, I'd suggest something like the following:
# d5.time_cut <- quantile(d5$Time,.1,na.rm=T)
# print(d5.time_cut)
# d5 <- subset(d5,Time>d5.time_cut)


# Study 6 - Proportionality Test -------------------------------------------
##d6 <- read.csv("know_wh_study6_Rdata.csv")

## Go back through and change Proportion <- Condition

d6$Condition <- factor(c("3 of 3 false","2 of 3 false","1 of 3 false","0 of 3 false")[d6$Condition+1])
d6$Condition <- factor(d6$Condition, levels = c("0 of 3 false","1 of 3 false","2 of 3 false","3 of 3 false"))
d6$Lang <- factor(c("English","Not English")[d6$Lang-20])

d6 <- subset(d6,Lang=="English")

# d6.time_cut <- quantile(d6$Time,.1,na.rm=T)
# print(d6.time_cut)
# d6 <- subset(d6,Time>d6.time_cut)

d6.plot <- ggplot(d6, aes(x=Condition,y=Know,fill=Condition)) +
  ylab("Knowledge Ascription Agreement") +
  xlab("") +
  coord_cartesian(ylim = c(1, 7)) +
  geom_boxplot() + scale_fill_discrete(name="Belief proportion") +
  geom_jitter(aes(colour=Condition), position = position_jitter(width = .1), alpha = 0.5)+
  scale_colour_hue(c=50, l=30, name="Belief proportion")+ 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_text(size=rel(1.5))
    ,legend.text=element_text(size=rel(1.75))
    ,axis.text.x = element_blank()
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.75)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.75))
    ,strip.text = element_text(size=rel(1.5))
  )
d6.plot
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig6.png")

#analyses
lm.6 <- lm(Know~Proportion, data=d6)
anova(lm.6)
etaSquared(lm.6)

print(d6.descip <- aggregate(Know ~ Proportion, d6, FUN=function(x) c(M =mean(x), SD =sd(x))))

d6.0of3 <- subset(d6,Proportion=="0 of 3 false")
d6.1of3 <- subset(d6,Proportion=="1 of 3 false")
d6.2of3 <- subset(d6,Proportion=="2 of 3 false")
d6.3of3 <- subset(d6,Proportion=="3 of 3 false")

##0 of 3 vs. 1 of 3
var.test(d6.0of3$Know,d6.1of3$Know)
t.test(d6.0of3$Know,d6.1of3$Know,equal.var=TRUE)
cohensD(d6.0of3$Know,d6.1of3$Know)

##1 of 3 vs. 2 of 3
var.test(d6.1of3$Know,d6.2of3$Know)
t.test(d6.1of3$Know,d6.2of3$Know,equal.var=TRUE)
cohensD(d6.1of3$Know,d6.2of3$Know)

##2 of 3 vs. 3 of 3
var.test(d6.2of3$Know,d6.3of3$Know)
t.test(d6.2of3$Know,d6.3of3$Know,equal.var=TRUE)
cohensD(d6.2of3$Know,d6.3of3$Know)

### Meta-Analytic Graph ###

##Edits to make data consistent
d2$Know[d2$Negation=="Doesn't know where"] <- 8 - d2$Know[d2$Negation=="Doesn't know where"]

d5$Study <- "5"

d6 <- read.csv("know_wh_study6_Rdata.csv")
d6$Study <- "6"
d6$Condition <- factor(c("All false","Mixed","Mixed","All true")[d6$Condition+1])
d6$Lang <- factor(c("English","Not English")[d6$Lang-20])
d6 <- subset(d6,Lang=="English")

d <- rbind(d1[,c(2:3,20)],d2[,c(3,5,23)],d4[,c(2,4,23)],d5[,c(2:3,7)],d6[,c(3:4,20)])

ds <- aggregate(Know ~ Study + Condition, d, mean)
ds$ci.h <- aggregate(Know ~ Study + Condition, d, ci.high)$Know
ds$ci.l <- aggregate(Know ~ Study + Condition, d, ci.low)$Know

do <- aggregate(Know ~ Condition, d, mean)
do$ci.h <- aggregate(Know ~ Condition, d, ci.high)$Know
do$ci.l <- aggregate(Know ~ Condition, d, ci.low)$Know
do$Study <- "Overall"
  
ds <- rbind(ds,do)

ds <- ds[order(ds$Condition,ds$Study),]

d.plot <- ggplot(ds, aes(x=Study,y=Know, colour=Condition)) +
  ylab("Knowledge Ascription Agreement") +
  xlab("Study") +
  coord_cartesian(ylim=c(1,7)) +
  facet_grid(.~Condition) +
  geom_point() +
  geom_errorbar(aes(ymin=Know - ds$ci.l, ymax=Know + ds$ci.h),width=0.2) + 
  scale_colour_hue(c=50, l=30, name="Agent's Beliefs") +
  geom_vline(xintercept=5.45,lty=2) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="none"
    ,axis.text.y=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.title.y=element_text(vjust=.95)
    ,axis.title.x=element_text(vjust=.25)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size=rel(1.5))
    ,panel.margin=unit(1,"lines")
  )
d.plot
#ggsave(file="C:/Users/Jonathan/Dropbox/Know-Wh Studies/allMaterials/Figures/Fig7.png")

