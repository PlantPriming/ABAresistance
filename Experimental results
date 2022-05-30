if (!require('agricolae')) install.packages('agricolae'); library('agricolae')
if (!require('forcats')) install.packages('forcats'); library('forcats')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('ggstatsplot')) install.packages('ggstatsplot'); library('ggstatsplot')
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
if (!require('car')) install.packages('car'); library('car')
if (!require('DescTools')) install.packages('DescTools'); library('DescTools')


###4-week old leaves infected with Botrytis cinerea ###
botrytis4weeks<-read.table("bcin_4.csv", header=TRUE,sep=",")
treatmentorderb4<-factor(botrytis4weeks$Treatment, levels=c('Water','20','50','100','500'),ordered = TRUE)

#Pre statistical tests for normality and variance#
shapiro.test(botrytis4weeks$Lesion.diameter)
leveneTest(botrytis4weeks$Lesion.diameter ~botrytis4weeks$Treatment)

#ANOVA for 4-week Botrytis infection#
AOV_botrytis4<-aov(botrytis4weeks$Lesion.diameter ~botrytis4weeks$Treatment)
summary(AOV_botrytis4)

#Post-Hoc test# LSD post hoc for 4-week Botrytis
botrytis4model<-aov(Lesion.diameter~treatmentorderb4, data=botrytis4weeks)
botrytis4_posthoc <- LSD.test(botrytis4model,"treatmentorderb4", p.adj="bonferroni")
plot(botrytis4_posthoc)
botrytis4_posthoc

#Results plot 4-week Botrytis
ggplot(botrytis4weeks, aes(x=treatmentorder4, y=Lesion.diameter)) + 
  geom_boxplot(fill=c('#FFFFFF','grey90','grey70','grey50','grey30')) +
  ylab("Lesion Diameter (mm)")+
  theme(panel.background = element_blank())+
  xlab("ABA Concentration (μM)")+
  scale_fill_brewer(palette="Greys")+
  ylim(3,5)+
  theme(axis.line = element_line(color="black", size = 0.3))



###8-week old leaves infected with Botrytis cinerea ###
botrytis8weeks<-read.table("bcin_8.csv", header=TRUE,sep=",")
treatmentorderb8<-factor(botrytis8weeks$Treatment, levels=c('Water','20','50','100','500'),ordered = TRUE)

#Pre statistical tests for normality and variance#
shapiro.test(botrytis8weeks$Lesion.diameter)
leveneTest(botrytis8weeks$Lesion.diameter ~botrytis8weeks$Treatment)


#ANOVA for 8-week Botrytis infection#
AOV_botrytis8<-aov(botrytis8weeks$Lesion.diameter ~botrytis8weeks$Treatment)
summary(AOV_botrytis8)

##Dunnett post-hoc test##
botrytis8weeks$Treatment<- as.factor(botrytis8weeks$Treatment)
DunnettTest(x= botrytis8weeks$Lesion.diameter, g= botrytis8weeks$Treatment, control = "Water")

#Results plot 8-weeks Botrytis infection
ggplot(botrytis8weeks, aes(x=treatmentorder8, y=Lesion.diameter)) + 
  geom_boxplot(fill=c('#FFFFFF','grey90','grey70','grey50','grey30')) +
  ylab("Lesion Diameter (mm)")+
  theme(panel.background = element_blank())+
  xlab("ABA Concentration (μM)")+
  scale_fill_brewer(palette="Greens", direction = 1)+
  theme(axis.line = element_line(color="black", size = 0.3))+
  ylim(1.5, 9)


#4-week old P. infestans infected #
phytophthora4weeks<-read.table("pinf_4.csv",header=TRUE,sep=",")
treatmentorder4<-factor(phytophthora4weeks$Treatment, levels=c('Water','20','50','100','500'),ordered = TRUE)

#Pre statistical tests for normality and variance#
shapiro.test(phytophthora4weeks$DSI)
leveneTest(phytophthora4weeks$DSI~phytophthora4weeks$Treatment)

#Kruskall Wallis test as non normal data#
kruskal.test(phytophthora4weeks$DSI~phytophthora4weeks$Treatment)

#Post hoc test#
phytophthora4weeks$Treatment<- as.factor(phytophthora4weeks$Treatment)
DunnettTest(x= phytophthora4weeks$DSI, g= phytophthora4weeks$Treatment, control = "Water")

#Results plots 4-week Phytophthora infection
ggplot(phytophthora4weeks, aes(x=treatmentorder4, y=DSI)) + 
  geom_boxplot(fill=c('#FFFFFF','grey90','grey70','grey50','grey30')) +
  ylab("Disease severity index (%)")+
  theme(panel.background = element_blank())+
  xlab("ABA Concentration (μM)")+
  scale_fill_brewer(palette="Blues", direction = 1)+
  theme(axis.line = element_line(color="black", size = 0.3))


#8-week old P. infestans infected #
phytophthora8weeks<-read.table("pinf_8.csv",header=TRUE,sep=",")
treatmentorderPI8<-factor(phytophthora8weeks$Treatment, levels=c('Water','20','50','100','500'),ordered = TRUE)

#Pre statistical tests for normality and variance#
shapiro.test(phytophthora8weeks$DSI)
leveneTest(phytophthora8weeks$DSI~phytophthora8weeks$Treatment)

#ANOVA as normal data
AOV_phytophthora8<-aov(phytophthora8weeks$DSI~phytophthora8weeks$Treatment)
summary(AOV_phytophthora8)

#Post hoc test#
phytophthora8model<-aov(DSI~treatmentorderPI8, data=phytophthora8weeks)
phytophthora8_posthoc <- LSD.test(phytophthora8model,"treatmentorderPI8", p.adj="bonferroni")
plot(phytophthora8_posthoc)
phytophthora8_posthoc

#Results plots 8-week Phytophthora infection
ggplot(phytophthora8weeks, aes(x=treatmentorder8, y=DSI)) + 
  geom_boxplot(fill=c('#FFFFFF','grey90','grey70','grey50','grey30')) +
  ylab("Disease severity index (%)")+
  theme(panel.background = element_blank())+
  xlab("ABA Concentration (μM)")+
  scale_fill_brewer(palette="Blues", direction = 1)+
  theme(axis.line = element_line(color="black", size = 0.3))


