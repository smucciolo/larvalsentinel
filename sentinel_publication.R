#code associated with manuscript
#"The impact of milkweed species on monarch survival and predation in a backyard garden"
#by S. Mucciolo, updated 4.30.2025

#download packages
library(tidyverse)
library(survival)
library(survminer)

#larval analysis ####
#QUESTION: Does larval survival vary between milkweed species?
#ANALYSIS: kaplan-meier survival estimate & log rank test

#data set where each larvae only had one entry: 
  #in status column, 1=died (the event) and 0=stayed alive the whole time ("censored")
larvae_kaplan<-read.csv("https://raw.githubusercontent.com/smucciolo/larvalsentinel/main/2023%20sentinel%20data_kaplan%20meier.csv")

#create Kaplan Meier survival estimate 
larvae_fullfit<-survfit(Surv(hours, status) ~ species+trt, data = larvae_kaplan)
print(larvae_fullfit)
summary(larvae_fullfit)

#is there a significant difference in survival between treatments? -> log rank
larvae_logrank<-survdiff(Surv(hours, status) ~ species+trt, data = larvae_kaplan)
print(larvae_logrank)
  #across all treatments, p<0.001

#which pairs are significant?
pairwise_survdiff(Surv(hours,status)~species+trt,data=larvae_kaplan)
  #no significant difference between either of the protected or exposed treatments 
  #but protected vs exposed are significantly different

#egg sentinels including control ####
updatedegg<-read.csv("https://raw.githubusercontent.com/smucciolo/larvalsentinel/refs/heads/main/eggkaplan_withcontrol_9.24.2024.csv")

#creating kaplan-meier
egg_fullfit<-survfit(Surv(TIME_POST_DEPLOY,STATUS)~SPECIES+TRT,data=updatedegg)
print(egg_fullfit)
summary(egg_fullfit)

#log rank to check if there are any significant differences
egg_full_logrank<-survdiff(Surv(TIME_POST_DEPLOY,STATUS)~SPECIES+TRT,data=updatedegg)
print(egg_full_logrank)
  #Yes! There is a significant difference between treatments 

#pairwise comparison
pairwise_survdiff(Surv(TIME_POST_DEPLOY,STATUS)~SPECIES+TRT,data=updatedegg)
  #sig difference between all treatments EXCEPT FOR between the two controls, which makes sense 


#making larval graph ####
larvae_ggsurv<-ggsurvplot(larvae_fullfit,pval = FALSE, conf.int=FALSE,
                          risk.table = FALSE, risk.table.col = "strata",
                          linetype = c("dotted","solid","dotted","solid"),
                          surv.median.line = "none", 
                          ggtheme = theme_classic(base_size=20,base_family="Avenir"),
                          palette = c("black", "black","gray","gray"),
                          legend.labs=c("A. incarnata, protected","A. incarnata, exposed",
                                        "A. syriaca, protected","A. syriaca, exposed"),
                          legend.title="Treatment",
                          ylab="Larval survival probability",
                          xlab="Time of day",
                          censor=FALSE,
                          legend=c(0.17,0.2),
                          position=position_dodge(width=0.5),
                          font.x=30,font.y=30,font.legend=20,
                          font.tickslab=20,xlim=c(0,28),ylim=c(0,1),
                          size=2)
larvae_ggsurv

#adding significance letters to the graph above
larvae_plot<-larvae_ggsurv$plot+
  ggplot2::annotate(geom="text",x=26.8,y=1,label="a",size=8)+
  ggplot2::annotate(geom="text",x=26.8,y=0.5,label="b",size=8)+
  ggplot2::annotate(geom="text",x=26.8,y=0.265,label="b",size=8)+
  scale_x_continuous(expand = c(0, 0, 0, 0),
                     breaks=c(0,8,16,24),
                     labels=c("15:00","23:00","02:00","15:00")) +
  scale_y_continuous(expand = c(0, 0, .05, 0))
larvae_plot
#exported at 1200 x 620 for manuscript

#making egg graph ####
egg_update_ggsurv<-ggsurvplot(egg_fullfit,pval = FALSE, conf.int=FALSE,
                              risk.table = FALSE, risk.table.col = "strata",
                              linetype = c("dotted","solid","dotted","solid"),
                              surv.median.line = "none", 
                              ggtheme = theme_classic(base_size=20,base_family="Avenir"),
                              palette = c("black", "black","gray","gray"),
                              legend.labs=c("A. incarnata, protected","A. incarnata, exposed",
                                            "A. syriaca, protected","A. syriaca, exposed"),
                              legend.title="Treatment",
                              ylab="Egg survival probability",
                              xlab="Time of day",
                              censor=FALSE,
                              legend=c(0.17,0.2),
                              position=position_dodge(width=0.5),
                              font.x=30,font.y=30,font.legend=20,
                              font.tickslab=20,xlim=c(0,28),ylim=c(0,1),
                              size=2)
egg_update_ggsurv

#adding significance letters
eggplot<-egg_update_ggsurv$plot+
  ggplot2::annotate(geom="text",x=26.8,y=0.44,label="b",size=8)+
  ggplot2::annotate(geom="text",x=26.8,y=0.205,label="c",size=8)+
  ggplot2::annotate(geom="text",x=26.8,y=1,label="a",size=8)+
  ggplot2::annotate(geom="text",x=26.8,y=0.933,label="a",size=8)+
  scale_x_continuous(expand = c(0, 0, 0, 0),
                     breaks=c(0,8,16,24),
                     labels=c("10:00","18:00","02:00","10:00"))+
  scale_y_continuous(expand = c(0, 0, .05, 0))

eggplot
#exported at 1200 x 620 for manuscript

