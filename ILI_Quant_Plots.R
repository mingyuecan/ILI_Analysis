## Note: There are some problems in installing some packages on Windows system. 
## It would be better to run the codes on a Macbook.

## Load library
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(stringr)

## Load Data
setwd("/Users/Constance/Desktop/ILI_Qualitative")
new <- read.csv("Quant_Data.csv", sep = ",", header = TRUE, check.names = FALSE)

questions <- names(new)
comp <- questions[seq(1, 36, 6)]
sk<-questions[seq(2, 6, 1)]
sm<-questions[seq(8, 12, 1)]
ref<-questions[seq(14, 18, 1)]
emp<-questions[seq(20, 24, 1)]
open<-questions[seq(26, 30, 1)]
inte<-questions[seq(32, 36, 1)]

Competencies<-new[comp]
SelfKnowledge<-new[sk]
SelfManagement<-new[sm]
Reflection<-new[ref]
Empathy<-new[emp]
Openness<-new[open]
Integrity<-new[inte]


## plot function
draw_subjects <- function(s,w) {
  df_1<-s %>% 
    summarise_all(funs(mean)) %>% 
    gather(Var, Mean)
  
  df_2<-s %>% 
    summarise_all(funs(median)) %>% 
    gather(Var, Median)
  
  df_3<-s %>% 
    summarise_all(funs(sd)) %>% 
    gather(Var, SD)
  
  df_4<-merge(df_1,df_2,by="Var",sort=F)
  df_x<-merge(df_4, df_3, by="Var",sort=F)
  
  new_df<-melt(df_x, id.vars="Var", varname=c('Variable', 'Statistics'))
  new_df$Var <- factor(new_df$Var, levels = unique(new_df$Var))
  
  ggplot(new_df,aes(x=Var,y=value,fill=factor(variable)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_discrete(name="Measures",
                        breaks=c(1, 2, 3),
                        labels=c("Mean", "Median", "SD"))+
    labs(title = deparse(substitute(s)), x = "")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(aes(label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25)+
    scale_x_discrete(labels = function(x) str_wrap(x, width = w))
}

## Plot statistics of six competencies
draw_subjects(Competencies,15)

## Plot statistics of subtopics:
draw_subjects(SelfKnowledge,20)
draw_subjects(SelfManagement,20)
draw_subjects(Reflection,20)
draw_subjects(Empathy,20)
draw_subjects(Openness,20)
draw_subjects(Integrity,20)


## Draw Histogram
themes <- questions[seq(1, 36, 6)]

subtopics <- questions[-seq(1, 36, 6)]

draw_themes <- function(t) {
  ggplot(data = new, aes(new[t])) + geom_histogram(binwidth = 5,
                                                   col = "black",
                                                   fill = "lightskyblue2") + 
    labs(title = t, x = "", y = "Count") + 
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = mean(new[t][, 1])),
      color = "red",
      linetype = "dashed",
      size = 0.5) +
    geom_vline(aes(xintercept = median(new[t][, 1])),
      color = "green",
      linetype = "dashed",
      size = 0.5)
  
}

draw_subtopics <- function(s) {
  p <- gsub('(.{1,58})(\\s|$)', '\\1\n', s)
  ggplot(data = new, aes(new[s])) + geom_histogram(binwidth = 0.5,
                                                   col = "black",
                                                   fill = "lightskyblue2") +
    labs(title = p, x = "", y = "Count") +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = mean(new[s][, 1])),
      color = "red",
      linetype = "dashed",
      size = 0.5) +
    geom_vline(aes(xintercept = median(new[s][, 1])),
      color = "green",
      linetype = "dashed",
      size = 0.5)
}

for (i in 1:length(themes)) {
  print(draw_themes(themes[i]))
}

for (i in 1:length(subtopics)) {
  print(draw_subtopics(subtopics[i]))
}


