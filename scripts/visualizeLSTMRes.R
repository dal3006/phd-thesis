#! /usr/bin/Rscript
library(plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(latex2exp)
library(directlabels)

setwd("Manuscripts/phd-thesis/data")

lstmres <- read.csv("lstmresults.csv")

lstmres$hour <- as.factor(as.character(lstmres$hour))

rmseres <- lstmres[lstmres$metric == "RMSE",]
ccres <- lstmres[lstmres$metric == "CC",]

ggplot(rmseres, aes(x=hour, y=value, fill=model)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(aes(label = round(value, digits = 2), group=model), size=5, position=position_dodge2(.9), vjust=.5, angle = 90, hjust=-.2) + 
  theme_gray(base_size = 20) +
  scale_fill_viridis_d(
    name = "Model", 
    labels = c("Bala & Reiff 2012", "GPNN", "GPNN - GPS", "Lazzus et. al 2017", "Persistence", "Wu & Lundstedt 1997"), 
    alpha = 1.) +
  theme(legend.position="top", legend.direction = "horizontal") +
  scale_x_discrete(
    name = "Prediction Horizon",
    breaks=c("1","2","3","4","5","6"),
    labels=c(
      parse(text = TeX('$t+1$')), parse(text = TeX('$t+2$')), parse(text = TeX('$t+3$')), 
      parse(text = TeX('$t+4$')), parse(text = TeX('$t+5$')), parse(text = TeX('$t+6$')))) +
  ylab("RMSE")

ggsave(
  filename = "lstmCompRMSE.pdf", 
  scale = 1.0)

ggplot(ccres, aes(x=as.factor(as.character(hour)), y=value, fill=model)) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(aes(label = round(value, digits = 2), group=model), size=5, position=position_dodge2(.9), vjust=.5, angle = 90, hjust=-.2) + 
  theme_gray(base_size = 20) +
  scale_fill_viridis_d(
    name = "Model", 
    labels = c("Bala & Reiff 2012", "GPNN", "GPNN - GPS", "Lazzus et. al 2017", "Persistence", "Wu & Lundstedt 1997"), 
    alpha = 1.) +
  theme(legend.position="top", legend.direction = "horizontal") +
  scale_x_discrete(
    name = "Prediction Horizon",
    breaks=c("1","2","3","4","5","6"),
    labels=c(
      parse(text = TeX('$t+1$')), parse(text = TeX('$t+2$')), parse(text = TeX('$t+3$')), 
      parse(text = TeX('$t+4$')), parse(text = TeX('$t+5$')), parse(text = TeX('$t+6$')))) +
  ylab("CC")

ggsave(
  filename = "lstmCompCC.pdf", 
  scale = 1.0)

