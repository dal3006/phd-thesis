#! /usr/bin/Rscript
library(plyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(latex2exp)
library(directlabels)
library(ggsci)

setwd("Manuscripts/phd-thesis/data")

pred_df <- read.csv("predictions.csv")
colnames(pred_df) <- c("time", "yvalue", "functype")

arp_samples <- read.csv("narp_samples.csv")
colnames(pred_df) <- c("time", "yvalue", "functype")

gp_exp_samples <- read.csv("gp_explicit_samples.csv")
colnames(pred_df) <- c("time", "yvalue", "functype")

gp_ar_samples <- read.csv("gp_ar_samples.csv")
colnames(pred_df) <- c("time", "yvalue", "functype")

#lower, MAP, truth, upper
palette1 <- c("forestgreen", "firebrick3", "#000000", "steelblue2")
lines1 <- c("dotdash", "solid", "solid", "dotdash")

palette2 <- c("firebrick3", "#000000")
lines2 <- c("solid", "solid")

limits_df <- data.frame(
  lower = pred_df[pred_df$functype == "lower",c("yvalue")],
  upper = pred_df[pred_df$functype == "upper",c("yvalue")],
  time = pred_df[pred_df$functype == "lower",c("time")]
)

adj_df <- pred_df[pred_df$functype %in% c("truth", "map"),]

adj_df$functype <- as.factor(as.character(adj_df$functype))

ggplot(adj_df, aes(x=time)) +
  geom_ribbon(
    data = limits_df,
    aes(ymin = lower, ymax = upper, x=time), 
    fill="blue", alpha="0.25") +
  geom_line(
    aes(y=yvalue, colour=functype, linetype="solid"), 
    size=0.8) +
  geom_point(size = 2, aes(y=yvalue, colour = functype)) +
  theme_gray(base_size = 2) + 
  scale_colour_manual(
    values=palette2, 
    name = "", 
    breaks = levels(adj_df$functype),
    labels=unname(TeX(c('$E \\left[\\hat{y}(t) | \\mathbf{x}_t\\right]$', '$y(t)$')))) + 
  scale_linetype_manual(values = lines2, guide=FALSE) +
  xlab(TeX('$t$')) + ylab(TeX('$y(t)$')) + 
  theme(legend.position="top", legend.direction = "horizontal")

ggsave("../figures/gp_nar_pred.pdf", scale = 1.0)

ggplot(adj_df[adj_df$time > 40, ], aes(x=time)) +
  geom_ribbon(
    data = limits_df,
    aes(ymin = lower, ymax = upper, x=time), 
    fill="blue", alpha="0.25") +
  geom_line(
    aes(y=yvalue, colour=functype, linetype="solid"), 
    size=0.8) +
  geom_point(size = 2, aes(y=yvalue, colour = functype)) +
  theme_gray(base_size = 22) + 
  scale_colour_manual(
   values=palette2, 
   name = "", 
   breaks = levels(adj_df$functype),
   labels=unname(TeX(c('$E \\left[\\hat{y}(t) | \\mathbf{x}_t\\right]$', '$y(t)$')))) + 
  scale_linetype_manual(values = lines2, guide=FALSE) +
  xlab(TeX('$t$')) + ylab(TeX('$y(t)$')) + 
  theme(legend.position="top", legend.direction = "horizontal")

ggsave("../figures/gp_nar_pred_partial.pdf", scale = 1.0)


gp_nar_prior <- read.csv("gp_ar_samples.csv")
colnames(gp_nar_prior) <- c("time", "yvalue", "path")
gp_nar_prior <- gp_nar_prior[gp_nar_prior$path %in% c('path_1', 'path_2', 'path_3', 'path4'),]
gp_nar_prior$path <- as.factor(as.character(gp_nar_prior$path))


ggplot(gp_nar_prior, aes(x=time,y=yvalue, colour=path)) +
  geom_line(aes(linetype="dash"), size=0.8) +
  geom_point(size = 2, aes(colour = path)) +
  theme_gray(base_size = 22) +
  theme(legend.position="top", legend.direction = "horizontal") +
  scale_color_startrek(
    name = "",
    labels=lapply(levels(gp_nar_prior$path), function (x){
      i <- sub('_', ' ', as.character(x))
      sub('p', 'P', i)
    })
  ) + 
  scale_linetype_manual(values = c("solid", "solid", "solid"), guide=FALSE) +
  xlab(TeX('$t$')) + ylab(TeX('$y(t)$'))

ggsave("../figures/gp_nar_prior.pdf", scale=1.0)

#Plot NAR samples


nar_prior <- read.csv("narp_samples.csv")
colnames(nar_prior) <- c("time", "yvalue", "path")
nar_prior <- nar_prior[nar_prior$path %in% c('path_1', 'path_2', 'path_3', 'path4'),]
nar_prior$path <- as.factor(as.character(nar_prior$path))


ggplot(nar_prior, aes(x=time,y=yvalue, colour=path)) +
  geom_line(aes(linetype="dash"), size=0.8) +
  geom_point(size = 2, aes(colour = path)) +
  theme_gray(base_size = 22) +
  theme(legend.position="top", legend.direction = "horizontal") +
  scale_color_manual(
    values = c("#000000", "#CC0C0099", "#5C88DA99"),
    name = "",
    labels=lapply(levels(gp_nar_prior$path), function (x){
      i <- sub('_', ' ', as.character(x))
      sub('p', 'P', i)
    })
  ) + 
  scale_linetype_manual(values = c("solid", "solid", "solid"), guide=FALSE) +
  xlab(TeX('$t$')) + ylab(TeX('$y(t)$'))

ggsave("../figures/nar_samples.pdf", scale=1.0)

#Plot GP-Explicit Samples

gp_explicit_prior <- read.csv("gp_explicit_samples.csv")
colnames(gp_explicit_prior) <- c("time", "yvalue", "path")
gp_explicit_prior <- gp_explicit_prior[gp_explicit_prior$path %in% c('path_1', 'path_2', 'path_3', 'path4'),]
gp_explicit_prior$path <- as.factor(as.character(gp_explicit_prior$path))


ggplot(gp_explicit_prior, aes(x=time,y=yvalue, colour=path)) +
  geom_line(aes(linetype="dash"), size=0.8) +
  geom_point(size = 2, aes(colour = path)) +
  theme_gray(base_size = 22) +
  theme(legend.position="top", legend.direction = "horizontal") +
  scale_color_startrek(
    name = "",
    labels=lapply(levels(gp_nar_prior$path), function (x){
      i <- sub('_', ' ', as.character(x))
      sub('p', 'P', i)
    })
  ) + 
  scale_linetype_manual(values = c("solid", "solid", "solid"), guide=FALSE) +
  xlab(TeX('$t$')) + ylab(TeX('$y(t)$'))

ggsave("../figures/gp_explicit_samples.pdf", scale=1.0)


