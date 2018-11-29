library(TraMineR)
library(data.table)
library(tidyverse)
library(dplyr)
library(reshape2)

occ <- read_csv("~/Downloads/occmob/occmob.csv", col_names = T)

occ_seq <- function(x){
dt <- data.table(melt(occ[x,-c(1:5)]))
dt <- dt[, idx := .GRP, by = value]
return(dt$idx)
}

par(mfrow=c(2,1))

seqplot(seqdef(apply(X = matrix(c(7)), MARGIN = 1, occ_seq) %>% t), cpal = colors()[max(occ_seq(7))],with.legend = F)

seqplot(seqdef(apply(X = matrix(x), MARGIN = 1, occ_seq) %>% t, cpal = ), cpal = sample(x = colors(), size =max(occ_seq(x))),with.legend = F)

#####

occ_plot <- function(x){
  seqplot(seqdef(apply(X = matrix(c(x)), MARGIN = 1, occ_seq) %>% t), cpal = colors()[1:max(occ_seq(x))],with.legend = F,main = paste(occ_seq(x),sep = " ",collapse = "  "),cex.main = 2)
}

occ_plot(sample(x = c(1:nrow(occ)), size = 1))

