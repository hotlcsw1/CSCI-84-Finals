# Gender Breakdown
library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

# Working Directory->"C:/Users/Owner/Documents"
charts.data <- read.csv("~/R/Datasets/Georgia_prev_election_results/RaceGenderAge/clean/gender_breakdown.csv")
p10 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = gender), data =charts.data,stat="identity")
p10

# Adjust x-axis scale
p10 <- p10 + scale_x_continuous(breaks=seq(2000,2012,4))
p10