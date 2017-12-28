#--------- MLB Player Movement Sankey --------#
# Author: Ryan Berns

setwd("~/mlbTransactions/core")
library(ggplot2)
library(scales)
library(dplyr)

# Load Datasets

appear <- read.csv("Appearances.csv",header=T,sep=",")
salary <- read.csv("Salaries.csv",header=T,sep=",")
player <- read.csv("Master.csv",header=T,sep=",")

# Only look at players 1985 and after
appearS <- appear %>% filter(yearID >= 1985)
rm(appear)

tmp <- appearS %>% group_by(playerID,yearID) %>% summarise(n=n()) %>% filter(n > 1)

tmp <- appearS %>% mutate(lastTeam = lag(order_by = playerID,teamID))
tmp2 <- tmp %>% select(yearID,teamID,lastTeam,playerID)
# Identify each player's first year to then default their last team for the first year to NA
firstYears <- tmp2 %>% group_by(playerID) %>% summarise(firstYear = min(yearID))

tmp3 <- tmp2 %>% left_join(firstYears,by=c("playerID")) %>%
  mutate(lastTeamFinal = case_when(
    as.character(firstYear)==as.character(yearID) ~ "N/A",
    as.character(firstYear)!=as.character(yearID) ~ as.character(lastTeam)))

withSala <- tmp3 %>% left_join(salary,by=c("yearID","playerID"))
