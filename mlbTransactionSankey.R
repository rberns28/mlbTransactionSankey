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

# Join salary information over
withSala <- tmp3 %>% left_join(salary,by=c("yearID","playerID"))

# Replacing null values in salary with zero
withSala$salary[is.na(withSala$salary)]<- 0


# Create a final slimmed down table
finalTrunc <- withSala %>% filter(yearID > 1985) %>% group_by(teamID.x,lastTeamFinal,yearID) %>%
                      summarise(players = n(),salary=sum(salary))

# ---------- Create Data for Sankey --------------- #

# Create an Index for Every Team
names <- as.data.frame(levels(as.factor(finalTrunc$lastTeamFinal)))
colnames(names) <-c("name")

namesTmp <- as.data.frame(cbind(names,as.numeric(rownames(names))))
colnames(namesTmp)<-c("name","idx")
namesTmp$name <- as.character(namesTmp$name)
namesTmp <- namesTmp %>% mutate(idx0 = (as.numeric(idx)-1)) %>% select(-idx)

finalTrunc$teamID.x <- as.character(finalTrunc$teamID.x)


# Join back the index to finalTrunc table

join1 <- finalTrunc %>% inner_join(namesTmp,by=c("teamID.x"="name")) %>% rename(target = idx0)
join2 <- join1 %>% inner_join(namesTmp,by=c("lastTeamFinal"="name")) %>% rename(source = idx0)

tmp3 <- join2 %>% ungroup() %>% filter(source != target) %>% select(-teamID.x, -lastTeamFinal)

allYrsPlayers <- tmp3 %>% group_by(source,target) %>%
                          summarise(players = as.numeric(sum(salary))) %>%
                          filter(source!=22)
        

nodesF <- namesTmp %>% mutate(nameNew = as.factor(name)) %>%
                      select(nameNew) %>% rename(name = nameNew)

sankeyNetwork(Links = as.data.frame(allYrsPlayers), Nodes = nodesF,
              Source = "source", Target = "target",
              Value = "players", NodeID = "name",
              fontSize= 8, nodeWidth = 50,fontFamily = "open sans")
