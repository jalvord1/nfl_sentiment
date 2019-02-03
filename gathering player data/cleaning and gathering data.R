library(dplyr)
library(rtweet)
library(rvest)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytext)

#Full Roster
url <-"xx"
full_roster <- read.csv(url)

full_roster <- full_roster %>%
  mutate(join = as.character(paste(FirstName, LastName, sep = " ")))


#top 100
url<- "https://en.wikipedia.org/wiki/NFL_Top_100_Players_of_2018"

top100 <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
  html_table()

top100 <- top100[[1]]

#bring in race df
race100 <- read.csv("/Volumes/easystore/Special Studies Fall 2018/nfl top 100 race.csv", stringsAsFactors = FALSE)

top100_full <- top100 %>%
  full_join(race100)

#FIXING NAMES FOR JOIN WITH FULL ROSTER

top100_full <- top100_full %>%
  mutate(join = ifelse(Player == "A. J. Green", "Adriel Green",
                ifelse(Player == "DeMarcus Lawrence", "Demarcus Lawrence",
                ifelse(Player == "A. J. Bouye", "Arlandus Bouye",
                ifelse(Player == "Mark Ingram Jr.", "Mark Ingram",
                ifelse(Player == "J. J. Watt", "Justin Watt",
                ifelse(Player == "Chris Harris Jr.", "Christopher Harris",
                ifelse(Player == "C. J. Mosley", "Clinton Mosley", 
                ifelse(Player == "Deshaun Watson", "Derrick Watson", 
                ifelse(Player == "Ha Ha Clinton-Dix", "Ha'Sean Clinton-Dix",
                ifelse(Player == "Geno Atkins", "Gene Atkins", 
                ifelse(Player == "Zach Ertz", "Zachary Ertz",
                ifelse(Player == "Zack Martin", "Zachary Martin", 
                ifelse(Player == "Cam Newton", "Cameron Newton",
                ifelse(Player == "Cameron Wake", "Derek Wake",
                ifelse(Player == "Case Keenum", "Casey Keenum",
                ifelse(Player == "Joey Bosa", "Joseph Bosa",
                ifelse(Player == "Julio Jones", "Quintorris Jones",
                ifelse(Player == "Kam Chancellor", "Kameron Chancellor",
                ifelse(Player == "Matt Ryan", "Matthew Ryan",
                ifelse(Player == "Matthew Stafford", "John Stafford",
                ifelse(Player == "Mike Daniels", "Michael Daniels",
                ifelse(Player == "Odell Beckham Jr.", "Odell Beckham",
                ifelse(Player == "Rob Gronkowski", "Robert Gronkowski", Player))))))))))))))))))))))))


#join with full roster?
full_w_top100 <- full_roster %>%
  full_join(top100_full, by = "join")

# write.csv(full_w_top100, "/Volumes/easystore/Special Studies Fall 2018/full roster with race.csv")


top100_more <- full_w_top100 %>%
  filter(!is.na(Race)) %>%
  filter(!(join == "Michael Thomas" & College %in% c("Stanford", "Southern Mississippi"))) %>%
  select(-Player, -Position) %>%
  rename(FullName = join)
  #there are more than 1 michael thomas

#how many players in each position/race
group <- top100_more %>%
  group_by(Race, PositionAbbr) %>%
  summarise(n = n()) 

#removing 
rm(group)


#column for twitter
top100_more <- top100_more %>%
  mutate(fortwitter = ifelse(Twitter == "", FullName, Twitter))
