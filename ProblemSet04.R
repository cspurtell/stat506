##### Problem 1. #####

library(tidyverse)
library(nzelect)
data("nzge")

### a. ###
nzge %>%
  group_by(election_year, voting_type) %>%
  summarize(total_votes = sum(votes, na.rm = TRUE)) %>%
  arrange(desc(total_votes))

### b. ###
nzge %>%
  filter(election_year == 2014, voting_type == "Candidate") %>%
  group_by(party) %>% 
  summarize(total_votes = sum(votes, na.rm = TRUE)) %>% 
  mutate(percent = 100 * total_votes / sum(total_votes)) %>% 
  arrange(desc(percent))

### c. ###
nzge %>% 
  group_by(election_year, voting_type, party) %>% 
  summarize(total_votes = sum(votes, na.rm = TRUE)) %>% 
  slice_max(total_votes, n = 1, with_ties = FALSE) %>% 
  pivot_wider(
    names_from = voting_type,
    values_from = party,
    names_prefix = "winner_"
  )


##### Problem 2. #####
atp = read.csv("data/atp_matches_2019.csv")

### a. ###
atp %>% 
  distinct(tourney_id, tourney_name) %>% 
  count()
# 128 tournaments took place in 2019

### b. ###
tourney_winners = atp %>% 
  filter(round == "F") %>% 
  group_by(tourney_id, tourney_name) %>% 
  summarize(winner = first(winner_name), .groups = "drop")





#Resources used: Tidyverse documentation for slice_max() and pivot_wider() functions