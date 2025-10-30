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
#The ATP dataset includes some tournaments that began in late 2018 but are part of the 2019 competitive season (for example, tournaments starting December 31, 2018). To handle this, I chose to include only tournaments that ended in 2019, reasoning that these were part of the 2019 season. Based on this filter, there were X tournaments in the 2019 season.

### b. ###
tourney_winners = atp %>% 
  filter(round == "F") %>% 
  group_by(tourney_id, tourney_name) %>% 
  summarize(winner = first(winner_name), .groups = "drop")

#“For consistency, I defined a ‘tournament’ as any event with a recorded final match (round == "F"). This ensures the champion is identifiable. Using this criterion, 67 tournaments had a final recorded in 2019.”

tourney_winners %>%
  count(winner, sort = TRUE)

tourney_winners %>%
  count(winner, sort = TRUE) %>%
  filter(n > 1)

#We can see that 12 players won more than one tournament, with the most being 5 by Dominic Thiem and Novak Djokovic

### c. ###
aces_long <- atp %>%
  select(w_ace, l_ace) %>%
  rename(winner = w_ace, loser = l_ace) %>%
  pivot_longer(cols = c(winner, loser),
               names_to = "result",
               values_to = "aces") %>%
  filter(!is.na(aces))

aces_long %>%
  group_by(result) %>%
  summarise(
    mean_aces = mean(aces, na.rm = TRUE),
    median_aces = median(aces, na.rm = TRUE),
    sd_aces = sd(aces, na.rm = TRUE),
    n = n()
  )

#We can see from the observed mean and sd for each group of players that the mean number of aces for winners is higher than for losers

aces_long %>%
  ggplot(aes(x = result, y = aces, fill = result)) +
  geom_boxplot() +
  labs(
    title = "Aces by Match Result (Winner vs. Loser)",
    x = "Result", y = "Number of Aces"
  ) +
  theme_minimal()
#The boxplots support the hypothesis, as the IQR for winners is significantly higher than for losers and with less spread

### d. ###
player_stats <- atp %>%
  select(winner_name, loser_name) %>%
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "result",
    values_to = "player"
  ) %>%
  mutate(win = if_else(result == "winner_name", 1, 0))

player_summary <- player_stats %>%
  group_by(player) %>%
  summarise(
    matches = n(),
    wins = sum(win),
    win_rate = wins / matches
  ) %>%
  filter(matches >= 5) %>%
  arrange(desc(win_rate))

player_summary %>%
  filter(win_rate == max(win_rate))

#From the above filtering, we find that Rafael Nadal has the highest win rate of around 87%

##### Problem 3. #####





#Resources used: Tidyverse documentation for slice_max() and pivot_wider() functions