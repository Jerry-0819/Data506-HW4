---
title: "Problem 2 - Tidyverse: Tennis"
format: html
execute:
  echo: true
  message: false
  warning: false
---

## Setup

```{r}
library(tidyverse)

# Read the file
tennis <- read_csv(
  "atp_matches_2019.csv",
  show_col_types = FALSE
)

# Check column names, variable types, and example values.
glimpse(tennis)

# Check the tournament levels and match rounds.
tennis %>%
  count(tourney_level, round) %>%
  knitr::kable(
    caption = "Number of matches by tournament level and round"
  )
```

## 1. How many tournaments took place in 2019

```{r}
# Count distinct tournament IDs, rather than match rows.
tournament_count <- tennis %>%
  summarise(
    tournaments = n_distinct(tourney_id)
  )

tournament_count %>%
  knitr::kable(
    col.names = "Number of tournament IDs",
    caption = "Tournament count in the 2019 dataset"
  )

# Inspect the count by level to clarify how team events are recorded.
tennis %>%
  distinct(tourney_id, tourney_level) %>%
  count(tourney_level, name = "tournaments") %>%
  knitr::kable(
    col.names = c("Tournament level", "Number of tournament IDs")
  )
```

**Answer:** There are `r tournament_count$tournaments` distinct
tournament IDs in the dataset. So there are `r tournament_count$tournaments`
tournaments

## 2. Players won more than one tournament

```{r}
# Identify final winners and count titles for each player.
# Player IDs distinguish players even if their names are similar.
titles <- tennis %>%
  filter(round == "F") %>%
  distinct(tourney_id, winner_id, winner_name) %>%
  count(winner_id, winner_name, name = "titles") %>%
  arrange(desc(titles), winner_name)

# Keep players who won at least two tournaments.
multiple_winners <- titles %>%
  filter(titles > 1)

multiple_winners %>%
  select(winner_name, titles) %>%
  knitr::kable(
    col.names = c("Player", "Tournament titles"),
    caption = "Players who won more than one tournament"
  )

# Summarize the number of repeat champions and the maximum title count.
title_summary <- titles %>%
  summarise(
    repeat_champions = sum(titles > 1),
    most_titles = max(titles)
  )

# Keep all players tied for the most titles.
top_champions <- titles %>%
  slice_max(titles, n = 1, with_ties = TRUE)

top_champions %>%
  select(winner_name, titles) %>%
  knitr::kable(
    col.names = c("Player", "Tournament titles"),
    caption = "Players with the most tournament titles"
  )
```

**Answer:** Yes. `r title_summary$repeat_champions` players won
more than one tournament.
`r paste(top_champions$winner_name, collapse = " and ")` tied
for the most titles, with `r title_summary$most_titles` each.

## 3. Is there any evidence that winners have more aces than losers

Compare winners and losers within the same match. Here, `w_ace`
contains the winner's ace count and `l_ace` contains the loser's
ace count.

```{r}
# Exclude a match if either ace count is missing.
# This ensures both averages use exactly the same matches.
ace_pairs <- tennis %>%
  select(w_ace, l_ace) %>%
  drop_na(w_ace, l_ace) %>%
  mutate(
    # Positive values mean the winner served more aces.
    ace_difference = w_ace - l_ace
  )

# Summarize the paired comparison.
ace_summary <- ace_pairs %>%
  summarise(
    matches = n(),
    mean_winner_aces = mean(w_ace),
    mean_loser_aces = mean(l_ace),
    mean_difference = mean(ace_difference),
    median_difference = median(ace_difference),
    proportion_winner_more = mean(ace_difference > 0)
  )

ace_summary %>%
  knitr::kable(
    digits = 3,
    caption = "Paired comparison of winner and loser ace counts"
  )

# Count how often the winner had more, equal, or fewer aces.
ace_comparison <- ace_pairs %>%
  mutate(
    comparison = case_when(
      ace_difference > 0 ~ "Winner had more aces",
      ace_difference == 0 ~ "Equal aces",
      ace_difference < 0 ~ "Winner had fewer aces"
    )
  ) %>%
  count(comparison, name = "matches") %>%
  mutate(
    percent = scales::percent(matches / sum(matches), accuracy = 0.1)
  )

ace_comparison %>%
  knitr::kable(
    col.names = c("Comparison", "Matches", "Percent"),
    caption = "Within-match ace comparisons"
  )
```

**Answer:** There is descriptive evidence that winners tend to have
more aces. Among the `r ace_summary$matches` matches with both
ace counts available, winners averaged
`r round(ace_summary$mean_winner_aces, 2)` aces, compared with
`r round(ace_summary$mean_loser_aces, 2)` for losers.
The average paired difference was
`r round(ace_summary$mean_difference, 2)` aces in favor of winners.

Winners had more aces in
`r scales::percent(ace_summary$proportion_winner_more, accuracy = 0.1)`
of these matches. These results describe an association; they do
not establish that serving more aces causes a player to win.
No formal hypothesis test is used here.

## 4. Highest win-rate of players with at least five matches

Calculate each player's win-rate as:

$$
\text{Win-rate} =
\frac{\text{Number of wins}}
{\text{Number of wins} + \text{Number of losses}}.
$$

```{r}
# Create one record for each winner, assigning a win indicator of 1.
winner_records <- tennis %>%
  transmute(
    player_id = winner_id,
    player_name = winner_name,
    won = 1L
  )

# Create one record for each loser, assigning a win indicator of 0.
loser_records <- tennis %>%
  transmute(
    player_id = loser_id,
    player_name = loser_name,
    won = 0L
  )

# Combine both sides so every match contributes one win and one loss.
player_rates <- bind_rows(winner_records, loser_records) %>%
  group_by(player_id) %>%
  summarise(
    player_name = first(player_name),
    matches = n(),
    wins = sum(won),
    losses = sum(won == 0L),
    .groups = "drop"
  ) %>%
  # Apply the minimum-match requirement before selecting the leader.
  filter(matches >= 5) %>%
  mutate(
    win_rate = wins / matches
  ) %>%
  arrange(desc(win_rate), player_name)

# Retain every player tied for the highest win-rate.
best_players <- player_rates %>%
  slice_max(win_rate, n = 1, with_ties = TRUE)

best_players %>%
  mutate(
    win_rate = scales::percent(win_rate, accuracy = 0.01)
  ) %>%
  select(player_name, matches, wins, losses, win_rate) %>%
  knitr::kable(
    col.names = c("Player", "Matches", "Wins", "Losses", "Win-rate"),
    caption = "Highest win-rate among players with at least five matches"
  )
```

**Answer:** `r paste(best_players$player_name, collapse = ", ")`
had the highest win-rate among players with at least five recorded
matches, at
`r scales::percent(best_players$win_rate[[1]], accuracy = 0.01)`
The table shows the corresponding match, win, and loss counts.
This calculation uses all match rows in the supplied dataset.