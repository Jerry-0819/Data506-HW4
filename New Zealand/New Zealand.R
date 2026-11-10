---
title: "Problem 1 - Tidyverse: New Zealand"
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
nzge <- readRDS("nzge.rds")

```

## 1. Total votes by year and vote type

Count all votes, including informal votes, separately for each year
and vote type.

```{r}
vote_totals <- nzge %>%
  # Create a separate group for each combination of year and vote type.
  group_by(election_year, voting_type) %>%

  # Add the vote counts across all parties and voting places.
  summarise(
    total_votes = sum(votes),
    .groups = "drop"
  ) %>%

  # Sort from the largest to the smallest
  arrange(desc(total_votes))

# Display the results as a formatted table.
vote_totals %>%
  knitr::kable(
    col.names = c("Year", "Vote type", "Total votes"),
    format.args = list(big.mark = ","),
    caption = "Total votes by year and vote type, including informal votes"
  )
```

## 2. Proportion of Candidate votes by party in 2014

Use valid Candidate votes as the denominator. Informal votes are
excluded because they are not votes for a party. Independent
candidates are retained as a category.

```{r}
candidate_2014 <- nzge %>%
  # Keep only valid Candidate votes from the 2014 election.
  filter(
    election_year == 2014,
    voting_type == "Candidate",
    party != "Informal Candidate Votes"
  ) %>%

  # Group the candidates belonging to the same party.
  group_by(party) %>%
  summarise(
    total_votes = sum(votes),
    .groups = "drop"
  ) %>%

  # Divide each party's total by the total valid candidate votes
  mutate(
    proportion = total_votes / sum(total_votes)
  ) %>%

  # Sort from the highest to the lowest
  arrange(desc(proportion))

candidate_2014 %>%
  # Add a percentage column
  mutate(
    percent = scales::percent(proportion, accuracy = 0.01)
  ) %>%
  knitr::kable(
    col.names = c("Party", "Candidate votes", "Proportion", "Percent"),
    digits = 4,
    format.args = list(big.mark = ","),
    caption = "Share of valid Candidate votes in 2014"
  )
```

## 3. Winning party by year and vote type

The winner is the party with the largest nationwide vote total
for each vote type, rather than the party winning the most seats.

```{r}
winners <- nzge %>%
  # Remove invalid votes
  filter(
    !party %in% c("Informal Candidate Votes", "Informal Party Votes")
  ) %>%

  # Calculate each party's total separately for every year and vote type
  group_by(election_year, voting_type, party) %>%
  summarise(
    total_votes = sum(votes),
    .groups = "drop"
  ) %>%

  # Compare parties within the same year and vote type.
  group_by(election_year, voting_type) %>%

  # Keep the party with the largest vote total.
  slice_max(
    order_by = total_votes,
    n = 1,
    with_ties = TRUE
  ) %>%

  # Produce one winner entry per year and vote type.
  summarise(
    winning_party = paste(party, collapse = ", "),
    .groups = "drop"
  ) %>%

  # Turn the two vote types into separate columns.
  pivot_wider(
    names_from = voting_type,
    values_from = winning_party
  ) %>%

  # Set the column order and sort years from earliest to latest.
  select(election_year, Candidate, Party) %>%
  arrange(election_year)

winners %>%
  knitr::kable(
    col.names = c("Year", "Candidate vote winner", "Party vote winner"),
    caption = "Winning parties by year and vote type"
  )
```