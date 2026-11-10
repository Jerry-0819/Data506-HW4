---
title: "Problem 3 - Visualization"
format:
  html:
    embed-resources: true
execute:
  echo: true
  message: false
  warning: false
---

## Setup

```{r}
library(tidyverse)

covid_raw <- read_csv("us-states.csv", show_col_types = FALSE)

# Keep the 50 states, excluding DC and territories.
covid <- covid_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(state %in% state.name) %>%
  select(date, state, cases_avg, cases_avg_per_100k) %>%
  # Give every state the same observation period.
  # Missing early dates are treated as zero reported cases.
  complete(
    state,
    date = seq.Date(min(date), max(date), by = "day"),
    fill = list(cases_avg = 0, cases_avg_per_100k = 0)
  ) %>%
  arrange(state, date)

# Apply consistent formatting to all plots.
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(
        size = 9, color = "gray40", hjust = 0
      ),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
)
```

## 1. How many major and minor spikes were there?
I combined cases across the 50 States and smoothed the curve to identify distinct waves,
Using 150,000 daily cases as the threshold for a major spike, I identified three major and five minor spikes

```{r}
# Sum state averages to obtain the combined daily series.
national <- covid %>%
  group_by(date) %>%
  summarise(
    daily_cases = sum(cases_avg),
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  mutate(
    # Additional smoothing reduces short reporting fluctuations.
    smooth_cases = slider::slide_dbl(
      daily_cases,
      mean,
      .before = 10,
      .after = 10,
      .complete = TRUE
    ),

    # A 57-day window includes 28 days before and after each date.
    local_maximum = slider::slide_dbl(
      smooth_cases,
      max,
      .before = 28,
      .after = 28,
      .complete = TRUE
    )
  )

peaks <- national %>%
  filter(
    !is.na(local_maximum),
    smooth_cases == local_maximum,
    smooth_cases >= 20000
  ) %>%
  mutate(
    magnitude = if_else(
      smooth_cases >= 150000,
      "Major",
      "Minor"
    ),
    label = format(date, "%b\n%Y")
  )

peaks %>%
  transmute(
    Date = date,
    Classification = magnitude,
    `Smoothed daily cases` = round(smooth_cases)
  ) %>%
  knitr::kable(
    format.args = list(big.mark = ","),
    caption = "Identified peaks in the combined 50-state series"
  )
```

```{r}

ggplot(national, aes(date, smooth_cases)) +
  geom_line(linewidth = 0.8, color = "#334155", na.rm = TRUE) +
  geom_hline(
    yintercept = 150000,
    linetype = "dashed",
    color = "gray55"
  ) +
  geom_point(
    data = peaks,
    aes(color = magnitude),
    size = 3
  ) +
  geom_text(
    data = peaks,
    aes(label = label),
    vjust = -0.6,
    size = 3
  ) +
  scale_color_manual(
    values = c("Major" = "#C2410C", "Minor" = "#2563EB")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs(
    title = "Eight distinct waves in reported COVID-19 cases",
    subtitle = "Additional 21-day smoothing; dashed line marks the major-peak threshold",
    x = NULL,
    y = "Daily reported cases, smoothed",
    color = "Peak classification",
    caption = "Source: The New York Times. Includes the 50 states; excludes DC and territories."
  )
```

**Answer:** Under this definition, there were
`r sum(peaks$magnitude == "Major")` major spikes and
`r sum(peaks$magnitude == "Minor")` minor spikes.

The largest spike occurred in January 2022. The other major
spikes occurred in January 2021 and September 2021.
Smaller waves appear in April and July 2020, April 2021,
July 2022, and December 2022.

The exact count depends on smoothing and the chosen thresholds.
This rule also cannot identify peaks at the very beginning or
end of the series because it requires surrounding observations.

## 2. States with the highest and lowest overall rates

I define the overall rate as the average daily reported cases
per 100,000 residents over the common observation period.
This adjusts for population size and gives every state the
same time window.

```{r}
# Calculate an overall population-adjusted rate for each state.
state_rates <- covid %>%
  group_by(state) %>%
  summarise(
    overall_rate = mean(cases_avg_per_100k),
    .groups = "drop"
  )

# Select the highest and lowest states.
extreme_states <- bind_rows(
  state_rates %>%
    slice_max(overall_rate, n = 1, with_ties = FALSE) %>%
    mutate(category = "Highest overall rate"),

  state_rates %>%
    slice_min(overall_rate, n = 1, with_ties = FALSE) %>%
    mutate(category = "Lowest overall rate")
)

extreme_states %>%
  select(category, state, overall_rate) %>%
  knitr::kable(
    digits = 2,
    col.names = c(
      "Category", "State", "Mean daily cases per 100,000"
    ),
    caption = "States with the highest and lowest overall reported rates"
  )

comparison <- covid %>%
  inner_join(extreme_states, by = "state")

# Display each state's largest seven-day average daily rate.
state_peaks <- comparison %>%
  group_by(state) %>%
  slice_max(cases_avg_per_100k, n = 1, with_ties = FALSE) %>%
  ungroup()

state_peaks %>%
  select(state, date, cases_avg_per_100k) %>%
  knitr::kable(
    digits = 2,
    col.names = c("State", "Peak date", "Peak daily cases per 100,000"),
    caption = "Largest reported daily rates in the selected states"
  )
```

```{r}
ggplot(
  comparison,
  aes(date, cases_avg_per_100k, color = state)
) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title = "Rhode Island and Maine followed different case trajectories",
    subtitle = "States selected by their mean daily reported cases per 100,000",
    x = NULL,
    y = "Daily cases per 100,000 residents\n(seven-day average)",
    color = NULL,
    caption = "Source: The New York Times. Both states use the same dates and vertical scale."
  )
```

**Answer:** Rhode Island had the highest overall rate, while
Maine had the lowest under the stated definition.

Rhode Island experienced a more pronounced early wave and much
larger peaks during several later waves, especially around
January 2022. Maine's early trajectory was comparatively low,
although it also experienced substantial later increases.

The shared vertical scale makes the difference in magnitude
visible. However, the ranking describes reported cases:
differences in testing, reporting, and unrecorded infections
may also contribute to the observed trajectories.

## 3. First five states to experience substantial COVID-19 activity

I define substantial activity as at least one reported daily
case per 100,000 residents, measured by the seven-day average,
for seven consecutive days.

```{r}
# Identify sustained threshold crossings.
onsets <- covid %>%
  group_by(state) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    above_threshold = cases_avg_per_100k >= 1,

    # Count qualifying days in the current seven-day window.
    qualifying_days = slider::slide_int(
      above_threshold,
      ~ as.integer(sum(.x)),
      .before = 6,
      .complete = TRUE
    )
  ) %>%
  filter(qualifying_days == 7L) %>%
  summarise(
    # Subtract six days to recover the start of the first run.
    onset_date = min(date) - 6,
    .groups = "drop"
  ) %>%
  arrange(onset_date, state)

# Keep ties at the fifth position rather than breaking them arbitrarily.
earliest_states <- onsets %>%
  slice_min(onset_date, n = 5, with_ties = TRUE)

earliest_states %>%
  knitr::kable(
    col.names = c("State", "Onset of sustained activity"),
    caption = "Earliest substantial activity, retaining ties for fifth"
  )

early_trajectories <- covid %>%
  semi_join(earliest_states, by = "state") %>%
  filter(
    date >= as.Date("2020-02-15"),
    date <= as.Date("2020-04-15")
  )
```

```{r}

ggplot(
  early_trajectories,
  aes(date, cases_avg_per_100k)
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_line(color = "#0072B2", linewidth = 0.8) +
  geom_vline(
    data = earliest_states,
    aes(xintercept = onset_date),
    linetype = "dotted",
    color = "#C2410C"
  ) +
  facet_wrap(vars(state), ncol = 3) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %d"
  ) +
  # A log(1 + y) scale retains zeros while revealing early increases.
  scale_y_continuous(
    transform = "log1p",
    breaks = c(0, 1, 5, 10, 25, 50, 100)
  ) +
  labs(
    title = "Early sustained outbreaks appeared in several states",
    subtitle = "Horizontal line: 1 case per 100,000; vertical lines: qualifying onset dates",
    x = NULL,
    y = "Daily cases per 100,000 residents\n(seven-day average; log1p scale)",
    caption = "Source: The New York Times. Onset requires seven consecutive days above the threshold."
  )
```

**Answer:** Washington reached the sustained threshold first,
on March 15, 2020, followed by New York on March 18.
Louisiana and New Jersey followed on March 19.

Michigan and Vermont tied for fifth on March 21, so the
plot includes six states rather than arbitrarily excluding one.

These are the earliest states under the chosen definition of
substantial reported activity. They are not necessarily the
first states where infections occurred. Different thresholds
or differences in early testing could change the ordering.