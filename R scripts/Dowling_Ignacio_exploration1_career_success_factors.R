## Exploration 1: What are Driving Factors Behind Career Success, and What Does Draft Spot Have To Do With That? 

# Average Value Distribution compared to All-Pro First Team Selections 
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  mutate(ap1_factored = factor(ap1)) |>
  ggplot(aes(x = ap1_factored, y = carav)) +
  geom_boxplot(fill = "sky blue") +
  labs(title = "Career Approximate Value by Number of All-Pro First Team Selections", 
       x = "All-Pro First Team Selections", y = "Career Approximate Value (AV)")

# How about Pro Bowls?
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  mutate(pb_factored = factor(pb)) |>
  ggplot(aes(x = pb_factored, y = carav)) +
  geom_boxplot(fill = "sky blue") +
  labs(title = "Career Approximate Value by Number of Pro Bowl Selections", 
       x = "Pro Bowl Selections", y = "Career Approximate Value (AV)")

# AV Distribution: Everyone. Could be biased because of non-retired players whose careers aren't finished.
ggplot(nfl_draft_prospect_data_outcomes_1985_to_2015, aes(x = carav)) +
  geom_histogram(color = "white")


# AV Distribution: Retired Players
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  ggplot(aes(x = carav)) +
  geom_histogram(color = "white", binwidth = 5) +
  labs(title = "Career AV Distribution for Retired Players: 1985-2015",
       x = "AV", y = "Count")

# AV Distribution: Retired First-Rounders
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  filter(rnd == 1) |>
  ggplot(aes(x = carav)) +
  geom_histogram(color = "white", binwidth = 5) +
  labs(title = "Career AV Distribution for Retired First-Rounders: 1985-2015",
       x = "AV", y = "Count")

# AV Distribution: Retired Second-Rounders
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  filter(rnd == 2) |>
  ggplot(aes(x = carav)) +
  geom_histogram(color = "white", binwidth = 5) +
  labs(title = "Career AV Distribution for Retired Second-Rounders: 1985-2015",
       x = "AV", y = "Count")

# AV Distribution: Retired Third-Rounders
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  filter(rnd == 3) |>
  ggplot(aes(x = carav)) +
  geom_histogram(color = "white", binwidth = 5) +
  labs(title = "Career AV Distribution for Retired Third-Rounders: 1985-2015",
       x = "AV", y = "Count")

# AV Median by Round: Table format
av_by_round <- nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  group_by(rnd) |>
  summarize(median_av = median(carav))

names = c("Round", "Median AV")
kable(data.frame(av_by_round), col.names = names)

# Round 1
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  filter(pick <= 32) |>
  mutate(pick_factored = factor(pick)) |>
  ggplot(aes(x = pick_factored, y = carav)) +
  geom_boxplot(fill = "gold") +
  labs(title = "Distribution of Career AV by Pick Number, Retired First-Rounders: 1985-2015",  x = "Overall Pick Number", y = "Career Approximate Value (AV)")

# Round 2
nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  filter(to < 2016) |>
  filter(pick <= 64, pick > 32) |>
  mutate(pick_factored = factor(pick)) |>
  ggplot(aes(x = pick_factored, y = carav)) +
  geom_boxplot(fill = "gray") +
  labs(title = "Distribution of Career AV by Pick Number, Retired Second-Rounders: 1985-2015",  x = "Overall Pick Number", y = "Career Approximate Value (AV)")

