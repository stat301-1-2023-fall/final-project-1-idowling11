# Exploration 3: What Makes a Successful Draft Pick, Long-Term? 
# And Who are the Most Successful Ones?

# Creating AV over Expected
top_avoe_overall <- nfl_draft_prospect_data_outcomes_1985_to_2015 |> 
  filter(to < 2016 | year <= 2011) |>
  mutate(carav = ifelse(is.na(carav), 0, carav)) |>
  mutate(draft_cluster = cut(pick, breaks = 67.2, right = FALSE)) |>
  group_by(draft_cluster) |>
  mutate(median_cluster_av = median(carav)) |>
  mutate(av_over_expected = carav - median_cluster_av) 

top_25_avoe_overall <- top_avoe_overall |>
  arrange(desc(av_over_expected)) |>
  ungroup(draft_cluster) |>
  select(player, pos, rnd, pick, year, carav, median_cluster_av, av_over_expected) |>
  slice_head(n = 25)

names <- c("Name", "Position", "Round", "Overall Pick", "Draft Year", "Total AV",
           "Median 5-Pick Cluster AV", "AV Over Expected")

kable(data.frame(top_25_avoe_overall), col.names = names, 
      caption = "Top 25 Players with Highest Approximate Value Over Expected Since 1985")

# Approximate Value over Expected vs. Draft Pick
top_avoe_overall |>
  ggplot(aes(x = pick, y = av_over_expected, color = pos)) +
  geom_point(alpha = 0.45) +
  labs(title = "Approximate Value over Expected vs. Draft Pick Since 1985", 
       x = "Overall Pick", y = "AV Over Expected", color = "Position")

# AV Over Expected Distribution for Players that Shatter Expectations
top_avoe_overall |>
  filter(av_over_expected >= 50) |>
  mutate(factored_round = factor(pick)) |>
  ggplot(aes(x = av_over_expected)) +
  geom_histogram(color = "white") +
  labs(title = "Players with an Approximate Value Over Expected of 50+ by Draft Result", 
       x = "Overall Draft Pick")

# Top 25 "Diamonds in the Rough"
top_25_avoe_diamonds <- top_avoe_overall |>
  filter(pick > 64) |>
  arrange(desc(av_over_expected)) |>
  ungroup(draft_cluster) |>
  select(player, pos, rnd, pick, year, carav, median_cluster_av, av_over_expected) |>
  slice_head(n = 25)

names <- c("Name", "Position", "Round", "Overall Pick", "Draft Year", "Total AV",
           "Median 5-Pick Cluster AV", "AV Over Expected")

kable(data.frame(top_25_avoe_diamonds), col.names = names, 
      caption = "Top 25 Retired Players with Highest AV Over Expected Since 1985, 
                  Drafted Round 3 or Later")

# Position Breakdown for Diamonds in Rough
avoe_diamonds <- top_avoe_overall |>
  filter(pick > 64, av_over_expected >= 50)

diamond_breakdown <- avoe_diamonds |>
  group_by(pos) |>
  summarize(count = n()) |>
  arrange(desc(count))

names <- c("Position", "Number of Players with 50+ AV over Expected")

kable(data.frame(diamond_breakdown), col.names = names, 
      caption = "Players with 50+ AV over Expected Drafted Round 3 or Later: 
                Position Breakdown")

# Teams' Success at Drafting Diamonds in the Rough
team_diamond_distribution <- avoe_diamonds |>
  group_by(team_abbr) |>
  ggplot(aes(x = fct_infreq(team_abbr), fill = team_abbr)) +
  geom_bar(show.legend = FALSE, width = 1) +
  labs(title = "Number of Late-Round Players with 50+ AVOE by Team", 
       x = "Team Abbreviation", y = "Number of 50+ AV Over Expected Players",
       subtitle = "The Green Bay Packers (GNB) and Pittsburgh Steelers (PIT) stand out.", 
       caption = "Every outer ring represents a 5-player threshold.")

pie <- team_diamond_distribution + coord_polar("x", start=0)
pie


# Establishing Cumulative Net-AV Over Expected: AFC teams
top_avoe_overall |>
  filter(team_abbr != "PHO", team_abbr != "RAI", team_abbr != "RAM") |>
  filter(team_abbr == "BAL" | team_abbr == "CLE" | team_abbr == "PIT" | 
           team_abbr == "CIN" | team_abbr == "NYJ"| team_abbr == "NWE" | 
           team_abbr == "BUF"| team_abbr == "MIA" | team_abbr == "HOU"| 
           team_abbr == "TEN" |team_abbr == "JAX" | team_abbr == "IND"|
           team_abbr == "KAN" | team_abbr == "SDG" | team_abbr == "OAK" | 
           team_abbr == "DEN") |>
  group_by(team_abbr) |>
  arrange(year) |>
  mutate(cum_team_avoe = cumsum(av_over_expected)) |>
  ggplot(aes(x = year, y = cum_team_avoe, color = team_abbr)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_label_repel(aes(label = team_abbr), fontface = "bold", size = 3, 
                   nudge_y = 2, max.overlaps = 15, stat = "unique") +
  coord_cartesian(xlim = c(1985, 2015)) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  geom_smooth(method = "lm", color = "red", size = 2) +
  labs(title = "Cumulative AV Over Expected: AFC Teams from 1985-2015",
       x = "Year",
       y = "Cumulative AV Over Expected", 
       subtitle = "The bold red line is the line of best fit.",
       color = "Team Abbreviation")

# Establishing Cumulative Net-AV Over Expected: NFC teams
top_avoe_overall |>
  filter(team_abbr != "PHO", team_abbr != "RAI", team_abbr != "RAM") |>
  filter(team_abbr == "NYG" | team_abbr == "DAL" | team_abbr == "PHI" | 
           team_abbr == "WAS" | team_abbr == "GNB"| team_abbr == "CHI" | 
           team_abbr == "MIN"| team_abbr == "DET" | team_abbr == "NOR"| 
           team_abbr == "ATL" |team_abbr == "CAR" | team_abbr == "TAM"|
           team_abbr == "SFO" | team_abbr == "SEA" | team_abbr == "ARI" | 
           team_abbr == "STL")|>
  group_by(team_abbr) |>
  arrange(year) |>
  mutate(cum_team_avoe = cumsum(av_over_expected)) |>
  ggplot(aes(x = year, y = cum_team_avoe, color = team_abbr)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_label_repel(aes(label = team_abbr), fontface = "bold", size = 3, 
                   nudge_y = 3, stat = "unique", max.overlaps = 20) +
  coord_cartesian(xlim = c(1985, 2015)) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  geom_smooth(method = "lm", color = "red", size = 2) +
  labs(title = "Cumulative AV Over Expected: NFC Teams from 1985-2015",
       x = "Year",
       y = "Cumulative AV Over Expected", 
       subtitle = "The bold red line is the line of best fit.",
       color = "Team Abbreviation")
