# Exploration 2: With First-Round Value Looking High, what Factors Potentially Indicate Getting Picked Highly in the 21st century for Different Positions?

# Examining EPA for QB prospects
qb_data_to_2004 |>
  ggplot(aes(x = total_epa)) +
  geom_histogram(color = "white", binwidth = 10) +
  labs(title = "Distribution of College Estimated Points Added: Quarterback Prospects from 2004-2021", x = "Estimated Points Added (EPA)")

top_qb_epas <- qb_data_to_2004 |>
  filter(total_epa >= 100) |>
  arrange(desc(total_epa)) |>
  slice_head(n = 25) |>
  select(player_name, season, total_epa)

names = c("Name", "NCAA Season", "EPA")  
kable(data.frame(top_qb_epas), col.names = names, caption = "Top 25 College Quarterback Seasons by Estimated Points Added Since 2004")

# Examining QBR for QB prospects
qb_data_to_2004 |>
  ggplot(aes(x = total_qbr)) +
  geom_histogram(color = "white", binwidth = 5) +
  labs(title = "Distribution of College Quarterback Rating: Prospects from 2004-2021", x = "Quarterback Rating (QBR)")

top_qbrs <- qb_data_to_2004 |>
  arrange(desc(total_qbr)) |>
  slice_head(n = 25) |>
  select(player_name, season, total_qbr)

names = c("Name", "NCAA Season", "QBR")  
kable(data.frame(top_qbrs), col.names = names, caption = "Top 25 College Quarterback Ratings Since 2004")

# Top 20 QBR and EPA since 2004
top_college_qbs <- qb_data_to_2004 |>
  filter(total_qbr >= 88.5, total_epa >= 102.2) |>
  arrange(desc(total_epa)) |>
  select(player_name, season, total_qbr, total_epa)

names = c("Name", "NCAA Season", "QBR", "EPA")  
kable(data.frame(top_college_qbs), col.names = names, caption = "Top College Quarterbacks by EPA and QBR since 2004")

# Elite QBs in Draft
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(player_name == "Mac Jones" |
           player_name == "Joe Burrow" | 
           player_name == "Kyler Murray" |
           player_name == "Jalen Hurts" | 
           player_name == "Sam Bradford" |
           player_name == "Cam Newton" | 
           player_name == "Baker Mayfield" | 
           player_name == "Marcus Mariota" | 
           player_name == "Johnny Manziel") |>
  ggplot(aes(x = overall, y = grade, color = player_name)) +
  geom_point(size = 4) +
  labs(title = "QBs in Top 25 of College QBR and EPA Since 2004: Draft Pick vs. ESPN Grade", subtitle = "Note that the highest-ranked, highest-picked QBs are near the top left of the graph.", x = "Overall Pick", y = "ESPN Draft Grade", color = "Player Name")

# Draft Picks and Grades for QBs with Top College EPAs
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(player_name == "Colt Brennan" |
           player_name == "Case Keenum" |
           player_name == "Patrick Mahomes" | 
           player_name == "McKenzie Milton" |
           player_name == "Colin Kaepernick" |
           player_name == "Chase Clement" |
           player_name == "Quinton Flowers" |
           player_name == "Cody Fajardo" |
           player_name == "Derek Carr" |
           player_name == "Deshaun Watson" |
           player_name == "Anthony Gordon" |
           player_name == "Mason Rudolph" |
           player_name == "Taylor Heinicke" |
           player_name == "Colt McCoy" |
           player_name == "Brandon Doughty" |
           player_name == "Graham Harrell") |>
  filter(overall > 0, overall < 1000) |>
  ggplot(aes(x = overall, y = grade, color = player_name)) +
  geom_point(size = 4) +
  labs(title = "QBs with Top 25 College EPA Since 2004: Draft Pick vs. ESPN Grade", subtitle = "This does not include the nine players also ranked in Top 25 for College QBR since 2004.", x = "Overall Pick", y = "ESPN Draft Grade", color = "Player Name", caption = "All other players in the top 25 not listed here or in subtitle were not drafted.")

# Draft Picks and Grades for QBs with Top College QBRs
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(player_name == "Tua Tagovailoa" |
           player_name == "Russell Wilson" |
           player_name == "Andrew Luck" | 
           player_name == "Justin Fields" |
           player_name == "Khalil Tate" |
           player_name == "Pat White" |
           player_name == "Jameis Winston" |
           player_name == "Seth Russell" |
           player_name == "Matt Corral" |
           player_name == "Kellen Moore" |
           player_name == "Anthony Gordon" |
           player_name == "Mason Rudolph" |
           player_name == "Zach Wilson" |
           player_name == "Kyle Trask") |>
  filter(overall > 0, overall < 1000) |>
  ggplot(aes(x = overall, y = grade, color = player_name)) +
  geom_point(size = 4) +
  labs(title = "QBs with Top 25 College EPA Seasons Since 2004: Draft Pick vs. ESPN Grade", subtitle = "This does not include the nine players also ranked in Top 25 for College QBR since 2004.", x = "Overall Pick", y = "ESPN Draft Grade", color = "Player Name", caption = "All other players in the top 25 not listed here or in subtitle were not drafted.")

# Effect of Weight and Height on linemen
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(pos_abbr == "OT" | pos_abbr == "OG" | pos_abbr == "DT" | pos_abbr == "DE") |>
  filter(height > 50) |>
  ggplot(aes(x = overall, y = weight, color = height)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~pos_abbr, labeller = labeller(pos_abbr =
                                              c("DE" = "Defensive Ends", "DT" = "Defensive Tackles",
                                                "OT" = "Offensive Tackles", "OG" = "Offensive Guards"))) +
  labs(title = "Weight and Height vs. Draft Pick: NFL-Drafted Linemen", x = "Overall Draft Pick", y = "Weight (lbs)", color = "Height (inches)")

# Using Factors to Establish Eras
nfl_prospects_by_era <- nfl_draft_prospect_data_combined_1985_to_2021 |>
  mutate(factored_draft_year = factor(draft_year)) |>
  mutate(
    draft_era = fct_collapse(factored_draft_year,
                             "1980s" = c("1985", "1986", "1987", "1989"),
                             "1990s" = c("1990", "1991", "1992", "1993", "1994", "1995", "1997", "1998",
                                         "1999"),
                             "2000s" = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                                         "2009"),
                             "2010s" = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018",
                                         "2019"),
                             "2020s" = c("2020", "2021"))) 

# Offensive Tackles
nfl_prospects_by_era |>
  filter(pos_abbr == "OT") |>
  filter(height > 50) |>
  ggplot(aes(x = overall, y = weight, color = height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~draft_era) +
  labs(title = "Weight and Height vs. Draft Pick: NFL-Drafted Offensive Tackles", x = "Overall Draft Pick", y = "Weight (lbs)", color = "Height (inches)")

# Offensive Guards
nfl_prospects_by_era |>
  filter(pos_abbr == "OG") |>
  filter(height > 50) |>
  ggplot(aes(x = overall, y = weight, color = height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~draft_era) +
  labs(title = "Weight and Height vs. Draft Pick: NFL-Drafted Offensive Guards", x = "Overall Draft Pick", y = "Weight (lbs)", color = "Height (inches)")

# Defensive Ends
nfl_prospects_by_era |>
  filter(pos_abbr == "DE") |>
  filter(height > 50) |>
  ggplot(aes(x = overall, y = weight, color = height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~draft_era) +
  labs(title = "Weight and Height vs. Draft Pick: NFL-Drafted Defensive Ends", x = "Overall Draft Pick", y = "Weight (lbs)", color = "Height (inches)")

# Defensive Tackles
nfl_prospects_by_era |>
  filter(pos_abbr == "DT") |>
  filter(height > 50) |>
  ggplot(aes(x = overall, y = weight, color = height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~draft_era) +
  labs(title = "Weight and Height vs. Draft Pick: NFL-Drafted Defensive Tackles", x = "Overall Draft Pick", y = "Weight (lbs)", color = "Height (inches)")

# QB Heights: Univariate
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(pos_abbr == "QB") |>
  ggplot(aes(x = height)) +
  geom_histogram(color = "white", binwidth = 1) +
  labs(title = "Distribution of Height: QB Prospects Since 2004", x = "Height (inches)")

# QBs: Draft Spot vs. Height
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(pos_abbr == "QB") |>
  ggplot(aes(x = overall, y = height)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm") +
  labs(title = "Relationship Between Height and Draft Spot: QB Prospects Since 2004", x = "Overall Draft Pick", y = "Height (inches)")

# Distribution of QB Prospect Heights Since 2004, by Year
nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(pos_abbr == "QB", draft_year > 2004) |>
  mutate(factored_year = factor(draft_year)) |>
  ggplot(aes(x = factored_year, y = height)) +
  geom_boxplot(fill = "green") +
  labs(title = "Distribution of Quarterback Prospect Heights Since 2004, by Year", x = "Draft Year", y = "Height (inches)")

# Draft Spot
nfl_prospects_by_era |>
  filter(pos_abbr == "QB") |>
  filter(draft_year >= 2000) |>
  ggplot(aes(x = overall, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~draft_era) +
  labs(title = "Height vs. Draft Pick: NFL-Drafted Quarterbacks", x = "Overall Draft Pick", y = "Height (inches)")

# ESPN Grade
nfl_prospects_by_era |>
  filter(pos_abbr == "QB") |>
  filter(draft_year >= 2000) |>
  ggplot(aes(x = grade, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~draft_era) +
  labs(title = "Height vs. ESPN Grade: NFL-Drafted Quarterbacks", x = "ESPN Grade", y = "Height (inches)", subtitle = "A higher ESPN Grade means the quarterback is more highly-touted.")

