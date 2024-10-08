## Packages and Original Raw Datasets, as well as Cleaning/Joining

# loading packages
library(tidyverse)
library(readr)
library(ggplot2)
library(naniar)
library(knitr)
library(stringr)

# reading in .csv from first dataset
nfl_draft_player_info_and_outcomes <- read_csv("data/nfl_draft.csv") 

# reading in five .csvs from second dataset
nfl_draft_prospect_data <- read_csv("data/nfl_draft_prospects.csv")
nfl_draft_profiles <- read_csv("data/nfl_draft_profiles.csv")
college_stats <- read_csv("data/college_statistics.csv")
qb_data_to_2004 <- read_csv("data/college_qbr.csv")


## Establishing a foundation for cleaning: Making raw dataset timeline
min_year_outcomes <- nfl_draft_player_info_and_outcomes |>
  summarize(min_year_outcomes = min(year))

max_year_outcomes <- nfl_draft_player_info_and_outcomes |>
  summarize(max_year_outcomes = max(year))

min_year_qb_data <- qb_data_to_2004 |>
  summarize(min_year_qb_data = min(season))

max_year_qb_data <- qb_data_to_2004 |>
  summarize(max_year_qb_data = max(season))

min_year_college_stats <- college_stats |>
  summarize(min_year_college = min(season)) 

max_year_college_stats <- college_stats |>
  summarize(max_year_qb_college = max(season))

min_year_prospect_data <- nfl_draft_prospect_data |>
  summarize(min_year_prospect = min(draft_year)) 

max_year_prospect_data <- nfl_draft_prospect_data |>
  summarize(max_year_prospect = max(draft_year))

year_ranges <- c(min_year_outcomes, max_year_outcomes, min_year_qb_data,
                 max_year_qb_data, min_year_college_stats, max_year_college_stats,
                 min_year_prospect_data, max_year_prospect_data)

names <- c("Outcomes Start Year", "Outcomes End Year", 
           "QB Data Start Year", "QB Data End Year", 
           "College Stats Start Year", "College Stats End Year",
           "Prospect Data Start Year", "Prospect Data End Year")

kable(data.frame(year_ranges), col.names = names)


## More Cleaning
# removing irrelevant variables that stood out immediately, changing time ranges
nfl_draft_profiles_modified <- nfl_draft_profiles |>
  select(-player_image, -link, -school_logo, -text1, -text2, -text3, -text4, -guid)

nfl_draft_prospect_data_modified <- nfl_draft_prospect_data |>
  select(-team_logo_espn, -guid, -player_image) |>
  filter(draft_year > 1984)

# joining nfl_draft_profiles with nfl_draft_prospect_data_modified using player_id
nfl_draft_prospect_data_combined <- nfl_draft_prospect_data_modified |>
  left_join(nfl_draft_profiles_modified, join_by(player_id)) |>
  select(-contains(".y"))

nfl_draft_prospect_data_combined_1985_to_2021 <- nfl_draft_prospect_data_combined|>
  rename(player_name = player_name.x, 
         position = position.x, 
         pos_abbr = pos_abbr.x,
         school = school.x, 
         school_name = school_name.x, 
         school_abbr = school_abbr.x,
         weight = weight.x,
         height = height.x,
         pos_rk = pos_rk.x,
         ovr_rk = ovr_rk.x,
         grade = grade.x)

# making a key for prospect_data_combined to join with player info/outcomes dataset
nfl_draft_prospect_data_combined_1985_to_2021$join_id_outcomes <- 
  str_c(nfl_draft_prospect_data_combined_1985_to_2021$draft_year,                                         nfl_draft_prospect_data_combined_1985_to_2021$player_name)

# joining prospect_data_combined with outcomes using the above key ("join_id_outcomes")
# establishing shared time range: 1985 to 2015 for consistency
nfl_draft_prospect_data_combined_1985_to_2015 <- 
  nfl_draft_prospect_data_combined_1985_to_2021 |>
  filter(draft_year < 2016)

nfl_draft_prospect_data_outcomes_1985_to_2015 <- nfl_draft_player_info_and_outcomes |>
  left_join(nfl_draft_prospect_data_combined_1985_to_2015, 
            join_by(column_a == join_id_outcomes))

# some more cleaning of new dataset          
nfl_draft_prospect_data_outcomes_1985_to_2015 <- 
  nfl_draft_prospect_data_outcomes_1985_to_2015 |>
  select(-tm, -overall, -round, -draft_year, -contains(".y"),
         -player_name, -draft_year, -college_univ,
         -link, -alt_player_id) |>
  rename(join_id_outcomes = column_a,  
         pick = pick.x, 
         player_id = player_id.x)

# writing/saving new datasets
write_csv(nfl_draft_prospect_data_outcomes_1985_to_2015,
          "data/nfl_draft_prospect_data_and_outcomes_1985_to_2015.csv")

write_csv(nfl_draft_prospect_data_combined_1985_to_2021, 
          "data/nfl_draft_prospects_and_profiles_combined_1985_to_2021.csv")
