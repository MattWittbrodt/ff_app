# Combined table into players for database
library(tidyverse)
library(mattDFS)

# Reading in players- getting the unique ones -----------------------------
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to10_combined.xlsx") %>%
     .[,c(2:4)] %>%
     unique() %>%
     arrange(proj_player) %>%
     mutate(proj_player = str_split(proj_player, " "),
            first_name = "",
            last_name = "",
            full_name = "")

# Getting first and last names
for(ii in 1:nrow(d)) {
  p <- unlist(d[ii,1])
  d[ii,4] <- as.character(p[1])
  d[ii,5] <- as.character(p[2])
  d[ii,6] <- paste0(as.character(p[1]), " ", as.character(p[2]))
}

# Getting player_id (first four of first name, first three of last name)
d2 <- d %>%
     select(-proj_player) %>%
     mutate(player_id = paste0(str_to_lower(substr(first_name,1,3)),str_to_lower(last_name)),
            player_id = paste0(substr(player_id, 1,6), str_to_lower(proj_pos)),
            year = 2020)

d2 <- d2[-c(283,319),] # removing Leveon bell for NYJ and Kalen Ballage for NYJ

print(paste0("There are ", length(unique(d2$player_id)), " unique player Id."))
print(paste0("There are as unique ID names as there are players = ", length(unique(d2$player_id)) == nrow(d2)))

# If everything is good, now making a player a number
d2$player_id <- c(1:nrow(d2))


# Preparing for insert ----------------------------------------------------
# Table Columns
# CREATE TABLE players(
#     player_id INT PRIMARY KEY,
#     first_name VARCHAR (50) NOT NULL,
#     last_name VARCHAR (50) NOT NULL,
#     full_name VARCHAR (50) NOT NULL,
#     team_id INT NOT NULL,
#     position_id INT(1) NOT NULL,
#     year NUMERIC (4) NOT NULL)

colnames(d2) <- c("position_id", "team", "first_name", "last_name", "full_name", "player_id", "year")

# Reorder columns
col_order <- c("player_id", "first_name", "last_name", "full_name", "team", "position_id","year")
d2 <- d2[, col_order]

# Getting position_id
d2 <- d2 %>% mutate(position_id = ifelse(position_id == "QB", 1, ifelse(position_id == "RB", 2, ifelse(position_id == "WR", 3, 4))))

# Load in mysql team csv (to prep for import)
team_sql <- read.csv("C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/team_mysql.csv") %>% select(pfr_abbreviation, team_id)

# Merge d2 with team_sql to get the correct team ID
d3 <- left_join(d2, team_sql, by = c("team" = "pfr_abbreviation")) %>% select(-team)

# Reorder to match
d3 <- d3[,c("player_id", "first_name", "last_name", "full_name", "team_id", "position_id","year")]

write.csv(d3,"C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/player_table_2020.csv", row.names = False)



# Importing into SQL ------------------------------------------------------
dbWriteTable(con, "players",
             value = d2, append = TRUE, row.names = FALSE)

# Reading in unique names from database
players <- dbReadTable(con, 'players') %>% select(-year)

####
#### Team Table
####

# CREATE TABLE teams(
#   team_id VARCHAR (3) PRIMARY KEY,
#   full_name VARCHAR (25) NOT NULL,
#   conference VARCHAR (3) NOT NULL,
#   division VARCHAR (10))

tms <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/team_names.xlsx") %>%
       select(pfr_abbreviation, full_name, conference, division)
colnames(tms) <- c("team_id", "full_name", "conference", "division")

# write table
dbWriteTable(con, "teams",
             value = tms, append = TRUE, row.names = FALSE)
#####
##### Vegas / Games Table ----
#####

# CREATE TABLE games(
#   team_id VARCHAR (3),
#   week NUMERIC NOT NULL,
#   year NUMERIC (4) NOT NULL,
#   opponent CHAR (3) NOT NULL,
#   home_team NUMERIC (1) NOT NULL,
#   line NUMERIC NOT NULL,
#   total NUMERIC NOT NULL,
#   favorite NUMERIC (1) NOT NULL,
#   implied_total NUMERIC NOT NULL,
#   PRIMARY KEY (team_id, week, year)
# )

data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to10_combined.xlsx") %>%
        select(proj_tm, proj_week, proj_opp, home_or_away, line, total, favorite, implied_total) %>%
        mutate(year = 2020)

colnames(data) <- c("team_id", "week", "opponent", "home_or_away", "line", "total", "favorite", "implied_total", "year")
data2 <- data[,c("team_id", "week", "year", "opponent", "home_or_away", "line", "total", "favorite", "implied_total")]

d <- read.csv("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/vegas_corrections_wk2_to_10.csv")

dbWriteTable(con, "games",
             value = d, append = TRUE, row.names = FALSE)

# Building projection table -----

# CREATE TABLE projections(
#   player_id INT NOT NULL,
#   week NUMERIC NOT NULL,
#   year NUMERIC (4) NOT NULL,
#   projected_ffpts NUMERIC NOT NULL,
#   projected_comp NUMERIC NOT NULL,
#   projected_pass_att NUMERIC NOT NULL,
#   projected_pass_yds NUMERIC NOT NULL,
#   projected_pass_td NUMERIC NOT NULL,
#   projected_int NUMERIC NOT NULL,
#   projected_rush_att NUMERIC NOT NULL,
#   projected_rush_yds NUMERIC NOT NULL,
#   projected_rush_td NUMERIC NOT NULL,
#   projected_rec NUMERIC NOT NULL,
#   projected_rec_yds NUMERIC NOT NULL,
#   projected_rec_td NUMERIC NOT NULL,
#   PRIMARY KEY (player_id, week, year)
# )

players <- dbReadTable(con, 'players') %>% select(-first_name, -last_name)

data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to10_combined.xlsx") %>%
        mutate(proj_player = str_remove(proj_player, "\\sJr$"))

proj <- select(data, starts_with("proj_"))

merged_projections <- right_join(players, proj, by = c("full_name" = "proj_player", "team" = "proj_tm", "position" = "proj_pos")) %>% 
      select(-full_name, -team, -position, -proj_opp, -proj_field, -proj_afpa, -proj_afpa_rk)

colnames(merged_projections) <- colnames(merged_projections) %>% str_replace("proj_week", "week") %>% str_replace("proj_", "projected_")
merged_projections2 <- merged_projections[,c("player_id","week","year",colnames(merged_projections[4:length(merged_projections)]))]
merged_projections2 <- filter(merged_projections2, is.na(player_id) == F)

dbWriteTable(con, "projections", value = merged_projections2, append = TRUE, row.names = FALSE)

# Rushing -----








