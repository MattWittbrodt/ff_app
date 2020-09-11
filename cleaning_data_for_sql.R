# Combined table into players for database
library(tidyverse)

# Reading in players- getting the unique ones -----------------------------
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/tmp_update_for_SQL.xlsx") %>%
     .[,c(2:4)]%>%
     unique() %>%
     arrange(proj_player) %>%
     .[-c(99, 131, 165, 276, 337, 346, 463),] %>%
     mutate(proj_player = str_split(proj_player, " "),
            first_name = NA,
            last_name = NA)

# Getting first and last names
for(ii in 1:nrow(d)) {
  d[ii,4] <- d[ii,1][[1]][[1]][1]
  d[ii,5] <- d[ii,1][[1]][[1]][2]
}

# Getting player_id (first four of first name, first three of last name)
d2 <- d %>%
     select(-proj_player) %>%
     mutate(player_id = paste0(str_to_lower(substr(first_name,1,3)),str_to_lower(last_name)),
            player_id = paste0(substr(player_id, 1,6), str_to_lower(proj_pos)))

print(paste0("There are ", length(unique(d2$player_id)), " unique player Id."))
print(paste0("There are as unique ID names as there are players = ", length(unique(d2$player_id)) == nrow(d2)))

# Preparing for insert ----------------------------------------------------
# Table Columns 
# CREATE TABLE players(
#   player_id VARCHAR(7) PRIMARY KEY,
#   first_name VARCHAR (50) NOT NULL,
#   last_name VARCHAR (50) NOT NULL,
#   team VARCHAR (3) NOT NULL,
#   position VARCHAR (2) NOT NULL

colnames(d2) <- c("position","team","first_name","last_name","player_id")

# Reorder columns
col_order <- c("player_id", "first_name", "last_name", "team", "position")
d2 <- d2[, col_order]

# Importing into SQL ------------------------------------------------------
dbWriteTable(con, "players", 
             value = d2, append = TRUE, row.names = FALSE)



