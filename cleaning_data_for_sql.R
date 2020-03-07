# Combined table into players for database

# Reading in players- getting the unique ones -----------------------------
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/tmp_update_for_SQL.xlsx") %>%
     .[,c(2:4)] %>%
     unique() %>%
     mutate(proj_player = str_split(proj_player, " "),
            first_name = NA,
            last_name = NA)

# Getting first and last names
for(ii in 1:nrow(d)) {
  d[ii,4] <- d[ii,1][[1]][[1]][1]
  d[ii,5] <- d[ii,1][[1]][[1]][2]
}

# Getting player_id (first four of first name, first three of last name)
d <- d %>%
     select(-proj_player) %>%
     mutate(player_id = paste0(str_to_lower(substr(first_name,1,4)),str_to_lower(substr(last_name,1,3))))

