# Combined table into players for database



# Reading in players- getting the unique ones -----------------------------
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/tmp_update_for_SQL.xlsx") %>%
     .[,c(2:4)] %>%
     unique() %>%
     mutate(proj_player = str_split(proj_player, " "),
            first_name = NA,
            last_name = NA)


