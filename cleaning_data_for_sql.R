# Combined table into players for database

d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/tmp_update_for_SQL.xlsx") %>%
     .[,c(2:4)]


## Getting unique players
