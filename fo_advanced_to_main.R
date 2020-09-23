library(tidyverse)
source("~/ff_shiny_app/ff_app/find_names.R", local = T)

# Reading in main DF
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_2_2020.xlsx") %>%
     mutate(proj_player_new = str_remove_all(proj_player, "(?<=[:upper:])[:alpha:]{1,}(?=[:space:])"))


# Football Outsiders
f <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/fo_advanced_stats.xlsx") %>%
     mutate(pass_player = ifelse(is.na(str_extract(pass_player, "^[:upper:]\\.[:upper:]\\.")) == T,
                                 pass_player,
                                 str_remove(pass_player, "\\.")),
            pass_player = str_replace(pass_player, "[:punct:]", " "),
            pass_player = str_remove_all(pass_player, "-"))
f[["pass_team"]] <- sapply(f[["pass_team"]], function(x) find_names(x, "fff_abbreviation"))

# Combining
combined_df <- left_join(d, f, by = c("proj_player_new" = "pass_player", "proj_tm" = "pass_team"))

rb2 <- filter(combined_df, is.na(pass_dyar) == T) %>% select(-starts_with("pass"))


# Name issues
proj player | new_proj_player
CeeDee Lamb	| CeeD Lamb
A.J. Green



