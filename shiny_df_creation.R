# DFS 2019 PreProcessing for Shiny App
library(tidyverse)

# Reading in team name chart
tm_names <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/team_names.xlsx")
source("~/ff_shiny_app/ff_app/find_names.R")

# Vegas Lines -------------------------------------------------------------
source("~/ff_shiny_app/ff_app/vegas_lines.R")
vegas <- vegas_lines()

vegas[["team"]] <- sapply(vegas[["team"]], function(x) find_names(x, "vegas"))


# DVOA Data ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/dvoa.R")
dvoa <- dvoa() 
dvoa_defense <- dvoa$defense
dvoa_offense <- dvoa$offense

dvoa_defense[["team"]] <- sapply(dvoa_defense[["team"]], function(x) find_names(x, "fff_abbreviation"))
dvoa_offense[["team"]] <- sapply(dvoa_offense[["team"]], function(x) find_names(x, "fff_abbreviation"))

# Positions ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/position_information.R")

# QB
qb <- position_stats("QB",2)

# RB
rb <- position_stats("RB",2)

# WR
wr <- position_stats("WR",2)

# TE
te <- position_stats("TE",2)


# Combinng into one DF ----------------------------------------------------

all_positions <- merge(qb,rb, all = TRUE) %>%
                 merge(wr, all = TRUE) %>%
                 merge(te, all = TRUE) %>%
                 merge(vegas, by.x = "tm", by.y = "team") %>%
                 merge(dvoa_defense, by.x = "opp", by.y = "team") %>%
                 merge(dvoa_offense, by.x = "tm", by.y = "team")

# Adding Dvoc
#writexl::write_xlsx(all_positions, "~/ff_shiny_app/all_data_wk_2.xlsx")
