# DFS 2019 PreProcessing for Shiny App
library(tidyverse)


# Vegas Lines -------------------------------------------------------------

source("~/ff_shiny_app/ff_app/vegas_lines.R")
vegas <- vegas_lines()


# Positions ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/position_information.R")

# QB
qb <- position_stats("QB",2)

# RB
rb <- position_stats("RB",2)

# WR
wr <- position_stats("RB",2)

# TE
te <- position_stats("TE",2)
