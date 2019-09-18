# DFS 2019 PreProcessing for Shiny App

shiny_df <- function(wk_num) {
  
library(tidyverse)

# Reading in team name chart ----------------------------------------------
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


# Last Week Data ----------------------------------------------------------

wk_data <- lapply(list("QB", "RB", "WR", "TE"), function(position) {

           wk_data <-  paste("https://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=2019&year_max=2019&season_start=1&season_end=-1&pos%5B%5D=",
                  position,
                  "&is_starter=E&game_type=R&career_game_num_min=0&career_game_num_max=499&qb_start_num_min=1&qb_start_num_max=400&game_num_min=0&game_num_max=99&week_num_min=",
                  as.character(wk_num - 1),
                  "&week_num_max=",
                  as.character(wk_num -1),
                  "&is_starter=E&game_type=R&career_game_num_min=0&career_game_num_max=499&qb_start_num_min=1&qb_start_num_max=400&game_num_min=0&game_num_max=99&week_num_min=1&week_num_max=1&c1stat=rush_att&c1comp=gt&c2stat=fanduel_points&c2comp=gt&c3stat=rush_att&c3comp=gt&c4stat=targets&c4comp=gt&c5val=1.0&order_by=fanduel_points",
                  #"&c1stat=rush_att&c1comp=gt&c1val=0&c2stat=fanduel_points&c2comp=gt&c3stat=rush_att&c3comp=gt&c4stat=targets&c4comp=gt&c4val=0&c5val=1.0&order_by=pass_rating",
                  sep = "") %>%                
            read_html() %>%
            html_table(fill = T) %>%
            .[[1]]
})

rz_types <- list("passing","rushing","receiving")

rz_df <- lapply(rz_types,
                function(x) {
                  
                  # Reading in Data
                  d <- paste("http://www.pro-football-reference.com/years/2019/redzone-",
                             x,
                             ".htm",
                             sep = "") %>%
                    read_html() %>%
                    html_table(fill = T) %>%
                    .[[1]]
                  
                  # Ammending names
                  rz_names <- paste(colnames(d), d[1,]) %>%
                    str_to_lower() %>%
                    str_remove_all("inside") %>%
                    str_remove_all("^\\s+") %>%
                    str_replace_all("20","twenty") %>%
                    str_replace_all("10","ten") %>%
                    str_replace_all("5","five") %>%
                    str_replace_all("(?<=\\w)%","_per") %>%
                    str_replace_all("%(?=\\w)","per_") %>%
                    str_replace_all("\\s+","_") %>%
                    str_replace_all("cmp","comp")
                  
                  
                  # Adding into new columns
                  colnames(d) <- paste(x,rz_names,sep = "_")
                  
                  # Final few cleaning steps
                  d <- d %>%
                    subset(.[[paste(x,"player",sep = "_")]] != "Player") %>%
                    select(-contains("link"))
                  
                })

# YTD data
ytd_types <- list("passing","rushing","receiving")

ytd_df <- lapply(ytd_types, function(x) {
  
  # Reading in Data
  d <- paste("http://www.pro-football-reference.com/years/2019/",
             x,
             ".htm",
             sep = "") %>%
    read_html() %>%
    html_table(fill = T) %>%
    .[[1]] %>%
    .[,-1]
  
  if(x == "passing" | x == "receiving") {
    d <- d  %>%
      subset(Pos != "Pos") %>%
      select(-Age, -GS, -Lng)
  } else {
    
    # Rush is divided into two rows
    colnames(d) <- d[1,]
    d <- d[-1,] %>%
      subset(Pos != "Pos" & as.numeric(Att) > 4) %>%
      select(-Age, -GS, -Lng)
    
  }
  
  # Ammending names
  d_names <- colnames(d) %>%
    str_to_lower() %>%
    str_replace_all("%","_per") %>%
    str_replace_all("cmp","comp") %>%
    str_replace("any/a", "avg_net_yds_per_att") %>%
    str_replace("ay/a", "avg_yds_per_att") %>%
    str_replace("ny/a", "net_yds_per_att") %>%
    str_replace("y/a", "yds_per_att") %>%
    str_replace("y/g", "yds_per_gm") %>%
    str_replace("y/c", "yds_per_completion") %>%
    str_replace("tgt", "target") %>%
    str_replace("r/g", "rec_per_gm") %>%
    str_replace("y/g", "yds_per_gm") %>%
    str_replace("y/r", "yds_per_rec") %>%
    str_replace("y/target", "yds_per_target")
  
  # Adding prefix to column names
  if(x == "rushing") {
    colnames(d) <- paste("ytd_rush", d_names, sep = "_")
  } else {
    if(x == "receiving") {
      colnames(d) <- paste("ytd_rec", d_names, sep = "_")
    } else {
      colnames(d) <- paste("ytd_pass", d_names, sep = "_")
    }
  }
  
  # Converting the catch % into numeric
  if(x == "receiving") {
    d[["ytd_rec_ctch_per"]] <- as.numeric(sapply(d[["ytd_rec_ctch_per"]],
                                                 function(x) str_remove(x, '%')))
  }
  
  # Making Columns numeric
  d[,c(4:length(d))] <- apply(d[,c(4:length(d))], 
                              2, 
                              function(x) as.numeric(as.character(x)))
  
  # Making a few columns per game
  
  if(x == "passing") {
    d <- mutate(d,
                ytd_pass_comp = ytd_pass_comp / ytd_pass_g,
                ytd_pass_att = ytd_pass_att / ytd_pass_g,
                ytd_pass_yds = ytd_pass_yds / ytd_pass_g,
                ytd_pass_td = ytd_pass_td / ytd_pass_g,
                ytd_pass_int = ytd_pass_int / ytd_pass_g)
  } else {
    if(x == "receiving") {
      d <- mutate(d,
                  ytd_rec_target = ytd_rec_target / ytd_rec_g,
                  ytd_rec_rec = ytd_rec_rec / ytd_rec_g,
                  ytd_rec_yds = ytd_rec_yds / ytd_rec_g,
                  ytd_rec_td = ytd_rec_td / ytd_rec_g)
    } else {
      d <- mutate(d,
                  ytd_rush_att = ytd_rush_att / ytd_rush_g,
                  ytd_rush_yds = ytd_rush_yds / ytd_rush_g,
                  ytd_rush_td = ytd_rush_td / ytd_rush_g)
    }}
  
})

data <- list(wk_data = wk_data,
             rz_df = rz_df,
             ytd_df = ytd_df)
# Positions ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/position_information.R")

# QB
qb <- position_stats("QB",wk_num,data)

# RB
rb <- position_stats("RB",wk_num,data)

# WR
wr <- position_stats("WR",wk_num,data)

# TE
te <- position_stats("TE",wk_num,data)


# Combinng into one DF ----------------------------------------------------

all_positions <- merge(qb,rb, all = TRUE) %>%
                 merge(wr, all = TRUE) %>%
                 merge(te, all = TRUE) %>%
                 left_join(vegas, by = c("proj_tm" = "team")) %>%
                 left_join(dvoa_defense, by = c("proj_opp" = "team")) %>%
                 left_join(dvoa_offense, by = c("proj_tm" = "team"))


# Adding previous week DVOA -----------------------------------------------

dvoa_previous <- dvoa_defense
names(dvoa_previous) <- paste("prev_wk_dvoa", names(dvoa_previous), sep = "_")

all_positions <- left_join(all_positions, dvoa_previous, by = c("prev_wk_opp" = "prev_wk_dvoa_team"))

# Adding Leverage Scores --------------------------------------------------
leverage <- read.csv(paste("~/ff_shiny_app/ff_app/data/4for4-fantasy-football-gpp-leverage-scores-table_wk",
                     wk_num,
                     ".csv", sep = "")) %>%
            select(-Opp) %>%
            mutate(Cash.Odds = as.numeric(gsub("\\%", "", Cash.Odds)),
                   GPP.Odds = as.numeric(gsub("\\%", "", GPP.Odds)),
                   Implied.Own. = as.numeric(gsub("\\%", "", Implied.Own.)),
                   Projected.Own. = as.numeric(gsub("\\%", "", Projected.Own.)),
                   Tm = as.character(Tm))

leverage_names <- colnames(leverage) %>% 
                  str_to_lower() %>% 
                  str_remove("[.]+$") %>%
                  str_replace("[.]","_")

colnames(leverage) <- leverage_names

# fixing team names and some select player names
leverage[["tm"]] <- sapply(leverage[["tm"]], function(x) find_names(x, "fff_abbreviation"))
leverage[["player"]] <- str_replace(leverage[["player"]], "DK", "D.K.")
leverage[["player"]] <- str_replace(leverage[["player"]], "Mitch", "Mitchell")


# Adding into full DF
all_positions <- left_join(all_positions, leverage, by = c("proj_player" = "player",
                                                           "proj_pos" = "pos",
                                                           "proj_tm" = "tm"))

#writexl::write_xlsx(all_positions, "~/ff_shiny_app/all_data_wk_2.xlsx")
#all_positions <- readxl::read_xlsx("~/ff_shiny_app/all_data_wk_2.xlsx")


}
