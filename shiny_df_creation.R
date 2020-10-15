# DFS 2019 PreProcessing for Shiny App

shiny_df <- function(wk_num,date) {

library(tidyverse)
library(mattDFS)

# Logging into stathead ----
stathead <- stathead_login()

# Reading in team name chart ----------------------------------------------
source("~/ff_shiny_app/ff_app/find_names.R", local = T)
source("~/ff_shiny_app/ff_app/name_fixes.R", local = T)

# Vegas Lines -------------------------------------------------------------
vegas <- vegas_lines(date)
vegas[["team"]] <- sapply(vegas[["team"]], function(x) find_names(x, "vegas"))
print('Vegas Lines Successful')

# DVOA Data ---------------------------------------------------------------
dvoa <- dvoa(playoffs = FALSE)
dvoa_defense <- dvoa$defense
dvoa_offense <- dvoa$offense

dvoa_defense[["def_team"]] <- sapply(dvoa_defense[["def_team"]], function(x) find_names(x, "fff_abbreviation"))
dvoa_offense[["off_team"]] <- sapply(dvoa_offense[["off_team"]], function(x) find_names(x, "fff_abbreviation"))
print('DVOA Successful')

# Last Week Data ----------------------------------------------------------

wk_data <- lapply(list("QB", "RB", "WR", "TE"), function(position) {

          # If week is 1, use last year's data
          wk_data <-  paste("https://stathead.com/football/pgl_finder.cgi?request=1&match=game&order_by_asc=0&order_by=fanduel_points&year_min=2020",
                            "&year_max=2020&game_type=R&ccomp%5B1%5D=gt&cval%5B1%5D=0&cstat%5B1%5D=rush_att&ccomp%5B2%5D=gt&cval%5B2%5D=0",
                            "&cstat%5B2%5D=pass_cmp&ccomp%5B3%5D=gt&cval%5B3%5D=0&cstat%5B3%5D=fanduel_points&ccomp%5B4%5D=gt&cval%5B4%5D=0&cstat%5B4%5D=targets&positions%5B%5D=",
                            str_to_lower(position),
                            "&age_min=0&age_max=99&game_num_min=0&game_num_max=99&week_num_min=",
                            as.character(wk_num - 1),
                            "&week_num_max=",
                            as.character(wk_num - 1),
                            "&season_start=1&season_end=-1",
                            sep = "")

          wk_data <- stathead %>% jump_to(url = wk_data) %>%
                      read_html() %>%
                      html_table(fill = T) %>%
                      .[[1]]
})

print("Last Week Data Successful")

# Red Zone Data -----------------------------------------------------------
rz_types <- list("passing","rushing","receiving")

rz_df <- lapply(rz_types,
                function(x) {

                  # Reading in Data
                  d <- paste("http://www.pro-football-reference.com/years/2020/redzone-",
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
# Fixing Names
rz_df <- lapply(rz_df, function(x) {name_fixes(x, 1, 2, 0)})

print("Red Zone Offensive Data Successful")

# Year to Date Data -------------------------------------------------------

ytd_df <- lapply(list("passing","rushing","receiving"), function(x) {

  # Reading in Data
  d <- paste("http://www.pro-football-reference.com/years/2020/",
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
      select(-Age, -GS, -Lng) %>%
      mutate(Pos = str_to_upper(Pos))

  } else {

    # Rush is divided into two rows
    colnames(d) <- d[1,]
    d <- d[-1,] %>%
      subset(Pos != "Pos" & as.numeric(Att) > 4) %>%
      select(-Age, -GS, -Lng) %>%
      mutate(Pos = str_to_upper(Pos))

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
                ytd_pass_comp = round(ytd_pass_comp / ytd_pass_g,2),
                ytd_pass_att = round(ytd_pass_att / ytd_pass_g,2),
                ytd_pass_yds = round(ytd_pass_yds / ytd_pass_g,2),
                ytd_pass_td = round(ytd_pass_td / ytd_pass_g,2),
                ytd_pass_int = round(ytd_pass_int / ytd_pass_g,2))
  } else {
    if(x == "receiving") {
      d <- mutate(d,
                  ytd_rec_player = str_remove_all(ytd_rec_player, '[:punct:]'),
                  ytd_rec_target = round(ytd_rec_target / ytd_rec_g,2),
                  ytd_rec_rec = round(ytd_rec_rec / ytd_rec_g,2),
                  ytd_rec_yds = round(ytd_rec_yds / ytd_rec_g,2),
                  ytd_rec_td = round(ytd_rec_td / ytd_rec_g,2))
    } else {
      d <- mutate(d,
                  ytd_rush_player = str_remove_all(ytd_rush_player, '[:punct:]'),
                  ytd_rush_att = round(ytd_rush_att / ytd_rush_g,2),
                  ytd_rush_yds = round(ytd_rush_yds / ytd_rush_g,2),
                  ytd_rush_td = round(ytd_rush_td / ytd_rush_g,2))
    }}

})

ytd_df <- lapply(ytd_df, function(x) {name_fixes(x, 1, 2, 3)})

print("Year to Data Data Successful")

# Combinging into one list ------------------------------------------------
data <- list(wk_data = wk_data,
             rz_df = rz_df,
             ytd_df = ytd_df)
# Positions ---------------------------------------------------------------
source("~/ff_shiny_app/ff_app/position_information.R", local = T)

# QB
qb <- position_stats("QB",wk_num, data, tm_names) %>%
      mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
             proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

# RB
rb <- position_stats("RB",wk_num,data, tm_names) %>%
  mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
         proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

# WR
wr <- position_stats("WR",wk_num,data,tm_names) %>%
  mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
         proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

source("~/ff_shiny_app/ff_app/wr_vs_cb.R")
wr_matchup <- wr_vs_cv(paste("~/ff_shiny_app/ff_app/data/wide-receiver-vs-cornerback-matchup-analysis-week-",
                             wk_num,
                             "-table.html",
                             sep = ""))

# Adding in matchup stats
wr <- left_join(wr, wr_matchup, by = c("proj_player" = "vs_cb_wr"))

# TE
te <- position_stats("TE",wk_num,data, tm_names) %>%
  mutate(proj_tm = ifelse(proj_tm == "character(0)", "LVE", proj_tm),
         proj_opp = ifelse(proj_opp == "character(0)", "LVE", proj_opp))

print("Positional Data Successful")

# Combinng into one DF ----------------------------------------------------

all_positions <- merge(qb,rb, all = TRUE) %>%
                 merge(wr, all = TRUE) %>%
                 merge(te, all = TRUE) %>%
                 left_join(vegas, by = c("proj_tm" = "team")) %>%
                 left_join(dvoa_defense, by = c("proj_opp" = "def_team")) %>%
                 left_join(dvoa_offense, by = c("proj_tm" = "off_team"))


# Adding previous week DVOA -----------------------------------------------

dvoa_previous <- dvoa_defense
names(dvoa_previous) <- paste("prev_wk_dvoa", names(dvoa_previous), sep = "_")

all_positions <- left_join(all_positions, dvoa_previous, by = c("prev_wk_opp" = "prev_wk_dvoa_def_team"))
print("Previous Week DVOA Successful")

# Adding Leverage Scores --------------------------------------------------
leverage <- read.csv(paste("~/ff_shiny_app/ff_app/data/4for4-gpp-leverage-scores-table_wk",
                     wk_num,
                     ".csv", sep = "")) %>%
            select(-Opp) %>%
            mutate(Cash.Odds = as.numeric(gsub("\\%", "", Cash.Odds)),
                   GPP.Odds = as.numeric(gsub("\\%", "", GPP.Odds)),
                   Implied.Own. = as.numeric(gsub("\\%", "", Implied.Own.)),
                   Projected.Own. = as.numeric(gsub("\\%", "", Projected.Own.)),
                   Tm = as.character(Tm),
                   Player = str_remove_all(Player, "[:punct:]"),
                   FD.Sal.. = as.numeric(str_remove_all(FD.Sal.., "[,|$]")))

leverage_names <- colnames(leverage) %>%
                  str_to_lower() %>%
                  str_remove("[.]+$") %>%
                  str_replace("[.]","_")

colnames(leverage) <- leverage_names

# fixing team names and some select player names
leverage[["tm"]] <- as.character(sapply(leverage[["tm"]], function(x) find_names(x, "fff_abbreviation")))
leverage[["player"]] <- str_replace(leverage[["player"]], "Mitch", "Mitchell")

leverage <- leverage %>% mutate(tm = ifelse(tm == "character(0)", "LVR", tm))

# Adding into full DF
all_positions <- left_join(all_positions, leverage, by = c("proj_player" = "player",
                                                           "proj_pos" = "pos",
                                                           "proj_tm" = "tm"))


print("Leverage Score Successful")

# Adding pricing information ----
pricing <- get_pricing(wk_num)
all_positions <- left_join(all_positions, pricing, by = c("proj_player" = "pricing_player",
                                                           "proj_pos" = "pricing_position",
                                                           "proj_tm" = "pricing_team"))
print("Pricing Data Import Successful")

# Adding in the advanced stats information ----
source("~/ff_shiny_app/ff_app/advanced_stats.R")
adv_pass <- advanced_passing_stats()
adv_rush <- advanced_rushing_stats()
adv_rec <- advanced_receiving_stats()

# Joining into one large dataframe
all_data <- all_positions %>%
            left_join(adv_pass, by = c("proj_player" = "adv_player")) %>%
            left_join(adv_rush, by = c("proj_player" = "adv_player")) %>%
            left_join(adv_rec, by = c("proj_player" = "adv_receiving_player"))

print("Advanced Stats Successful")

# Adding in individual information from Football Outsiders ----
source("~/ff_shiny_app/ff_app/football_outsiders_advanced_position.R")

# Running specific Data
qb_df <- fo_qb()
rb_df <- fo_rb()
wr_df <- fo_pass_catchers("wr")
te_df <- fo_pass_catchers("te")

# Merging into 1 DF
fo_all_positions <- full_join(qb_df, rb_df, by = c("pass_player" = "rush_player",
                                                   "pass_team" = "rush_team",
                                                   "rush_dyar","rush_eyds", "rush_dvoa",
                                                   "rush_yar","rush_voa","rush_yards")) %>%
                    full_join(wr_df, by = c("pass_player" = "rec_player",
                                            "pass_team" = "rec_team",
                                            "rec_dyar" , "rec_dvoa", "rec_eyds", "rec_catch_rate",
                                            "rec_yar","rec_voa","rec_yards")) %>%
                    full_join(te_df, by = c("pass_player" = "rec_player",
                                            "pass_team" = "rec_team",
                                            "rec_dyar" , "rec_dvoa", "rec_eyds", "rec_catch_rate",
                                            "rec_yar","rec_voa","rec_yards")) %>%
                    mutate(pass_player = ifelse(is.na(str_extract(pass_player, "^[:upper:]\\.[:upper:]\\.")) == T,
                                        pass_player,
                                        str_remove(pass_player, "\\.")),
                    pass_player = str_replace(pass_player, "[:punct:]", " "),
                    pass_player = str_remove_all(pass_player, "-"))

fo_all_positions[["pass_team"]] <- sapply(fo_all_positions[["pass_team"]], function(x) find_names(x, "fff_abbreviation"))

# Merging with rest of data
all_data2 <- all_data %>%
             mutate(proj_player_new = str_remove_all(proj_player, "(?<=[:upper:])[:lower:]{1,}(?=[:space:])"))


# Combining
all_data_fo_pos <- left_join(all_data2, fo_all_positions, by = c("proj_player_new" = "pass_player", "proj_tm" = "pass_team")) %>%
                   select(-proj_player_new)
print("FO All Advanced Position Stats Combined")

# Adding in pace of place stats ----
pace <- get_pace()
pace[["off_team"]] <- sapply(pace[["off_team"]], function(x) find_names(x, "fff_abbreviation"))
all_data_fo_pos_pace <- left_join(all_data_fo_pos, pace, by = c("proj_tm" = "off_team"))

print("Pace of Play Successful")

# Returning full DF ----
return(all_data_fo_pos_pace)

}

# Getting full dataframe --------------------------------------------------
df <- shiny_df(6, "10/18")

#writexl::write_xlsx(df, "~/ff_shiny_app/ff_app/data/all_data_wk_6_2020.xlsx")
