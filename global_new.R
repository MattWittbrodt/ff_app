## Global file for EMA app
## Inspiration: https://mraess.netlify.app/2018/07/the-awesomeness-that-is-the-global-r-file-or-how-to-clean-up-your-shiny-app/

library(tidyverse)
library(lubridate)
library(writexl)

# Reading in complete data for week ----
get_filename <- function() {
  # List all files in directory with this structure, print out to log
  data_files <- list.files(path = "/srv/connect/apps/ff_app/data/", pattern = "all_data_wk_\\d{1,}_2020\\.xlsx")
  print(data_files)
  
  # Extract weeks, print actual file name used to log
  weeks = as.numeric(str_extract(string = data_files, pattern = "\\d(?=_)"))
  max_week_file <- data_files[weeks == max(weeks)]
  cat(paste0("This weeks file is: ", max_week_file, " \n"))

  return(max_week_file)
  }

#this_week_file <- get_filename()
#this_week_file <- paste0("data/", this_week_file)

this_week_file <- "C:/Users/mattw/Desktop/dfs_test.xlsx"


## Reading in specific excel file
# df <- readxl::read_xlsx(this_week_file) %>% 
        #mutate(proj_opp = ifelse(proj_field == 2, paste("@",proj_opp, sep = ""), proj_opp))

# Getting time
day_time <- Sys.time() - (60*60*6) # adjusting for time zone differences in shiny app

###
### Data Frame Creation
###

# @param x: (str) name of sheet to read
dfs_sheet <- function(file_name,x) {
  return(readxl::read_xlsx(file_name, sheet = x) %>% select(-week, -season))
}

# Main DFS Panel ----
dfs_df <- dfs_sheet(this_week_file, "main")
fd_sal_min <- min(dfs_df$fd_sal)
fd_sal_max <- max(dfs_df$fd_sal)

# QB Panel ----
def_qb <- dfs_sheet(this_week_file, "qb_def")
off_qb <- dfs_sheet(this_week_file, "qb_off")

# RB Panel ----
rb_def <- dfs_sheet(this_week_file, "rb_def")
rb_off <- dfs_sheet(this_week_file, "rb_off")

# WR Panel ----
wr_def <- dfs_sheet(this_week_file, "wr_def")
wr_off <- dfs_sheet(this_week_file, "wr_off")

# TE Panel Data ----
te_def <- dfs_sheet(this_week_file, "te_def")
te_off <- dfs_sheet(this_week_file, "te_off")


#######
####### Misc Text in App ----
#######

qb_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>IAY</strong> = intended air yards (yards on all passes, complete or not) || <strong>DYAR</strong> = Defense-adjusted Yards Above Replacement. This gives the value of the quarterbacks performance compared to replacement level, adjusted for situation and opponent and then translated into yardage.
<br><strong>Eyds - yds</strong> = Effective yards vs actual yards - translates DVOA into a yards/att number. Greater difference = played better than standard yards indicates and vice versa. More dependent on usage than DYAR.
<br><strong>On Target </strong>= percentage of throws on target || <strong>Bad Thrrow %</strong> = poor throws per attempt || <strong>Pressure %</strong> = percentage of pressures per drop back<br><br></span></font>'

rb_def_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>Total Touches:</strong> Rush Att + Target by RB vs. Defense ||&nbsp;<strong>Rushing Advantage</strong> = Rush D DVOA + Rush Off DVOA, higher is better || <strong>DVOA Difference</strong> = Rush DVOA - Defense DVOA, lower means rushing d is a strength (i.e., compartively better than overall)
<br><strong>Power Success</strong> = % runs on 3rd/4th down OR 1st/2nd &amp; goal from &lt;= 2 yds which were successful; Difference between offense is next column ||&nbsp;<strong>DVOA Difference</strong> = O Line success (%) - D Line Success (%) ||&nbsp;<strong>Adj Net Yards</strong> = Adjusted Yds allowed by D line
<br><strong>Difference vs Off</strong> = Adj Net Yds from Offense - Defense, higher is better<br><br></span></font>'

rb_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>High Value</strong> = Rushes inside 10 yd line + target (also expressed as % total touches) || <strong>DYAR</strong> = Defense-adjusted Yards Above Replacement (performance on plays with RB carry/catch vs. replacement level, adjusted for situation and translated to yardage)
<br><strong>DVOA</strong> = &nbsp;Defense-adjusted Value Over Average. Value, per play, over an average running back in the same game situations. More positive DVOA = better performance (negative = below-average). The simple version: <em>DYAR means a running back with more total value. DVOA means a running back with more value per play</em>. || <strong>Diff</strong> = Difference between Effective Yards (translation of DVOA into a yards per attempt) and regular yards. Players with more Effective Yards vs standard yards played better than standard stats would otherwise indicate (this measure is dependent on usage than DYAR). || <strong>Suc.</strong> % = &nbsp;successful running plays (the definition of success being different based on down and distance) divided by total running plays. A player with higher DVOA and a low success rate mixes long runs with downs getting stuffed at the line of scrimmage. A player with lower DVOA and a high success rate generally gets the yards needed, but doesn&#39;t often get more. It is not adjusted for opponent.<br><br></span></font>'

receiver_def_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<br><strong>Def (DVOA):</strong> Total defense DVOA (<strong>passing only = Pass</strong>) || <strong>Pass Adv</strong> = Pass D DVOA (opponent) + Pass Off (own team) DVOA, higher is better || <strong>(DVOA) Difference</strong> = Pass DVOA - Defense DVOA, lower means passing d is a strength (i.e., compartively better than overall)
<br><strong>Adj Sack Rate</strong> % of sacks on each dropback by defensive live || <strong>Sake Rate Diff</strong> = Offensive O Line Sack Rate - Defense D Line Sack rate, greater = better and indicative of more time to throw<br><br></span></font>'

wr_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<p><strong>DYAR&nbsp;</strong>= Defense-adjusted Yards Above Replacement (performance on plays with WR catch vs. replacement level, adjusted for situation and translated to yardage) ||
<strong>DVOA&nbsp;</strong>= &nbsp;Defense-adjusted Value Over Average. Value, per play, over an average wide receiver in the same game situations. More positive DVOA = better performance (negative = below-average).
The simple version: DYAR means a wide receiver with more total value. DVOA means a wide receiver with more value per play. || <strong>EYds - Yds</strong> = Difference between Effective Yards (translation of DVOA into a yards per attempt) and regular yards. Players with more Effective Yards vs standard yards played better than standard stats would otherwise indicate (this measure is dependent on usage than DYAR). ||
<strong>ADOT&nbsp;</strong>= average depth of target (in yds) ||
<strong>Air Yards</strong> (yds/gm) = targets / gm * ADOT (yds/tgt); how many yards a player is targeted with, on average, per game. ||
<strong>RACR</strong>= Receiver Air Conversion Ratio (Receiving Yards / Air Yards). RACR is an efficiency metric that rolls up catch rate and yards after the catch into one number. It can also be thought of as the number of receiving yards a player creates for every air yard thrown at him. ||
<strong>Target and Air Yard %</strong> = Share (%) of total passes (target) and air yards for the player || <strong>WOPR</strong> = Weighted opportunity rating that incorporates a players share of team targets and air yards (1.5 x Target Market Share + 0.7 x Air Yards Market Share). This essentially is trying to equate slot receivers (high target #, low air yards) with deep threats (low target #, high air yards) to get a better picture of usage.<br><br></span></font>'

te_off_legend <- '<span style="color: #666666;"><font size=-1><strong>Legend:</strong>
<p><strong>DYAR</strong>= Defense-adjusted Yards Above Replacement (performance on plays with TE catch vs. replacement level, adjusted for situation and translated to yardage) ||
<strong>DVOA</strong> = Defense-adjusted Value Over Average. Value, per play, over an average tight end in the same game situations. More positive DVOA = better performance (negative = below-average). The simple version: DYAR means a tight end with more total value. DVOA means a wide receiver with more value per play. ||
<strong>EYds - Yds</strong> = Difference between Effective Yards (translation of DVOA into a yards per attempt) and regular yards. Players with more Effective Yards vs standard yards played better than standard stats would otherwise indicate (this measure is dependent on usage than DYAR). ||
<strong>ADOT</strong> = average depth of target (in yds) || <strong>Air Yards</strong> (yds/gm) = targets / gm * ADOT (yds/tgt); how many yards a player is targeted with, on average, per game. ||
<strong>RACR</strong>= Receiver Air Conversion Ratio (Receiving Yards / Air Yards). RACR is an efficiency metric that rolls up catch rate and yards after the catch into one number. It can also be thought of as the number of receiving yards a player creates for every air yard thrown at him. ||
<strong>Target and Air Yard %</strong> = Share (%) of total passes (target) and air yards for the player || <strong>WOPR</strong> = Weighted opportunity rating that incorporates a players share of team targets and air yards (1.5 x Target Market Share + 0.7 x Air Yards Market Share). This essentially is trying to equate slot receivers (high target #, low air yards) with deep threats (low target #, high air yards) to get a better picture of usage.
|| <strong> Pass DVOA </strong> = team passing DVOA <br><br></span></font>'


