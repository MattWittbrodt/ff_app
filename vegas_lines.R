# Getting Vegas Lines
# date is string formatted "mm/dd"

vegas_lines <- function(date) {
  
  library(rvest)
  library(tidyverse)
  
  # Using rvest to get the data in
  #url <- "https://www.sportsline.com/nfl/odds"
  d <- "http://www.vegasinsider.com/nfl/odds/las-vegas/" %>%
        read_html() %>% 
        html_table(fill = T) %>%
        .[[12]]
  
  # New data cleaning
  df <- d %>% 
        filter(str_detect(X1,"/") == T & 
               str_detect(X1, date) == T &
               str_detect(X1, "8:") == F & 
               str_detect(X1, "9:") == F) %>%
        select(X1,X3)
  
  names(df) <- c("game", "total")
         
  df2 <- df %>%
         mutate(total = str_replace_all(total, "�", ".5"),
                total = str_replace(total, "u-10", " "),
                total = str_remove(total, "-\\d{2}$"),
                game = str_remove_all(game, "\\."),
                game = str_remove(game, paste(date,"\\s+",sep = "")),
                game = str_remove(game, "PM\\s+"),
                teams = str_extract_all(game, "(?<=\\d{3}\\s)(\\w{5}\\s\\w{3}|\\w{4,}|\\w{2,}\\s\\w{2,})"),
                home = map_chr(teams, last),
                away = map_chr(teams, first),
                line = str_extract(total, "(-\\d+\\.\\d|-\\d+)"),
                line = ifelse(str_detect(total, "PK") == T, 0, line),
                favorite = ifelse(str_detect(total, ("^-")) == T, away, home),
                total = str_remove(total, "(-\\d+\\.\\d|-\\d+)"),
                total = str_remove(total, "-10"),
                total = str_remove(total, "EV"),
                total = str_remove(total, "PK"),
                total = str_remove(total, "-20"),
                total = str_remove(total, "-19"),
                total = str_remove(total, "-15"),
                total = str_remove(total, "-05")) %>%
        select(-teams, -game) %>%
        # moving into final df form
        gather(key = "home_or_away", value = "team", -total, -favorite, -line) %>%
        # Ordering columns 
        .[,c("team", "home_or_away", "line", "total", "favorite")] %>%
        # Further mutating  
        mutate(
           favorite = ifelse(team == favorite, 1, 0),
           line = ifelse(favorite == 1, as.numeric(line), as.numeric(line)*-1),
           total = as.numeric(str_remove_all(total, "\\s")),
           implied_total = (total/2)+((line*-1)/2),
           team = str_replace_all(team, "\\s", "_"))
  
  
  return(df2)
  
}

