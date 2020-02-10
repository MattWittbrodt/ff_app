#### test
library(tidyverse)

# Getting files, reading in, and putting into a list -----
f <- as.list(list.files("~/ff_shiny_app/ff_app/data/analysis_data/",
                pattern = "all_data",
                full.names = T)) %>%
     lapply(function(x) {
  
      if (str_detect(x, "wk_15") | str_detect(x, "wk_16")) {
         d <- readxl::read_xlsx(x) %>%
              select(-ytd_rec_1d,-ytd_pass_1d, -ytd_rush_1d)
      } else {d <- readxl::read_xlsx(x)}
       
     d <- mutate(d, vs_cb_ypr = as.numeric(vs_cb_ypr),
                    vs_cb_fpt = as.numeric(vs_cb_fpt),
                    dline_adj_line_yards = as.numeric(dline_adj_line_yards),
                    oline_adj_line_yards = as.numeric(oline_adj_line_yards),
                    prev_wk_dvoa_dline_adj_line_yards = as.numeric(prev_wk_dvoa_dline_adj_line_yards))
    
      return(d)
      }) %>% 
    bind_rows()

# Putting into one dataframe ----

f2 <- bind_rows(f)






# Workspace for later adding w4,w6,w5 etc-----------------------------------------------------------------------
w4 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_4.xlsx") %>% names()
w6 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_6_2.xlsx") %>% names()
w5 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_5.xlsx") %>% names()
w15 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_15.xlsx") %>%
       select(-ytd_rec_1d,-ytd_pass_1d, -ytd_rush_1d) %>% names()

ttt <- as.data.frame(cbind(w15,w6,w5,w4)) %>% 
       mutate(w15_w6 = ifelse(as.character(w15) == as.character(w6),1,0),
              w15_w4 = ifelse(as.character(w15) == as.character(w4),1,0),
              w15_w5 = ifelse(as.character(w15) == as.character(w5),1,0))

# So far, this works with wk 15-10
dd <- rbind(w15,w14,w13,w12,w11)

## processing wei to dave
# x = string of column names
replaced_names <- function(x) {
  
  n <- x

  # converting the 'DAVE' to 'WEI'
  for(ii in 1:length(n)) {
    
    n_first <- n[ii]
    
    if (str_detect(n_first,"dave")) {
      
      # Recognizing a general pattern of dave/dvoa metrics
      n_first <- str_replace(n_first, "dave", "dvoa")
      n_first <- paste("wei", n_first, sep = "_")
      
      if (str_detect(n_first, "total_dvoa")) {n_first <- str_remove(n_first, "_total")}
      n_first <- switch(n_first,
                        "wei_prev_wk_dvoa_dvoa" = "prev_wk_dvoa_wei_dvoa",
                        "wei_prev_wk_dvoa_total_dvoa" = "prev_wk_dvoa_wei_dvoa",
                        "wei_prev_wk_dvoa_defense_dvoa" = "prev_wk_dvoa_wei_defense_dvoa",
                        "wei_prev_wk_dvoa_defense_dvoa_rk" = "prev_wk_dvoa_wei_defense_dvoa_rk",
                        n_first)
    }
    
    # Fixing a few issues
    n_first <- n_first %>%
               str_replace("rbyards","rb_yards") %>%
               str_replace("powerrank", "power_rank") %>%
               str_replace("stuffedrank", "stuffed_rank") %>%
               str_replace("levelyards", "level_yards") %>%
               str_replace("levelrank", "level_rank") %>%
               str_replace("fieldyards", "field_yards") %>%
               str_replace("fieldrank", "field_rank") %>%
               str_replace("adj_total", "adjtotal") %>%
               str_replace("adj_pass", "adjpass") %>%
               str_replace("adj_rush", "adjrush")
    
    
    n[ii] <- n_first
  }
  
  return(n)
}


  

