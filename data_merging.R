#### test
library(tidyverse)

f <- as.list(list.files("~/ff_shiny_app/ff_app/data/",
                pattern = "all_data",
                full.names = T)
)

f2 <- lapply(f, function(x) {
  d <- readxl::read_xlsx(x)
  return(t(as.data.frame(names(d))))
  }) %>%
  bind_cols(fill = NA)

w11 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_11.xlsx") %>% names()
w10 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_10.xlsx") %>% names()
w9 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_9.xlsx") %>% names()
w15 <- readxl::read_xlsx("~/ff_shiny_app/ff_app/data/all_data_wk_15.xlsx") %>%
       select(-ytd_rec_1d,-ytd_pass_1d, -ytd_rush_1d) %>% names()

ttt <- as.data.frame(cbind(w15,w11,w10,w9)) %>% 
       mutate(w15_w11 = ifelse(as.character(w15) == as.character(w11),1,0),
              w15_w10 = ifelse(as.character(w15) == as.character(w10),1,0),
              w15_w9 = ifelse(as.character(w15) == as.character(w9),1,0))


# So far, this works with wk 15-10
dd <- rbind(w15,w14,w13,w12,w11)



