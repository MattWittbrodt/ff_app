
####
#### Merging 2020 data
####


wk7 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_7_2020.xlsx") %>%
       mutate(adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

wk6 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_6_2020.xlsx") %>%
       mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
              vs_cb_fpt = as.numeric(vs_cb_fpt),
              adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

pp <- full_join(wk7, wk6, by = colnames(wk6))
writexl::write_xlsx(pp, "C:/Users/mattw/Desktop/week7and6combined.xlsx")


wk5 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_5_2020.xlsx") %>%
       mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
              vs_cb_fpt = as.numeric(vs_cb_fpt),
              adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

wk4 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_4_2020.xlsx") %>%
  mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
         vs_cb_fpt = as.numeric(vs_cb_fpt),
         adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))
