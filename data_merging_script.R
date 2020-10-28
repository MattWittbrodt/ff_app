
####
#### Merging 2020 data
####
dupl_check <- function(x) {str_detect(x, "\\.(y|x)")}

wk7 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_7_2020.xlsx") %>%
       mutate(adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

wk6 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_6_2020.xlsx") %>%
       mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
              vs_cb_fpt = as.numeric(vs_cb_fpt),
              adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

pp <- full_join(wk7, wk6, by = colnames(wk6))
colnames(pp)[dupl_check(colnames(pp))]
writexl::write_xlsx(pp, "C:/Users/mattw/Desktop/week7and6combined.xlsx")

# Week 5 -----
wk5 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_5_2020.xlsx") %>%
       mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
              vs_cb_fpt = as.numeric(vs_cb_fpt),
              adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))
writexl::write_xlsx(pp, "C:/Users/mattw/Desktop/2020_weeks5to7_combined.xlsx")


# Merging 5, 6, 7 & checking for .x or .y column names
pp2 <- full_join(pp, wk5, by = colnames(wk5))
colnames(pp2)[dupl_check(colnames(pp2))]
writexl::write_xlsx(pp2, "C:/Users/mattw/Desktop/2020_weeks5to7_combined.xlsx")

# Week 4 -------
wk4 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_4_2020.xlsx") %>%
  mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
         vs_cb_fpt = as.numeric(vs_cb_fpt))

wk4[,c(272:324)] <- apply(wk4[,c(272:324)], 2, function(x) {as.numeric(x)})

pp3 <- full_join(pp2, wk4, by = colnames(wk4))
colnames(pp3)[dupl_check(colnames(pp2))]
writexl::write_xlsx(pp3, "C:/Users/mattw/Desktop/2020_weeks4to7_combined.xlsx")

# Week 3 -------
wk3 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/analysis_data/all_data_wk_3_2020_advanced_FO.xlsx") %>%
  mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
         vs_cb_fpt = as.numeric(vs_cb_fpt),
         adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))
wk3[,c(272:324)] <- apply(wk3[,c(272:324)], 2, function(x) {as.numeric(x)})

pp4 <- full_join(pp3, wk3, by = colnames(wk3))
colnames(pp4)[dupl_check(colnames(pp4))]
writexl::write_xlsx(pp4, "C:/Users/mattw/Desktop/2020_weeks_3to7_combined.xlsx")

# Week 2 -------
wk2 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_2_2020.xlsx") %>%
  mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
         vs_cb_fpt = as.numeric(vs_cb_fpt),
         adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))
wk2[,c(272:324)] <- apply(wk2[,c(272:324)], 2, function(x) {as.numeric(x)})

pp5 <- full_join(pp4, wk2, by = colnames(wk2))
colnames(pp5)[dupl_check(colnames(pp5))]
writexl::write_xlsx(pp5, "C:/Users/mattw/Desktop/2020_weeks_2to7_combined.xlsx")
