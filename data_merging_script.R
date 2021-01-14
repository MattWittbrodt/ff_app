
####
#### Merging 2020 data
####
dupl_check <- function(x) {str_detect(x, "\\.(y|x)")}
library(tidyverse)

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
wk3[,c(272:336)] <- apply(wk3[,c(272:336)], 2, function(x) {as.numeric(x)})

pp4 <- full_join(pp3, wk3, by = colnames(wk3))
colnames(pp4)[dupl_check(colnames(pp4))]
writexl::write_xlsx(pp4, "C:/Users/mattw/Desktop/2020_weeks_3to7_combined.xlsx")

# Week 2 -------
wk2 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/analysis_data/all_data_wk_2_2020_advanced_FO.xlsx") %>%
  mutate(vs_cb_ypr = as.numeric(vs_cb_ypr),
         vs_cb_fpt = as.numeric(vs_cb_fpt),
         adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))
wk2[,c(272:346)] <- apply(wk2[,c(272:346)], 2, function(x) {as.numeric(x)})

pp5 <- full_join(pp4, wk2, by = colnames(wk2))
colnames(pp5)[dupl_check(colnames(pp5))]
writexl::write_xlsx(pp5, "C:/Users/mattw/Desktop/2020_weeks_2to7_combined.xlsx")

# Week 8 ----

data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to7_combined.xlsx")

wk8 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_8_2020.xlsx") %>%
       mutate(adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

pp <- full_join(wk8, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to8_combined.xlsx")

# Week 9 ----

data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to8_combined.xlsx")

wk9 <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_9_2020.xlsx") %>%
       mutate(adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa))

pp <- full_join(wk9, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to9_combined.xlsx")

# Week 10 ----

data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to9_combined_vegas_fixes.xlsx")

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_10_2020.xlsx") %>%
        mutate(adv_passing_iay_per_pa = as.numeric(adv_passing_iay_per_pa),
               vs_cb_ypr = as.numeric(vs_cb_ypr),
               vs_cb_fpt = as.numeric(vs_cb_fpt))

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to10_combined.xlsx")

# Week 11 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to10_combined_name_fixes.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:350,352:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_11_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to11_combined.xlsx")

# Week 12 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to11_combined.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_12_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to12_combined.xlsx")

# Week 13 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to12_combined.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_13_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to13_combined.xlsx")

# Week 14 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to13_combined.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_14_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to14_combined.xlsx")

# Week 15 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to14_combined.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_15_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to15_combined.xlsx")


# Week 16 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to15_combined.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_16_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to16_combined.xlsx")

# Week 17 ----
data <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to16_combined.xlsx")
data[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(data))] <- apply(data[,c(6:21,23:179,182:196,198:201,205:350, 352:ncol(data))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_17_2020.xlsx")
cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))] <- apply(cur_wk[,c(6:21,23:179,182:196,198:201,205:354,356:ncol(cur_wk))], 2, function(x) {as.numeric(str_remove_all(as.character(x), ",|%"))})

pp <- full_join(cur_wk, data, by = colnames(data))

colnames(pp)[dupl_check(colnames(pp))]

writexl::write_xlsx(pp, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to17_combined.xlsx")
