## Top 10

library(tidyverse)

wk_num <- 12

pos <- "QB"

d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to11_combined.xlsx") %>%
     filter(proj_week <= wk_num - 1 & proj_week > 3 & proj_pos == pos)

cur_wk <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_12_2020.xlsx") %>%
          filter(proj_pos == pos)


# need previous week fantasy data, so reading in this week's data to look at
tw <- readxl::read_xlsx(paste0("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_",wk_num,"_2020.xlsx")) %>%
      filter(proj_pos == pos) %>%
      select(proj_player, prev_wk_fantasy_fdpt)

weeks <- unique(d$proj_week)

compiled_data <- lapply(as.list(weeks), function(x) {

  # Getting last week and current week data
  wk <- filter(d, proj_week == x) %>% select(-prev_wk_fantasy_fdpt)

  if(x < max(weeks)){
    lw <- filter(d, proj_week == (x+1)) %>% select(proj_player, prev_wk_fantasy_fdpt)
  } else {lw <- tw}

  # Add last week data to current week
  combined <- left_join(wk, lw, by = "proj_player") %>% filter(is.na(prev_wk_fantasy_fdpt) == F)

  # Adding whether they are top ten or not
  top <- combined %>% slice_max(order_by = prev_wk_fantasy_fdpt, n = 10) %>% mutate(top_ten = factor(1))
  not_top <- combined %>% slice_min(order_by = prev_wk_fantasy_fdpt, n = nrow(combined) - 10) %>% mutate(top_ten = factor(0))

  if(nrow(top) + nrow(not_top) != nrow(combined)) {print(x); break}
  # Put back into one DF
  combined <- rbind(top, not_top)

  return(combined)

})

# Putting into a DF and doing some QB-specific processing
processed_data <- do.call(rbind, compiled_data)

qb_col <- c("proj_pos","proj_player","prev_wk_fantasy_fdpt","proj_week",
            "pts_vs_g", "projected_own","proj_afpa_rk","line","total",
            "implied_total","pts_vs_passing_att", "pts_vs_passing_yds", "pts_vs_passing_td",
            "pts_vs_fantasy_per_game_fdpt","def_red_zone_td","def_red_zone_pct",
            "def_dvoa", "def_pass_dvoa", "def_dline_adjusted_sack_rate_rk",
            "def_dline_adjusted_sack_rate","def_pass_qb_rating_allowed",
            "def_pass_adj_net_yds_per_att", "off_oline_adjusted_sack_rate",
            "ytd_pass_yds_per_gm","ytd_pass_td","adv_passing_iay","pass_dyar",
            "adv_passing_ontgt_per","adv_passing_bad_per",
            "adv_passing_prss_per","pass_eyds","pass_yards","rush_eyds","rush_dyar","rush_yards",
            "passing_twenty_att","passing_twenty_td","top_ten")

processed_data <- select(processed_data, qb_col) %>%
                  mutate(adv_passing_iay = round(adv_passing_iay / as.numeric(pts_vs_g), 2),
                         pass_dyar = round(pass_dyar/as.numeric(pts_vs_g),2),
                         rush_yards = round(rush_yards / as.numeric(pts_vs_g),2),
                         pass_yds_diff = round((pass_eyds - pass_yards)/as.numeric(pts_vs_g),2),
                         rush_yds_diff = round((rush_eyds - rush_yards)/as.numeric(pts_vs_g),2),
                         DVOA_Diff = def_pass_dvoa - def_dvoa,
                         pts_vs_passing_att = round(as.numeric(pts_vs_passing_att) / as.numeric(pts_vs_g),2),
                         pts_vs_passing_yds = round(as.numeric(pts_vs_passing_yds) / as.numeric(pts_vs_g),2),
                         pts_vs_passing_td = round(as.numeric(pts_vs_passing_td) / as.numeric(pts_vs_g),2),
                         pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt)) %>%
                  select(-pts_vs_g)

cur_wk_processed <- select(cur_wk, qb_col[-length(qb_col)]) %>%
                  mutate(adv_passing_iay = round(adv_passing_iay / as.numeric(pts_vs_g), 2),
                         pass_dyar = round(pass_dyar/as.numeric(pts_vs_g),2),
                         rush_yards = round(rush_yards / as.numeric(pts_vs_g),2),
                         pass_yds_diff = round((pass_eyds - pass_yards)/as.numeric(pts_vs_g),2),
                         rush_yds_diff = round((rush_eyds - rush_yards)/as.numeric(pts_vs_g),2),
                         DVOA_Diff = def_pass_dvoa - def_dvoa,
                         pts_vs_passing_att = round(as.numeric(pts_vs_passing_att) / as.numeric(pts_vs_g),2),
                         pts_vs_passing_yds = round(as.numeric(pts_vs_passing_yds) / as.numeric(pts_vs_g),2),
                         pts_vs_passing_td = round(as.numeric(pts_vs_passing_td) / as.numeric(pts_vs_g),2),
                         pts_vs_fantasy_per_game_fdpt = as.numeric(pts_vs_fantasy_per_game_fdpt)) %>%
                  select(-pts_vs_g)

# Cleaning up data (for entire dataset)
processed_data[,-c(2:3)] <- apply(processed_data[,-c(2:3)], 2,
                            function(x) {c <- str_remove_all(as.character(x), "%") %>% as.numeric()})

cur_wk_processed[,-c(2:3)] <- apply(cur_wk_processed[,-c(2:3)], 2,
                                  function(x) {c <- str_remove_all(as.character(x), "%") %>% as.numeric()})



# For now, doing a median replace for NA's
# TODO - smarter NA replacement
#for(ii in c(5:36,38:40)) {processed_data[,ii][is.na(processed_data[,ii])] <- median(unlist(processed_data[,ii]), na.rm = T)}
for(ii in c(4:ncol(processed_data))) {processed_data[,ii][is.na(processed_data[,ii])] <- median(unlist(processed_data[,ii]), na.rm = T)}
for(ii in c(4:ncol(cur_wk_processed))) {cur_wk_processed[,ii][is.na(cur_wk_processed[,ii])] <- median(unlist(cur_wk_processed[,ii]), na.rm = T)}

# Normalizing
processed_data[,c(6:36,38:40)] <- apply(processed_data[,c(6:36,38:40)], 2, function(x) {m <- mean(x); sd = sd(x); x-m/sd})
processed_data$top_ten <- as.factor(processed_data$top_ten)


cur_wk_processed[,c(6:ncol(cur_wk_processed))] <- apply(cur_wk_processed[,c(6:ncol(cur_wk_processed))], 2, function(x) {m <- mean(x); sd = sd(x); x-m/sd})



# Running Random Forest and PCA -----
library(randomForest)

train <- filter(processed_data, proj_week <= wk_num - 2)
test <- filter(processed_data, proj_week == wk_num - 1)

# Creating a formula for the RF
formula <- "top_ten ~ "
for(n in colnames(processed_data[,c(6:36,38:40)])) {if(n != 'DVOA_Diff') {formula <- paste0(formula,n," + ")} else {formula <- paste0(formula, n)}}
formula<- as.formula(formula)

# Getting PCA on data
classifier <- randomForest(formula,
                           data = train,
                           ntree = 10000,
                           sampsize = 25,
                           importance = T)

varImpPlot(classifier)
classifier

## Getting metrics on training data
training_pred <- predict(classifier, type = "prob")
train$pred <- training_pred[,2]

## Running a sensitivity analysis on decision marker
# Sensitivity Analysis for threshold -----
for(ii in c(0.30,0.40,0.45,0.50,0.55,0.60,0.65)) {

  # Getting working version of df
  wd <- train

  # do accuracy calculation
  wd$outcome_bi = ifelse(wd$pred > ii, 1, 0)
  wd$tp = ifelse(wd$outcome_bi == 1 & wd$top_ten == 1,1,0)
  wd$fp = ifelse(wd$outcome_bi == 1 & wd$top_ten == 0,1,0)
  wd$fn = ifelse(wd$outcome_bi == 0 & wd$top_ten == 1,1,0)
  wd$tn = ifelse(wd$outcome_bi == 0 & wd$top_ten == 0,1,0)

  # Calculating Values
  accuracy <- (sum(wd$tp) + sum(wd$tn)) / nrow(wd)
  precision <- sum(wd$tp) / (sum(wd$tp) + sum(wd$fp))
  recall <- sum(wd$tp) / (sum(wd$tp) + sum(wd$fn))
  f1 <- 2 * ((precision * recall)/ (precision + recall))

  # Getting MSE
  wd$sq_error <- (as.numeric(as.character(wd$top_ten)) - wd$pred)^2

  # Print result
  cat(paste0("Threshold of: ",ii,
             " = Accuracy: ", round(accuracy*100,2),
             " | Precision: ", round(precision*100,2),
             " | Recall: ", round(recall*100,2),
             " | F1: ", round(f1,2),
             " | MSE: ", round(mean(wd$sq_error),4),
             " \n"))

}

## Test dataset testing ----
test <- test[-4,]
test_pred <- predict(classifier, newdata = test, type = "prob")
test$pred <- test_pred[,2]

cur_wk_pred <- predict(classifier, newdata = cur_wk_processed, type = "prob")
cur_wk_processed$pred <- cur_wk_pred[,2]
cur_wk_processed$top_ten <- ifelse(cur_wk_processed$pred > 0.5, 1, 0)
predictions <- select(cur_wk_processed, proj_player, top_ten)

test$pred_binomial <- ifelse(test$pred > 0.5, 1, 0)

test$tp = ifelse(test$pred_binomial == 1 & test$top_ten == 1,1,0)
test$fp = ifelse(test$pred_binomial == 1 & test$top_ten == 0,1,0)
test$fn = ifelse(test$pred_binomial == 0 & test$top_ten == 1,1,0)
test$tn = ifelse(test$pred_binomial == 0 & test$top_ten == 0,1,0)

# Calculating Values
(accuracy <- (sum(test$tp) + sum(test$tn)) / nrow(test))
(precision <- sum(test$tp) / (sum(test$tp) + sum(test$fp)))
(recall <- sum(test$tp) / (sum(test$tp) + sum(test$fn)))
(f1 <- 2 * ((precision * recall)/ (precision + recall)))

# Getting MSE
test$sq_error <- (as.numeric(as.character(test$top_ten)) - test$pred)^2
mean(test$sq_error)


