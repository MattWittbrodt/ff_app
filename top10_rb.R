## Top 10
library(tidyverse)

wk_num <- 12

pos <- "RB"

d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to11_combined.xlsx") %>%
  filter(proj_week <= wk_num - 1 & proj_week > 3 & proj_pos == pos)

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

rb_col <- c("proj_pos","proj_player","prev_wk_fantasy_fdpt","proj_week",
            "pts_vs_g", "projected_own","proj_afpa_rk","line","total",
            "implied_total", "ytd_rush_att", "pts_vs_fantasy_per_game_fdpt",
            "pts_vs_rec_tgt","pts_vs_rec_yds","pts_vs_rec_td",
            "pts_vs_rush_att","pts_vs_rush_yds","pts_vs_rush_td","high_value_touches","rush_eyds","rush_yards",
            "rec_eyds","rec_yards","rec_dyar","rush_dyar",
            "def_rush_yds_per_att","def_red_zone_td","def_red_zone_pct","ytd_rush_g",
            "def_dvoa","def_rush_dvoa","off_rush_dvoa","def_dline_power_success_rate",
            "def_dline_adjusted_line_yards","def_dline_stuffed_rate","def_dline_second_level_yards",
            "def_dline_open_field_yards","off_oline_adjusted_line_yards","off_oline_power_success_rate",
            "off_oline_stuffed_rate","off_oline_open_field_yards","off_oline_second_level_yards",
            "ytd_rec_target", "top_ten")

processed_data <- select(processed_data, rb_col) %>%
  mutate(pts_vs_g = as.numeric(pts_vs_g),
         pts_vs_rec_tgt = as.numeric(pts_vs_rec_tgt)/pts_vs_g,
         pts_vs_rec_yds = as.numeric(pts_vs_rec_yds)/pts_vs_g,
         pts_vs_rec_td = as.numeric(pts_vs_rec_td)/pts_vs_g,
         pts_vs_rush_att = as.numeric(pts_vs_rush_att)/pts_vs_g,
         pts_vs_rush_yds = as.numeric(pts_vs_rush_yds)/pts_vs_g,
         pts_vs_rush_td = as.numeric(pts_vs_rush_td)/pts_vs_g,
         pts_vs_total_touch = pts_vs_rush_att + pts_vs_rec_tgt,
         DVOA_Advantage = def_rush_dvoa + off_rush_dvoa,
         DVOA_Difference = def_rush_dvoa - def_dvoa,
         net_adj_line_yd_diff = off_oline_adjusted_line_yards - (def_dline_adjusted_line_yards*-1),
         power_success_diff = off_oline_power_success_rate - def_dline_power_success_rate,
         second_level_yds_diff = as.numeric(off_oline_second_level_yards) - as.numeric(def_dline_second_level_yards),
         open_fieldyards_diff = as.numeric(off_oline_open_field_yards) - as.numeric(def_dline_open_field_yards),
         ytd_rec_target = ifelse(is.na(ytd_rec_target) == T, 0, ytd_rec_target),
         total_touches = ytd_rush_att + ytd_rec_target,
         rush_eyard_diff = round((rush_eyds - rush_yards)/ytd_rush_g,2),
         rec_eyard_diff = round((rec_eyds - rec_yards)/ytd_rush_g,2),
         rec_dyar = round(rec_dyar/ytd_rush_g,2),
         rush_dyar = round(rush_dyar/ytd_rush_g,2)) %>%
  select(-pts_vs_g, ytd_rush_g)


# Cleaning up data (for entire dataset)
processed_data[,-c(2:3)] <- apply(processed_data[,-c(2:3)], 2,
                                  function(x) {c <- str_remove_all(as.character(x), "%") %>% as.numeric()})


# For now, doing a median replace for NA's
# TODO - smarter NA replacement
#for(ii in c(5:36,38:40)) {processed_data[,ii][is.na(processed_data[,ii])] <- median(unlist(processed_data[,ii]), na.rm = T)}
for(ii in c(4:ncol(processed_data))) {processed_data[,ii][is.na(processed_data[,ii])] <- median(unlist(processed_data[,ii]), na.rm = T)}

# Normalizing
#processed_data[,c(6:36,38:40)] <- apply(processed_data[,c(6:36,38:40)], 2, function(x) {m <- mean(x); sd = sd(x); x-m/sd})
processed_data[,c(5:42,44:53)] <- apply(processed_data[,c(5:42,44:53)], 2, function(x) {m <- mean(x); sd = sd(x); x-m/sd})
processed_data$top_ten <- as.factor(processed_data$top_ten)

# Running Random Forest and PCA -----
library(randomForest)

# Splitting into training set
train_split <- sort(sample(nrow(processed_data),nrow(processed_data)*.8))

train <- processed_data[train_split,]
test <- processed_data[-train_split,]

# Creating a formula for the RF
formula <- "top_ten ~ "
for(n in colnames(processed_data[,c(6:42,44:53)])) {if(n != 'rec_eyard_diff') {formula <- paste0(formula,n," + ")} else {formula <- paste0(formula, n)}}
formula<- as.formula(formula)

# Running the Random Forest
classifier <- randomForest(formula,
                           data = train,
                           ntree = 10000,
                           sampsize = 25,
                           importance = T)

varImpPlot(classifier)
classifier

## Getting metrics on training data
training_pred <- predict(classifier, type = "prob")
train$pred <- training_pred[,1]

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
test_pred <- predict(classifier, newdata = test, type = "prob")
test$pred <- test_pred[,1]

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


