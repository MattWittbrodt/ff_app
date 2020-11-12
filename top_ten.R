# ###
# ### This function will go through and look at each week's top 10
# ###
#

library(tidyverse)
library(googlesheets4)

### Applying QB specific mutations ----
qb2 <- as.data.frame(position2[[1]]) %>%
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

#### Getting in existing data
qb <- read_csv("C:/Users/mattw/Desktop/top_ten_qb_combined.csv") %>%
      rbind(qb2)


rrr <- qb[,6:39]
rrr2 <- apply(rrr, 2, function(x) {r <- ntile(x, 5)
                                   rd <- data.frame(cbind(x, r))
                                   colnames(rd) <- c("value", "quintile")
                                   rd2 <- rd %>% filter(quintile != 5) %>%
                                          group_by(quintile) %>%
                                          summarize(val = max(value)) %>%
                                          mutate(quintile = quintile * 20) %>%
                                          spread(key = quintile, value = val)
                                   return(rd2)
                                   })


get_quintiles <- function(x) {

  # Getting quintiles of variable
  r <- ntile(x, 5)
  # Binding with data df
  rd <- data.frame(cbind(x, r))
  colnames(rd) <- c("value", "quintile")

  # Removing 5th quintile (not needed for out levels)
  # getting max of remaining 4 which gives 20,40,60,80th
  # making into a 1 row for later rbind
  rd2 <- rd %>% filter(quintile != 5) %>%
    group_by(quintile) %>%
    summarize(val = max(value)) %>%
    mutate(quintile = quintile * 20) %>%
    spread(key = quintile, value = val)

  return(rd2)
}

# function to take in list and turn into 1 df
quintile_list_to_df <- function(d) {

  l <- lapply(c(1:length(d)), function(x,n) {cbind(n[[x]],d[[x]])}, names(d))
  l2 <- do.call("rbind",l)
  names(l2) <- c("stat", "20th", "40th", "60th", "80th")
  return(l2)
}

rrr2 <- apply(rrr, 2, function(x) get_quintiles(x))
rr3 <- quintile_list_to_df(rrr2)

# My "is the data meaningful" check. If the 40th percentile isn't above the median, not gaining any information
for(ii in 1:nrow(rr3)) {n <- rr3[ii,1]; med <- median(qb2[[n]], na.rm = T); if(rr3[ii,3] > med) {print(n)} else {rr3[ii,1] <- NA}}
rr4 <- filter(rr3, is.na(stat) == F)

# write_sheet(rr3, "https://docs.google.com/spreadsheets/d/1NOGl0Yo6BpOb2_jbxNxtZp8LZxyMbFlvuYn7OmEv5-Q/edit?usp=sharing", "qb2")


## Trying to look at a binomial regression
yy <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to7_combined.xlsx") %>%
      filter(proj_pos == "QB" & proj_week > 3)

## https://advstats.psychstat.org/book/factor/efa.php#:~:text=%20Exploratory%20factor%20analysis%20%201%20Preparing%20data.,factors%20is%20decided%2C%20we%20can%20conduct...%20More%20

cormat <- cor(qb[,c(6:39)])

uu <- psych::pca(cormat, nfactors = 5, n.obs = 40, scores = T)
uu2 <- abs(uu$loadings) > 0.7

tt = cbind(rr4, uu2)

###
### starting with the fill dataset ----

d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_4to7_combined.xlsx") %>%
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
      filter(proj_pos == "QB")
d2 <- select(d, colnames(qb))
d3 <- d2 %>% slice_max(order_by = prev_wk_fantasy_fdpt, n = 40) %>% mutate(top_ten = factor(1))
d4 <- d2 %>% slice_min(order_by = prev_wk_fantasy_fdpt, n = 77) %>% mutate(top_ten = factor(0))
d5 <- cbind(d3, d4)

for(ii in 3:39) {print(ii); qb_train[,ii][is.na(qb_train[,ii])] <- median(unlist(qb_train[,ii]), na.rm = T)}
for(ii in 3:39) {print(ii); qb_test[,ii][is.na(qb_test[,ii])] <- median(unlist(qb_test[,ii]), na.rm = T)}

#####################################################################
######### Trying to formalize the function with just QB for now -----
#####################################################################

# Reading in previous week's data to get the top 10
wk_num <- 10

get_top_ten <- function(wk_num) {

  # Read in last week's data
  lw <- readxl::read_xlsx(paste0("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_",(wk_num-1),"_2020.xlsx")) %>%
        select(-prev_wk_fantasy_fdpt) %>%
        filter(proj_pos == "QB")

  # Read in this week's data (allows us to get the top fd points)
  tw <- readxl::read_xlsx(paste0("C:/Users/mattw/Documents/ff_shiny_app/ff_app/data/all_data_wk_",wk_num,"_2020.xlsx")) %>%
        select(proj_player, prev_wk_fantasy_fdpt)
  
  d <- left_join(lw, tw, by = "proj_player")
}

prev_wk <- get_top_ten(wk_num)

#get_positions <- function(df) {

  # Making list of positions
  #position <- as.list(unique(df$proj_pos))

  # Looping over list to apply specific columns
  position2 <- lapply("QB", function(pos) {

    # Main switch function that returns the columns of interest
    cols <- switch(pos,
                   "QB" = c("pts_vs_g", "projected_own","proj_afpa_rk","line","total",
                            "implied_total","pts_vs_passing_att", "pts_vs_passing_yds", "pts_vs_passing_td",
                            "pts_vs_fantasy_per_game_fdpt","def_red_zone_td","def_red_zone_pct",
                            "def_dvoa", "def_pass_dvoa", "def_dline_pass_rank",
                            "def_dline_pass_adjusted_sack_rate","def_pass_qb_rating_allowed",
                            "def_pass_adj_net_yds_per_att", "off_oline_pass_adjusted_sack_rate",
                            "ytd_pass_yds_per_gm","ytd_pass_td","adv_passing_iay","pass_dyar",
                            "adv_passing_ontgt_per","adv_passing_bad_per",
                            "adv_passing_prss_per","pass_eyds","pass_yards","rush_eyds","rush_dyar","rush_yards",
                            "passing_twenty_att","passing_twenty_td"),
                   "RB" = c("pts_vs_g", "projected_own","proj_afpa_rk","line","total",
                            "implied_total", "ytd_rush_att", "pts_vs_fantasy_per_game_fdpt",
                            "pts_vs_rec_tgt","pts_vs_rec_yds","pts_vs_rec_td",
                            "pts_vs_rush_att","pts_vs_rush_yds","pts_vs_rush_td",
                            "def_rush_yds_per_att","def_red_zone_td","def_red_zone_pct",
                            "def_dvoa","def_rush_dvoa","off_rush_dvoa","def_dline_power_success",
                            "def_dline_adj_line_yards","def_dline_stuffed","def_dline_2nd_level_yards",
                            "def_dline_open_field_yards","off_oline_adj_line_yards","off_oline_power_success",
                            "off_oline_stuffed","off_oline_open_field_yards","off_oline_2nd_level_yards",
                            "ytd_rec_target"))

    # adding some identifying information and then selecting the specific column
    cols2 <- c("proj_pos","proj_player","prev_wk_fantasy_fdpt","proj_week", cols)

    top_ten2 <- top_ten[,cols2] %>% filter(proj_pos == pos)

  })

#}

### Applying QB specific mutations ----
qb2 <- as.data.frame(position2[[1]]) %>%
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


#First, reading in the full dataset and getting previous top 10 finishers
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to9_combined_vegas_fixes.xlsx") %>%
     filter(proj_week <= wk_num - 1) %>%
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
  filter(proj_pos == "QB" & proj_week > 3 & is.na(prev_wk_fantasy_fdpt) == F) %>%
  select(colnames(qb2))

# TODO: MERGE THE NEW TOP 10 DATA IN AND SAVE OUT
top <- lapply(as.list(unique(d$proj_week)), function(x) {
  d2 <- filter(d, proj_week == x) %>% slice_max(order_by = prev_wk_fantasy_fdpt)}








# Computing top 10 for each week and adding a one to that column
d2 <- d %>% slice_max(order_by = prev_wk_fantasy_fdpt, n = 40) %>% mutate(top_ten = factor(1))
d3 <- d %>% slice_min(order_by = prev_wk_fantasy_fdpt, n = 77) %>% mutate(top_ten = factor(0))
all_d <- rbind(d2, d3)
write.csv(all_d, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/qb_top_top_4to9.csv", row.names = F)
# For now, doing a median replace for NA's
# TODO - smarter NA replacement
for(ii in 3:39) {all_d[,ii][is.na(all_d[,ii])] <- median(unlist(all_d[,ii]), na.rm = T)}


# Training PCA Model analysis -----
all_d_wk <- filter(all_d, proj_week != wk_num)

pca <- prcomp(all_d_wk[,c(6:39)], scale = T, center = T)


screeplot(pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 8, col="blue", lty=5)
abline(h = 0.7806, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC10"),
       col=c("blue"), lty=5, cex=0.6)


library(factoextra)
fviz_pca_ind(pca, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = all_d_wk$top_ten,
             col.ind = "black",
             palette = "jco",
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Top 10 (1 = Yes)") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5))

fviz_pca_biplot(pca, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = all_d$top_ten,
             col.ind = "black",
             palette = "jco",
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Top 10 (1 = Yes)") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5))


pc1 <- abs(pca$rotation) %>% as.data.frame %>% rownames_to_column %>%
            select(rowname, PC1) %>% arrange(desc(PC1)) %>% head(5)
as.vector(pc1$rowname)

pc2 <- abs(pca$rotation) %>% as.data.frame %>% rownames_to_column %>%
       select(rowname, PC2) %>% arrange(desc(PC2)) %>% head(5)
as.vector(pc2$rowname)

# Working out the accuracy on the training data
pca_pred <- as.data.frame(predict(pca, all_d_wk)) %>% mutate(top_ten = all_d_wk$top_ten)
m <- glm(top_ten ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = pca_pred, family = "binomial")
summary(m)
pca_pred$binomial_pred <- predict.glm(m, pca_pred, type = "response")

for(ii in c(0.30,0.40,0.45,0.50,0.55,0.60,0.65)) {

  # Getting working version of df
  wd <- pca_pred

  # do accuracy calculation
  wd$outcome_bi = ifelse(wd$binomial_pred > ii, 1, 0)
  wd$tp = ifelse(wd$outcome_bi == 1 & wd$top_ten == 1,1,0)
  wd$fp = ifelse(wd$outcome_bi == 1 & wd$top_ten == 0,1,0)
  wd$fn = ifelse(wd$outcome_bi == 0 & wd$top_ten == 1,1,0)
  wd$tn = ifelse(wd$outcome_bi == 0 & wd$top_ten == 0,1,0)

  # Calculating Values
  accuracy <- (sum(wd$tp) + sum(wd$tn)) / nrow(wd)
  precision <- sum(wd$tp) / (sum(wd$tp) + sum(wd$fp))
  recall <- sum(wd$tp) / (sum(wd$tp) + sum(wd$fn))
  f1 <- 2 * ((precision * recall)/ (precision + recall))

  # Print result
  cat(paste0("Threshold of: ",ii,
             " = Accuracy: ", round(accuracy*100,2),
             " | Precision: ", round(precision*100,2),
             " | Recall: ", round(recall*100,2),
             " | F1: ", round(f1,2),
             " \n"))

}

# Testing data
test_data <- filter(all_d, proj_week == (wk_num-1))
pca_test <- as.data.frame(predict(pca, test_data)) %>% mutate(top_ten = test_data$top_ten)
pca_test$binomial_pred <- predict.glm(m, pca_test, type = "response")

# Calculating Accuracy
pca_test$outcome_bi = ifelse(pca_test$binomial_pred > 0.45, 1, 0)
#pca_test$accuracy = ifelse(pca_test$outcome_bi == as.numeric(as.character(pca_test$top_ten)),1,0)
pca_test$tp = ifelse(pca_test$outcome_bi == 1 & pca_test$top_ten == 1,1,0)
pca_test$fp = ifelse(pca_test$outcome_bi == 1 & pca_test$top_ten == 0,1,0)
pca_test$fn = ifelse(pca_test$outcome_bi == 0 & pca_test$top_ten == 1,1,0)
pca_test$tn = ifelse(pca_test$outcome_bi == 0 & pca_test$top_ten == 0,1,0)

# Calculating Values
(accuracy <- (sum(pca_test$tp) + sum(pca_test$tn)) / nrow(pca_test))
(precision <- sum(pca_test$tp) / (sum(pca_test$tp) + sum(pca_test$fp)))
(recall <- sum(pca_test$tp) / (sum(pca_test$tp) + sum(pca_test$fn)))
(f1 <- 2 * ((precision * recall)/ (precision + recall)))

# Calculate MSE
pca_test$sq_error <- (as.numeric(as.character(pca_test$top_ten)) - pca_test$binomial_pred)^2
mean(pca_test$sq_error)

# Creating a plot to see where we missed worst
errors <- cbind(test_data[,2], pca_test[,c(35,36,42)])


ggplot(data = errors, aes(x = binomial_pred, y = sq_error)) + 
  geom_point(shape = 21, size = 6, aes(fill = errors$top_ten)) +
  geom_label(label = errors$proj_player, 
             nudge_x = 0.01, nudge_y = 0.01) +
  theme_bw() + ggtitle("Week 9; MSE = 0.61")

























