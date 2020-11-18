library(randomForest)

train <- filter(processed_data, proj_week <= wk_num - 2)
test <- filter(processed_data, proj_week == wk_num - 1)

formula <- "top_ten ~ "
for(n in colnames(processed_data)[c(6:36,38:40)]) {if(n != 'DVOA_Diff') {formula <- paste0(formula,n," + ")} else {formula <- paste0(formula, n)}}
formula <- as.formula(formula)

classifier <- randomForest(formula,
                           data = train,
                           ntree = 1000,
                           mtry = 4,
                           sampsize = 25,
                           importance = T)

## Getting metrics on training data
training_pred <- predict(classifier)
train$pred <- training_pred

train$tp = ifelse(train$pred == 1 & train$top_ten == 1,1,0)
train$fp = ifelse(train$pred == 1 & train$top_ten == 0,1,0)
train$fn = ifelse(train$pred == 0 & train$top_ten == 1,1,0)
train$tn = ifelse(train$pred == 0 & train$top_ten == 0,1,0)

# Calculating Values
(accuracy <- (sum(train$tp) + sum(train$tn)) / nrow(train))
(precision <- sum(train$tp) / (sum(train$tp) + sum(train$fp)))
(recall <- sum(train$tp) / (sum(train$tp) + sum(train$fn)))
(f1 <- 2 * ((precision * recall)/ (precision + recall)))

## Test dataset testing ----
test_pred <- predict(classifier, newdata = test[,c(6:36,38:40)])
test$pred <- test_pred

test$tp = ifelse(test$pred == 1 & test$top_ten == 1,1,0)
test$fp = ifelse(test$pred == 1 & test$top_ten == 0,1,0)
test$fn = ifelse(test$pred == 0 & test$top_ten == 1,1,0)
test$tn = ifelse(test$pred == 0 & test$top_ten == 0,1,0)

# Calculating Values
(accuracy <- (sum(test$tp) + sum(test$tn)) / nrow(test))
(precision <- sum(test$tp) / (sum(test$tp) + sum(test$fp)))
(recall <- sum(test$tp) / (sum(test$tp) + sum(test$fn)))
(f1 <- 2 * ((precision * recall)/ (precision + recall)))



