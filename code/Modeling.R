#Modeling
library(tictoc)

##Setting up training and testing set
set.seed(246)
indices <- createDataPartition(finalDF$UpgradeInternet, p = 0.8, list = FALSE)
train_set <- finalDF[indices,]
test_set <- finalDF[-indices,]
n_features <- ncol(finalDF)-1

##Fit Baseline Model
tic()
rf1 <- ranger(
    UpgradeInternet ~ .,
    data = train_set,
    mtry = floor(sqrt(n_features)),
    respect.unordered.factors = "order",
    seed = 246,
    importance = 'impurity',
    num.trees = n_features * 10
)
rf1
toc()

##Hyper parameter tuning
hyper_grid <- expand.grid(
    mtry            = floor(n_features * c(.05, 0.75, .10, 0.125, .15, .175, .20, .225, .25, .275, .30)),
    min.node.size   = c(1, 3, 5, 7, 10),
    replace         = c(TRUE, FALSE),
    sample.fraction = c(.5, .63, .8, 1),
    num.trees       = n_features* c(5,7,10,13,15,17,20),
    splitrule       = c('gini', 'extratrees'),
    predError       = NA,
    rmse            = NA
)
# tic()
# for(i in seq_len(nrow(hyper_grid))) {
#     # fit model for ith hyperparameter combination
#     fit <- ranger(
#         formula         = UpgradeInternet ~ .,
#         data            = train_set,
#         num.trees       = hyper_grid$num.trees[i],
#         mtry            = hyper_grid$mtry[i],
#         min.node.size   = hyper_grid$min.node.size[i],
#         replace         = hyper_grid$replace[i],               #commented out because
#         sample.fraction = hyper_grid$sample.fraction[i],        it takes a while
#         splitrule       = hyper_grid$splitrule[i],
#         verbose         = FALSE,
#         seed            = 246,
#         respect.unordered.factors = 'order',
#     )
#     # export OOB error
#     hyper_grid$predError[i] <- fit$prediction.error
#     hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
# }
# toc()
#
# hyper_grid %>%
#     arrange(rmse) %>%
#     head(10)


## Fit best model from hyperparameter tune
bestmod1 <- ranger(
    UpgradeInternet ~ .,
    data = train_set,
    mtry = 11,
    min.node.size = 10,
    replace = FALSE,
    sample.fraction = 0.63,
    num.trees = 1000,
    seed = 246,
    importance = 'impurity',
    respect.unordered.factors = 'order'
)
bestmod1

#Tree plot
nt <- seq(1,5000,10)
oob_mse <- vector("numeric", length(nt))

# for (i in 1:length(nt)) {
#     rf <- ranger(
#         UpgradeInternet ~ .,
#         data = train_set,
#         mtry = 11,
#         min.node.size = 10,
#         replace = FALSE,
#         sample.fraction = 0.63,       #Commented out because it takes a while
#         num.trees = nt[i],
#         seed = 246,
#         importance = 'impurity',
#         respect.unordered.factors = 'order',
#         write.forest = FALSE
#     )
#
#     oob_mse[i] <- rf$prediction.error
# }

plot(x=nt, y=oob_mse, type="l")

treeDF <- as.data.frame(cbind(nt,oob_mse))
treePlot <- treeDF %>%
    ggplot(aes(x=nt, y=oob_mse))+
    geom_line(color = "red")+
    theme_pubr() +
    labs(x='Number of Trees', y="Out-Of-Bag MSE", title = "Number of Trees vs MSE")
treePlot

##Ten most important predictors
VarImp <- importance(bestmod1)
VarImp <- sort(VarImp, decreasing = TRUE)
VarImp10 <- head(VarImp,10)
VarImp10_df <- stack(VarImp10)
ggplot(VarImp10_df, aes(x = values, y = reorder(ind, values))) +
    geom_segment(aes(x = 0, xend = values, y = ind, yend = ind), linetype = "solid", color = "grey50") +
    geom_point(color = "darkred", size = 3) +
    labs(title = "Variable Importance", x = "Importance Score", y = "") +
    theme_minimal()




##Confusion matrices
predictions <- predict(bestmod1, data = train_set)$predictions
conMat_train <- confusionMatrix(predictions, train_set$UpgradeInternet)
confusion_table_train <- conMat_train$table

predictionsTest <- predict(bestmod1, data = test_set)$predictions
conMat_test <- confusionMatrix(predictionsTest, data = test_set$UpgradeInternet)
confusion_table_test <- conMat_test$table


