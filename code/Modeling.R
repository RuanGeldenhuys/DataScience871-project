#Modeling
library(tictoc)
set.seed(246)
indices <- createDataPartition(finalDF$UpgradeInternet, p = 0.8, list = FALSE)

train_set <- finalDF[indices,]
test_set <- finalDF[-indices,]
n_features <- ncol(finalDF)-1
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

#Hyper parameter tuning
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
#         replace         = hyper_grid$replace[i],
#         sample.fraction = hyper_grid$sample.fraction[i],
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

bestmod1 <- ranger(
    UpgradeInternet ~ .,
    data = train_set,
    mtry = 11,
    min.node.size = 10,
    replace = FALSE,
    sample.fraction = 0.63,
    num.trees = 616,
    seed = 246,
    importance = 'impurity',
    respect.unordered.factors = 'order'
)
bestmod1
VarImp <- importance(bestmod1)
VarImp <- sort(VarImp, decreasing = TRUE)
VarImp10 <- head(VarImp,10)
