#Modeling
set.seed(246)
indices <- createDataPartition(finalDF$UpgradeInternet, p = 0.8, list = FALSE)

train_set <- finalDF[indices,]
test_set <- finalDF[-indices,]
n_features <- ncol(finalDF)-1
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

#Hyper parameter tuning
hyper_grid <- expand.grid(
    mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
    min.node.size = c(1, 3, 5, 10),
    replace = c(TRUE, FALSE),
    sample.fraction = c(.5, .63, .8),
    num.trees = n_features* c(5,10,15,20),
    predError = NA,
    rmse = NA
)

for(i in seq_len(nrow(hyper_grid))) {
    # fit model for ith hyperparameter combination
    fit <- ranger(
        formula         = UpgradeInternet ~ .,
        data            = train_set,
        num.trees       = hyper_grid$num.trees[i],
        mtry            = hyper_grid$mtry[i],
        min.node.size   = hyper_grid$min.node.size[i],
        replace         = hyper_grid$replace[i],
        sample.fraction = hyper_grid$sample.fraction[i],
        verbose         = FALSE,
        seed            = 123,
        respect.unordered.factors = 'order',
    )
    # export OOB error
    hyper_grid$predError <- fit$prediction.error
    hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

hyper_grid %>%
    arrange(rmse) %>%
    head(10)