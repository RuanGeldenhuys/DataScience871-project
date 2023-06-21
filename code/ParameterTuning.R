#Trees
nt <- seq(1,2000,10)
oob_mse <- vector("numeric", length(nt))

for (i in 1:length(nt)) {
    rf <- ranger(
        UpgradeInternet ~ .,
        data = train_set,
        mtry = floor(sqrt(n_features)),
        respect.unordered.factors = "order",
        seed = 246,
        importance = 'impurity',
        num.trees = nt[i]
    )

    oob_mse[i] <- rf$prediction.error
}

treeDF <- as.data.frame(cbind(nt,oob_mse))
write.csv(treeDF, "data/treeDF.csv")


#Hyperparameter Tuning
hyper_grid <- expand.grid(
    mtry            = floor(n_features * c(.05, 0.75, .10, 0.125, .15, .175, .20, .225, .25, .275, .30)),
    min.node.size   = c(1, 3, 5, 7, 10),
    replace         = c(TRUE, FALSE),
    sample.fraction = c(.5, .63, .8, 1),
    splitrule       = c('gini', 'extratrees'),
    predError       = NA,
    rmse            = NA
)
tic()
for(i in seq_len(nrow(hyper_grid))) {
    # fit model for ith hyperparameter combination
    fit <- ranger(
        formula         = UpgradeInternet ~ .,
        data            = train_set,
        mtry            = hyper_grid$mtry[i],
        num.trees       = 500,
        min.node.size   = hyper_grid$min.node.size[i],
        replace         = hyper_grid$replace[i],
        sample.fraction = hyper_grid$sample.fraction[i],
        splitrule       = hyper_grid$splitrule[i],
        verbose         = FALSE,
        seed            = 246,
        respect.unordered.factors = 'order',
    )
    # export OOB error
    hyper_grid$predError[i] <- fit$prediction.error
    hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}
toc()

Top10_grid = hyper_grid %>%
    arrange(rmse) %>%
    head(10)
Top10_grid <- as.data.frame(Top10_grid)
write.csv(Top10_grid, "data/parametertuning_results.csv")