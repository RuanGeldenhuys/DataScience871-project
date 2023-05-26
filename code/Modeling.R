#Modeling
n_features <- ncol(finalDF)-1
rf1 <- ranger(
    UpgradeInternet ~ .,
    data = finalDF,
    mtry = floor(sqrt(n_features)),
    respect.unordered.factors = "order",
    seed = 246,
    importance = 'impurity'
)
rf1
