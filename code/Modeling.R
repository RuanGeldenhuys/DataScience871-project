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

##Tree plot
treeDF_import <- read.csv('data/treeDF.csv')
treePlot <- treeDF_import %>%
    ggplot(aes(x=nt, y=oob_mse))+
    geom_line(color = "red")+
    theme_minimal() +
    labs(x='Number of Trees', y="Out-Of-Bag MSE", title = "Number of Trees vs MSE")
treePlot

##Hyper parameter tuning
Top10_models <- read.csv("data/parametertuning_results.csv")

## Fit best model from hyperparameter tune
bestmod1 <- ranger(
    UpgradeInternet ~ .,
    data = train_set,
    mtry = 8,
    min.node.size = 7,
    replace = TRUE,
    sample.fraction = 1,
    splitrule = 'gini',
    num.trees = 500,
    seed = 246,
    importance = 'impurity',
    respect.unordered.factors = 'order'
)
bestmod1


##Ten most important predictors
VarImp <- importance(bestmod1)
VarImp <- sort(VarImp, decreasing = TRUE)
VarImp10 <- head(VarImp,10)
VarImp10_df <- stack(VarImp10)

new_labels <- c(
    "Age" = "Age",
    "Employment" = "Employment Status",
    "ChildAge" = "Child Age",
    "MovieTime_Smartphone" = "Movies on Smartphone",
    "yes_tech" = "Devices owned",
    "SportTime_Smartphone" = "Sport on Smartphone",
    "SportTime_Tablet" = "Sport on Tablet",
    "Income" = "Income",
    "MovieTime_Computer" = "Movies on Computer",
    "tvTime_Smartphone" = "TV Shows on Smartphone"
)

ImportancePlot <- ggplot(VarImp10_df, aes(x = values, y = fct_reorder(ind, values))) +
    geom_segment(aes(x=0, xend = values, yend = ind), linetype = "solid", color = "grey50") +
    geom_point(color = "red", size = 3) +
    labs(title = "Feature Importance", x = "Importance Score", y = "") +
    theme_minimal()+
    scale_y_discrete(labels = new_labels)
ImportancePlot

##Confusion matrices
predictions <- predict(bestmod1, data = train_set)$predictions
conMat_train <- confusionMatrix(predictions, train_set$UpgradeInternet)
confusion_table_train <- conMat_train$table

predictionsTest <- predict(bestmod1, data = test_set)$predictions
conMat_test <- confusionMatrix(predictionsTest, data = test_set$UpgradeInternet)
confusion_table_test <- conMat_test$table

testtable <- confusionMatrix(test_set$UpgradeInternet, predictionsTest)
testtable$table
