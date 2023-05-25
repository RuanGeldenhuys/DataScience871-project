Data Science 871 project
================

## Media Consumption and Willingness to upgrade internet package.

The purpose of this project is to use survey data in order to determine
which factors are important predictors of whether individuals would be
willing to upgrade their internet packages. All code and functions are
stored under the “code” folder. This readme is solely for visualization
of data and results.

Code files:

## Data loading and cleaning

The data set contains survey responses from 2131 individuals regarding
demographic factors (age, sex, race, ect.), the types of apps they use,
time spent on different devices, preferred type of media, and other
issues regarding media consumption. I restrict this data set to include
only responses from individuals that have internet access.

The features can be broadly broken down into 6 categories, namely
demographic factors, technology owned by individuals, device usage, app
usage, user subscriptions and lastly, what individuals’ preferred form
of entertainment are.

``` r
NAs <- 0
for (i in 1:nrow(appDF)) {
    if (any(appDF[i,] == "#NULL!") == TRUE) {  #Checking how many users did not answer this question
        NAs <- NAs + 1
    }
}
NAs
```

    ## [1] 412

Under closer inspection it would appear that a total of 412 survey
respondents did not answer the question regarding app usage. Thus, for
now, I exclude this feature from further analysis.

\##Target Variable The target variable is turned into a factor variable
indicating a 1 if they are willing to upgrade their internet package and
a 0 if they are not.

``` r
Q29 <- masterDF[,151]

UpgradeInternet <- ifelse(Q29 == "I am not willing to pay more for faster download speeds as my current speed is sufficient for my needs" |
                  Q29 == "I prefer faster speed but I am unwilling to pay more than I already do", 0, 1)
UpgradeInternet <- as.factor(UpgradeInternet)
head(UpgradeInternet)
```

    ## [1] 1 1 1 0 1 1
    ## Levels: 0 1

``` r
summary(UpgradeInternet)
```

    ##   0   1 
    ## 814 744
