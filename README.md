Data Science 871 project
================

## Media Consumption and Willingness to upgrade internet package.

The purpose of this project is to use survey data in order to determine
which factors are important predictors of whether individuals would be
willing to upgrade their internet packages.

Code files:

## Data loading and cleaning

The data set contains survey responses from 2131 individuals regarding
demographic factors (age, sex, race, ect.), the types of apps they use,
time spent on different devices, preferred type of media, and other
issues regarding media consumption.

The first step is converting factor variables to features and converting
answers to a question that was reported in multiple columns to one
column

``` r
masterDF <- read.csv('data/DeloitteMediaConsumptionSurvey.csv')
head(demographicDF)
```

    ##   Age Gender    Region                      Employment
    ## 1  36   Male     South Employed full-time or part-time
    ## 2  26 Female Northeast Employed full-time or part-time
    ## 3  32 Female Northeast Employed full-time or part-time
    ## 4  25 Female      West Employed full-time or part-time
    ## 5  28   Male   Midwest                         Student
    ## 6  33   Male Northeast                   Self-employed
    ##                                Race Children             Income ChildAge
    ## 1 White or Caucasian (Non-Hispanic)      Yes $50,000 to $99,999    10-13
    ## 2 White or Caucasian (Non-Hispanic)      Yes $50,000 to $99,999      0-4
    ## 3 White or Caucasian (Non-Hispanic)      Yes  Less than $29,999      0-4
    ## 4 White or Caucasian (Non-Hispanic)      Yes  Less than $29,999      0-4
    ## 5 White or Caucasian (Non-Hispanic)      Yes $50,000 to $99,999      0-4
    ## 6 White or Caucasian (Non-Hispanic)      Yes $50,000 to $99,999      5-9
