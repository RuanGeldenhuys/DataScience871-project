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

``` r
masterDF <- read.csv('data/DeloitteMediaConsumptionSurvey.csv')
demographicDF <- masterDF %>% 
    select(c(2:17)) 
    
demographicDF <- demographicDF %>% 
    select(c(-3, -4,-15))

colnames(demographicDF) <- c("Age", "Gender", "Region", "Employment", "Race", 
                             "Children", "Child0_4", "Child5_9", "Child10_13", 
                             "Child14_18", "Child19_25", "Child26", "Income")

factor_cols = c(2:6, 13)
demographicDF <- demographicDF %>% 
    mutate_at(factor_cols, as.factor) %>% 
    mutate(ChildAge = case_when(
        Child0_4 == 'Yes' ~ '0-4',
        Child5_9 == 'Yes' ~ "5-9",
        Child10_13 == "Yes" ~ "10-13",
        Child14_18 == "Yes" ~ "14-18",
        Child19_25 == "Yes" ~ "19-25",
        Child26 == "Yes" ~ "26"
    )) %>% 
    mutate(ChildAge = as.factor(ChildAge)) %>% 
    select(-c(7:12))


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
