##Data Cleaning and Feature Engineering

#Demographic features
demographicDF <- masterDF %>%
    select(c(2:17))                 #Select relevant demographic features
demographicDF <- demographicDF %>%
    select(c(-3, -4,-15))

colnames(demographicDF) <- c("Age", "Gender", "Region", "Employment", "Race",
                             "Children", "Child0_4", "Child5_9", "Child10_13",
                             "Child14_18", "Child19_25", "Child26", "Income")

factor_cols = c(2:6, 13)
demographicDF <- demographicDF %>%
    mutate_at(factor_cols, as.factor) %>%       #Convert all variables to factors
    mutate(ChildAge = case_when(
        Child0_4 == 'Yes' ~ '0-4',
        Child5_9 == 'Yes' ~ "5-9",
        Child10_13 == "Yes" ~ "10-13",
        Child14_18 == "Yes" ~ "14-18",           #Convert child age variables to one factor feature
        Child19_25 == "Yes" ~ "19-25",
        Child26 == "Yes" ~ "26"
    )) %>%
    mutate(ChildAge = as.factor(ChildAge)) %>%
    select(-c(7:12))

#Media features
techownedDF <- masterDF %>%
    select(c(18:37))

newHeaders <- ExtractColumnHeaders(colnames(techownedDF)) # Call the cleaning function

colnames(techownedDF) <- newHeaders
colnames(techownedDF)[3]<- "StreamingBox"
colnames(techownedDF)[4]<- "StreamingFob"
colnames(techownedDF)[9]<- "Router"

techownedDF <- data.frame(lapply(techownedDF, factor))

YesString <- 'Yes'
yes_counts <- apply(techownedDF, 1, function(row) sum(row == YesString))
techownedDF <- cbind(techownedDF, yes_counts)






