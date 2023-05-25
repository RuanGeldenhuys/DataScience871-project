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

#Tech owned
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

#Tech preferences
techprefDF <- masterDF[,c(60:78)]
prefnames <- paste('pref', colnames(techownedDF), sep = '_')
prefnames <- prefnames[-c(20:21)]
colnames(techprefDF) <- prefnames               #This code sets up the ranking df
techprefDF <- techprefDF %>%                    #1,2,3 indicate position - 0 indicates no rank
    replace(., techprefDF == '#NULL!', 0)
techprefDF <- as.data.frame(lapply(techprefDF, as.numeric))


#Time spent on each activity each device
movieDF <- masterDF[,c(80:83)] #Extract the columns indicating time spent for each on activity on each device
colnames(movieDF) <- c('MovieTime_Smartphone', 'MovieTime_Tablet', 'MovieTime_Computer', 'MovieTime_TV')

sportDF <- masterDF[,c(84:87)]
colnames(sportDF) <- c('SportTime_Smartphone', 'SportTime_Tablet', 'SportTime_Computer', 'SportTime_TV')

tvshowsDF <- masterDF[,c(88:91)]
colnames(tvshowsDF) <- c('tvTime_Smartphone', 'tvTime_Tablet', 'tvTime_Computer', 'tvTime_TV')

timespentDF <- cbind(movieDF, sportDF, tvshowsDF)

#Next we do app usage
#Subscriptions
#Entertainment activities

