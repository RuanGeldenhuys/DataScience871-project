##Data Cleaning and Feature Engineering
masterDF <- initialDF[initialDF[,130] == "Yes",]
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
    mutate(ChildAge = ifelse(is.na(ChildAge), 'None', ChildAge)) %>%
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
appDF <- masterDF[,c(92:126)]
headers <- colnames(appDF)
string <- str_extract(headers, "(?<=\\.\\.).*" )
string <- str_extract(string, "(?<=\\.\\.).*" )
string <- str_extract(string, "(?<=\\.\\.).*" )
string <- str_extract(string, "(?<=\\.\\.).*" )
colnames(appDF) <-  string

counter <- 0
for (i in 1:nrow(appDF)) {
    if (any(appDF[i,] == "#NULL!") == TRUE) {  #Checking how many users did not answer this question
        counter <- counter + 1
    }
}
counter

#Subscriptions
subDF <- masterDF[,c(129:138)]
subHeader <- ExtractColumnHeaders(colnames(subDF))
colnames(subDF) <- subHeader
subDF <- data.frame(lapply(subDF, factor))
subDF <- subDF[,-2]

YesString <- 'Yes'
yes_counts2 <- apply(subDF, 1, function(row) sum(row == YesString))
subDF <- cbind(subDF, yes_counts2)

#Subscription preferences
subprefDF <- masterDF[,c(141:150)]
subprefHeader <- paste('pref', subHeader, sep = '_')
colnames(subprefDF)<-subprefHeader
subprefDF <- subprefDF %>%
    replace(., subprefDF == '#NULL!', 0)
subprefDF <- subprefDF[,-2]
subprefDF <- as.data.frame(lapply(subprefDF, as.numeric))

#Entertainment activities rankings
activityDF <- masterDF[,c(152:161)]
colnames(activityDF) <- c('LiveEvents', 'Movies', 'TV', 'Music', 'Books',
                          'Magazines', 'Newspapers', 'Radio', 'Video Games',
                          'InternetSocial')
activityDF <- activityDF %>%
    replace(., activityDF == '#NULL!', 0)
activityDF <- as.data.frame(lapply(activityDF, as.numeric))


##Target variable
Q29 <- masterDF[,151]

UpgradeInternet <- ifelse(Q29 == "I am not willing to pay more for faster download speeds as my current speed is sufficient for my needs" |
                  Q29 == "I prefer faster speed but I am unwilling to pay more than I already do", 0, 1)
UpgradeInternet <- as.factor(UpgradeInternet)

finalDF <- cbind(UpgradeInternet, demographicDF, techownedDF, techprefDF,
                 timespentDF, subDF, subprefDF, activityDF)
colnames(finalDF)[30] <- "yes_tech"
colnames(finalDF)[71] <- "yes_subs"
