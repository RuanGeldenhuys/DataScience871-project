#Exploratory Data Analysis

#Age
ageHist <- finalDF %>%
    ggplot(aes(x=Age))+
    geom_histogram(bins = 37,fill='bisque3', color = 'white', alpha=0.9)+
    ggtitle("Distribution of Age")+
    ylab("Count")
ageHist

#Gender
genderBar <- finalDF %>%
    ggplot(aes(x=reorder(Gender,Gender,length)))+
    geom_bar(fill='bisque3', color = 'white', alpha=0.9)+
    coord_flip() +
    ggtitle("Gender breakdown")+
    xlab('Gender')+
    ylab('Count')+
    geom_text(
        aes(label = after_stat(count)),
        stat = "count",
        hjust = 1.5,
        colour = 'black',
        size = 7
    )
genderBar

#Income distribution
finalDF$Income <- factor(finalDF$Income,
                         levels = c('Do not know', 'Less than $29,999',
                                    '$30,000 to $49,999', '$50,000 to $99,999',
                                    '$100,000 to $299,999', 'More than $300,000'
                                    ))
incomeBar <- finalDF %>%
    ggplot(aes(x=Income)) +
    geom_bar(fill='bisque3', color = 'white', alpha=0.9)+
    ggtitle('Income Distribution')+
    ylab('Count')+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
incomeBar

#Ethnicity breakdown
levels(finalDF$Race) <- c("African American", "Caribbean",
                          "East/Southeast Asian", "Hispanic",
                          "Middle Eastern", "Multiracial", "Pacific Islander",
                          "Not Listed", "South Asian", "White or Caucasian")
ethBar <- finalDF %>%
    ggplot(aes(x=Race))+
    geom_bar(fill='bisque3', color = 'white', alpha=0.9)+
    ggtitle('Ethnicity Breakdown')+
    ylab('Count')+
    coord_flip()
ethBar

demogPlot <- ggarrange(ageHist, genderBar, incomeBar, ethBar, ncol = 2, nrow = 2)
demogPlot

#Equipment ownership
tech2 <- techownedDF %>%
    select(c(1:19))

longtech <- tech2 %>%
    pivot_longer(
        cols = everything(),
        names_to = "Technology",
        values_to = "Yes_No"
    ) %>%
    group_by(Technology, Yes_No) %>%
    summarize(Num_People = n())

library(viridis)
techBar <- ggplot(longtech, aes(fill=Yes_No, y=Num_People, x=Technology)) +
    geom_bar(position="dodge", stat="identity")+
    ggtitle("Technology Ownership")+
    ylab('Count')+
    scale_fill_viridis(discrete = TRUE)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
techBar

#Watch time
watchtimeDF <- cbind(movieDF, tvshowsDF, sportDF)

longWatch <- watchtimeDF %>%
    pivot_longer(
        cols = everything(),
        names_to = "Appliance",
        values_to = "Hours"
    ) %>%
    separate(Appliance, into = c("Type", "Appliance"), sep = "_") %>%
    group_by(Type, Appliance) %>%
    summarise(avgwatch = mean(Hours))

watchBar <- ggplot(longWatch, aes(fill=Appliance, y=avgwatch, x=Type))+
    geom_bar(position = 'fill', stat = 'identity')+
    scale_fill_viridis(discrete = T)+
    ylab("Average % Watch Time")+
    ggtitle("Entertainment Watch Time broken down Appliance")+
    scale_x_discrete(labels = c("Movies", "Sport", "TV Shows"))
watchBar

#Subscriptions
longSub <- subDF %>%
    pivot_longer(
        cols = everything(),
        names_to = "Subscriptions",
        values_to = "Yes_No"
    ) %>%
    group_by(Subscriptions, Yes_No) %>%
    summarize(Num_People = n())

subBar <- ggplot(longSub, aes(fill=Yes_No, y=Num_People, x=Subscriptions)) +
    geom_bar(position="dodge", stat="identity")+
    ggtitle("Subscription Ownership")+
    ylab('Count')+
    scale_fill_viridis(discrete = TRUE)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
subBar


##Bivariate
#Demographics vs Willingness to pay

#Age
age_BIdf <- finalDF %>%
    select(c(1,2))

ageDensityPlot <- ggplot(age_BIdf, aes(x = Age, fill = factor(UpgradeInternet))) +
    geom_density(alpha = 0.5) +
    labs(x = "Age", y = "Density", fill = "Upgrade Internet") +
    scale_fill_viridis(discrete = TRUE, labels = c("No", "Yes")) +
    ggtitle("Age vs Upgrading Internet")+
    theme_pubr()
ageDensityPlot

#Techowned
techAmount_BIdf <- finalDF %>%
    select(c(1,30))

techDensityPlot <- ggplot(techAmount_BIdf, aes(x = yes_counts, fill = factor(UpgradeInternet))) +
    geom_density(alpha = 0.5) +
    labs(x = "Amount of devices owned", y = "Density", fill = "Upgrade Internet") +
    scale_fill_viridis(discrete = TRUE, labels = c("No", "Yes")) +
    ggtitle("Technology ownership vs Upgrading Internet") +
    theme_pubr()
techDensityPlot



#Wacthtime
watchtime_BIdf <- cbind(finalDF[,1], watchtimeDF)
colnames(watchtime_BIdf)[1] <- "UpgradeInternet"
movie_BIdf <- watchtime_BIdf %>%
    select(c(1:5))
tv_BIdf <- watchtime_BIdf %>%
    select(c(1,6:9))
sport_BIdf <- watchtime_BIdf %>%
    select(c(1,10:13))


plotViolins <- function(dat, title){
    df_long <- reshape2::melt(dat, id.vars = "UpgradeInternet")

    # Create violin plots with overlapping categories based on UpgradeInternet
    watchtimeViolins <- ggplot(df_long, aes(x = variable, y = value, fill = factor(UpgradeInternet))) +
        geom_violin(scale = "width", trim = FALSE, position = "identity", alpha = 0.5) +
        scale_fill_viridis(discrete = TRUE, labels = c("No", "Yes")) +
        labs(x = "Variable", y = "Value", fill = "Upgrade Internet")+
        ggtitle(title) +
        scale_x_discrete(labels = c("Smartphone", "Tablet", "Computer", "TV"))+
        theme_pubr()
    return(watchtimeViolins)
}




