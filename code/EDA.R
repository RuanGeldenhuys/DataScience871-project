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




