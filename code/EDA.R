#Exploratory Data Analysis

#Age
ageHist <- finalDF %>%
    ggplot(aes(x=Age))+
    geom_histogram(bins = 37,fill='cornflowerblue', color = 'white', alpha=0.9)+
    ggtitle("Distribution of Age")+
    ylab("Count")
ageHist

#Gender
genderBar <- finalDF %>%
    ggplot(aes(x=reorder(Gender,Gender,length)))+
    geom_bar(fill='cornflowerblue', color = 'white', alpha=0.9)+
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







