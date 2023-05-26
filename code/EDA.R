#Exploratory Data Analysis

#Age
ageHist <- finalDF %>%
    ggplot(aes(x=Age))+
    geom_histogram(fill='lightblue', color = 'white', alpha=0.9)+
    ggtitle("Distribution of Age")
ageHist

#Gender
genderBar <- finalDF %>%
    ggplot(aes(x=Gender))+
    geom_bar()+
    coord_flip()
genderBar


