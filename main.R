library(pwr)

data <- read.csv('project_data.csv', header = TRUE)

west_data <- subset(data, Region == "West")
asia_data <- subset(data, Region == "Asia")
africa_data <- subset(data, Region == "Africa")
south_america_data <- subset(data, Region == "SouthAmerica")

west_data_mean <- aggregate(STEM_choices ~ Region + Land, data = west_data, FUN = mean)
asia_data_mean <- aggregate(STEM_choices ~ Region + Land, data = asia_data, FUN = mean)
africa_data_mean <- aggregate(STEM_choices ~ Region + Land, data = africa_data, FUN = mean)
south_america_data_mean <- aggregate(STEM_choices ~ Region + Land, data = south_america_data, FUN = mean)
all_means <- rbind(west_data_mean, asia_data_mean, africa_data_mean, south_america_data_mean)

boxplot(west_data$STEM_choices, asia_data$STEM_choices, africa_data$STEM_choices, south_america_data$STEM_choices,
        names=c("West", "Asia", "Africa", "South America"),
        xlab="Regions", ylab="STEM choices", main='Boxplot of STEM choices in the regions')

boxplot(west_data_mean$STEM_choices, asia_data_mean$STEM_choices, africa_data_mean$STEM_choices, south_america_data_mean$STEM_choices,
        names=c("West", "Asia", "Africa", "South America"),
        xlab="Regions", ylab="STEM mean choices", main='Boxplot of mean STEM choices in countries in the regions')

hist(west_data_mean$STEM_choices, xlab='Mean STEM choices in cuontries', main='Histogram over West')
hist(asia_data_mean$STEM_choices, xlab='Mean STEM choices in cuontries', main='Histogram over Asia')
hist(africa_data_mean$STEM_choices, xlab='Mean STEM choices in cuontries', main='Histogram over Africa')
hist(south_america_data_mean$STEM_choices, xlab='Mean STEM choices in cuontries', main='Histogram over South America')

shapiro.test(west_data_mean$STEM_choices)
shapiro.test(asia_data_mean$STEM_choices)
shapiro.test(africa_data_mean$STEM_choices)
shapiro.test(south_america_data_mean$STEM_choices)

bartlett.test(list(west_data_mean$STEM_choices, asia_data_mean$STEM_choices, africa_data_mean$STEM_choices, south_america_data_mean$STEM_choices))


pwr.anova.test(k = 4,
               n = NULL,
               f = 0.5,
               sig.level = 0.05,
               power = 0.8)

anova_result <- aov(STEM_choices ~ Region, data = all_means)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
