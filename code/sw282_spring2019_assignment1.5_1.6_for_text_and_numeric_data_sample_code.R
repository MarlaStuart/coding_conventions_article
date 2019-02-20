# FOR DATA Assignment 1, 5. and 6.: Data Exploration
# Univariate and bivariate tables and visualizations
# 1/30/19
# Marla Stuart

# Research question
# What are the structural and social determinants of health disparities?
# Data: 51 tables of health data 

################################################################################
################################################################################
# PREPARE ENVIRONMENT #########################################################
# Clear the environment
rm(list=ls())

# Load packages
# for missingness function
library(matrixStats)
# CommpareGroups
library(compareGroups)

# Create personal functions

# My table -- so I don't have to always type useNA="always"
# But this only works for a single variable -- not for a cross table with 2 variables
mytable <- function(x) {
  table(x, useNA = "always")
}

# Missingness
missingness <- function(table) {
  na_count <- as.data.frame(sapply(table, function(y) sum(is.na(y))))
  names(na_count)[1] <- "number"
  na_count$percent <- round((na_count$number / nrow(table) * 100), digits = 2)
  na_count <- na_count[order(-na_count$percent), ]
  print(na_count)
}

# set the working directory
setwd("C:/Users/marlastuart/Dropbox/SW282/Spring_2019/module_data_exploration")

################################################################################
################################################################################
# LOAD DATA ###################################################################
data <- read.csv("master.csv")
head(data)
class(data)
str(data)
names(data)
data$X <- NULL

################################################################################
################################################################################
# ASSIGNMENT 1: 5.	Produce univariate and bivariate statistics and visualizations.

# Fix some of the variables to be more readable for tables and plots
# reorder metro long
mytable(data$OMBcode2013)
data$OMBcode2013 <- factor(data$OMBcode2013, levels = c("Large central metro",
                                                        "Large fringe metro",
                                                        "Medium metro",
                                                        "Small metro",
                                                        "Micropolitan",
                                                        "Noncore"))
data$OMBcode2013 <- factor(data$OMBcode2013, labels = c("Central", "Fringe", 
                                                        "Medium", "Small", 
                                                        "Micro", "NonCore"))
mytable(data$OMBcode2013)

# shorten labels
mytable(data$shortage_primary_care)
mytable(data$shortage_dental)
mytable(data$shortage_mental_health)
data$shortage_primary_care <- factor(data$shortage_primary_care, labels = c("None of County",
                                                                            "Part of County",
                                                                            "Whole County"))
data$shortage_dental <- factor(data$shortage_dental, labels = c("None of County",
                                                                "Part of County",
                                                                "Whole County"))
data$shortage_mental_health <- factor(data$shortage_mental_health, labels = c("None of County",
                                                                              "Part of County",
                                                                              "Whole County"))
mytable(data$shortage_primary_care)
mytable(data$shortage_dental)
mytable(data$shortage_mental_health)

# First, statitistics
# data by DV
names(data)
hist(data$life_expectancy)
summary(data$life_expectancy)
# Create a dichotomous/binary/dummy/catecorical variable
data$life_long <- 0
data$life_long[data$life_expectancy > 79.49] <- 1
summary(data$life_long)
mytable(data$life_long)
780/3142

# Create bivariate table by longevity
names(data)
groups_table <- compareGroups(life_long ~ .-fips -county_name -state_abbr,
                              max.ylev = 55, data = data, bivar = T, byrow = T)

groups_table <- createTable(groups_table,hide.no = c("no"), type =  1, show.all = T)       
groups_table
plot(groups_table)
export2word(groups_table, file = "groups_table.doc")

# remove HIV because large missing and not sig
missingness(data)
data$hiv_p <- NULL

# Second, visualizations
# Plot variables by concept and life expectancy

# concept = community
# include county, state, regions, metro, age, gender, desert pop

# Make small multiples
# Set up the plot window
dev.off() 
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
pdf("community1.pdf", width = 11, height = 8.5)
par(mfrow = c(3,3)) 

# level (3) # of counties
mytable(data$state_abbr)
plot(reorder(data$state_abbr, data$state_abbr, FUN = length), 
     main = "Counties in State", xaxt="n", cex.main = 2)
mytable(data$region_9)
plot(reorder(data$region_9, data$region_9, FUN = length),  
     main = "Counties in Region9", xaxt="n", cex.main = 2)
mytable(data$region_4)
plot(reorder(data$region_4, data$region_4, FUN = length),  
     main = "Counties in Region4", xaxt="n", cex.main = 2)

# level (3) population
county_state <- aggregate(data$state_abbr, by = list(data$state_abbr), FUN = length)
names(county_state) <- c("state_abbr", "counties_n")
pop_state <- aggregate(data$total_population, by = list(data$state_abbr), FUN = sum)
names(pop_state) <- c("state_abbr", "total_population")
county_state <- merge(county_state, pop_state)
county_state <- county_state[order(county_state$counties_n),] 
head(county_state)
rm(pop_state)

county_region_9 <- aggregate(data$region_9, by = list(data$region_9), FUN = length)
names(county_region_9) <- c("region_9", "counties_n")
pop_region_9 <- aggregate(data$total_population, by = list(data$region_9), FUN = sum)
names(pop_region_9) <- c("region_9", "total_population")
county_region_9 <- merge(county_region_9, pop_region_9)
head(county_region_9)
county_region_9 <- county_region_9[order(county_region_9$counties_n),] 
rm(pop_region_9)

county_region_4 <- aggregate(data$region_4, by = list(data$region_4), FUN = length)
head(county_region_4)
names(county_region_4) <- c("region_4", "counties_n")
pop_region_4 <- aggregate(data$total_population, by = list(data$region_4), FUN = sum)
head(pop_region_4)
names(pop_region_4) <- c("region_4", "total_population")
county_region_4 <- merge(county_region_4, pop_region_4)
head(county_region_4)
county_region_4 <- county_region_4[order(county_region_4$counties_n),] 
rm(pop_region_4)

barplot(county_state$total_population, main = "Population by State", xaxt="n", cex.main = 2)
barplot(county_region_9$total_population, main = "Population by Region9", xaxt="n", cex.main = 2)
barplot(county_region_4$total_population, names.arg = county_region_4$region_4, 
        main = "Population by Region4", xaxt="n", cex.main = 2)

# level (3) life expectancy
data$state_abbr <- factor(data$state_abbr, levels = c("DC", "DE", "HI", "RI", 
                                                      "CT", "NH", "MA", "VT", 
                                                      "AZ", "ME", "NV", "NJ", 
                                                      "WY", "MD", "AK", "UT", 
                                                      "NM", "OR", "WA", "ID", 
                                                      "SC", "ND", "WV", "MT", 
                                                      "CA", "NY", "CO", "LA", 
                                                      "SD", "AL", "FL", "PA", 
                                                      "WI", "AR", "OK", "MS", 
                                                      "MI", "MN", "OH", "IN", 
                                                      "NE", "TN", "IA", "NC", 
                                                      "IL", "KS", "MO", "KY", 
                                                      "VA", "GA", "TX"))
mytable(data$state_abbr)
data$region_9 <- factor(data$region_9, levels = c("new england", "middle atlantic",
                                                  "pacific", "mountain", 
                                                  "east south central", 
                                                  "east north central", "west south central",
                                                  "south atlantic", "west north central"))
mytable(data$region_9)
data$region_4 <- factor(data$region_4, levels = c("northeast", "west", "midwest", "south"))
mytable(data$region_4)

boxplot(life_expectancy ~ state_abbr, data = data, las = 2, main = "Life Expectancy by State", las=2, cex.main = 2)
boxplot(life_expectancy ~ region_9, data = data, main = "Life Expectancy by Region9", las=2, cex.main = 2)
boxplot(life_expectancy ~ region_4, data = data, main = "Life Expectancy by Region4", las=2, cex.main = 2)

# clean up environment
rm(county_state)
rm(county_region_4)
rm(county_region_9)

dev.off() 
pdf("community2.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))
boxplot(life_expectancy ~ OMBcode2013, data = data, las = 2, 
        main = "Life Expectancy by Metro Classification", las=2, cex.main = 2)
plot(life_expectancy ~ medianage, data = data, las = 2, 
     main = "Life Expectancy by Median Age", las=2, cex.main = 2)
plot(life_expectancy ~ age_under_25_p, data = data, las = 2, 
     main = "Life Expectancy by Population under 25", las=2, cex.main = 2)
plot(life_expectancy ~ age_65_older_p, data = data, las = 2, 
     main = "Life Expectancy by Population over 64", las=2, cex.main = 2)
plot(life_expectancy ~ gender_female_population_p, data = data, las = 2, 
     main = "Life Expectancy by population female", las=2, cex.main = 2)
plot(life_expectancy ~ desert_pop_2010_p, data = data, las = 2, 
     main = "Life Expectancy by population in food desert", las=2, cex.main = 2)

dev.off() 
pdf("community3.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

counts <- table(data$life_long, data$OMBcode2013)
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main = "Metropolitan Classification",
        beside = T, ylim=c(0,100),
        ylab = "Percent of Counties",
        cex.main = 2, cex.names = .75)
# legend("topleft", legend=c("Long life no", "Long life yes"), bty="n", fill=c("black", "gray"))

boxplot(medianage ~ life_long, data = data, main = "Median Age", cex.main = 2, 
        names = c("Long life No", "Long life Yes"),
        ylab = "Age")
boxplot(age_under_25_p ~ life_long, data = data, main = "Under 25", cex.main = 2,
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(age_65_older_p ~ life_long, data = data, main = "Over 64", cex.main = 2,
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(gender_female_population_p ~ life_long, data = data, main = "Female", cex.main = 2,
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(desert_pop_2010_p ~ life_long, data = data, main = "In Food Desert", cex.main = 2,
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")

# economic stability
dev.off() 
pdf("economic.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

boxplot(unemployed_p ~ life_long, data = data, main = "Unemployed", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(poverty_p ~ life_long, data = data, main = "In Poverty", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(income_0_24_p ~ life_long, data = data, main = "Income $0-$20k", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(income_100_over_p ~ life_long, data = data, main = "Income $100K+", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(household_computer_p ~ life_long, data = data, main = "Household Computer", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(household_internet_p ~ life_long, data = data, main = "Household Internet", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")

# Health
dev.off() 
pdf("health.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

boxplot(disability_p ~ life_long, data = data, main = "With Disability", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(inactive_population_20_over_p ~ life_long, data = data, main = "Inactive", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(obese_population_20_over_p ~ life_long, data = data, main = "With Obesity", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(diabetes_population_20_over_p ~ life_long, data = data, main = "With Diabetes", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(drug_deaths_100K ~ life_long, data = data, main = "Drug Deaths / 100K", cex.main = 2, 
        names = c("Long life No", "Long life Yes"),
        ylab = "Deaths/100K Pop")
boxplot(births_15_to_19_p ~ life_long, data = data, main = "Teen Births", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")

# Health System
dev.off() 
pdf("health_system.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

boxplot(uninsured_p ~ life_long, data = data, main = "Uninsured", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(physicians_100k ~ life_long, data = data, main = "Physicians", cex.main = 2, 
        names = c("Long life No", "Long life Yes"),
        ylab = "Per 100K Pop")
boxplot(providers_100k_p ~ life_long, data = data, main = "Other Providers", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), 
        ylab = "Per 100K Pop")

counts <- table(data$life_long, data$shortage_primary_care)
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main = "Shortage Primary Care",
        beside = T, ylim=c(0,100),
        ylab = "Percent of Counties",
        cex.main = 2, cex.names = .75)

counts <- table(data$life_long, data$shortage_dental)
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main = "Shortage Dental",
        beside = T, ylim=c(0,100),
        ylab = "Percent of Counties",
        cex.main = 2, cex.names = .75)

counts <- table(data$life_long, data$shortage_mental_health)
counts <- scale(counts, FALSE, colSums(counts)) * 100
barplot(counts, main = "Shortage Mental Health",
        beside = T, ylim=c(0,100),
        ylab = "Percent of Counties",
        cex.main = 2, cex.names = .75)

# Social Context 1
dev.off() 
pdf("social_context_1.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

boxplot(household_with_under_18_p ~ life_long, data = data, main = "Household with Child", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(household_with_65_older_p ~ life_long, data = data, main = "Household with Elder", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(household_female_head_p ~ life_long, data = data, main = "Household Female Head", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(household_lives_alone_p ~ life_long, data = data, main = "Householder Lives alone", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(married_female_never_p ~ life_long, data = data, main = "Female Never Married", cex.main = 2, 
        names = c("Long life No", "Long life Yes"),
        ylab = "Deaths/100K Pop")
boxplot(married_male_never_p ~ life_long, data = data, main = "Male Lives Alone", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")

# Social Context 2
dev.off() 
pdf("social_context_2.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(2, 3))

boxplot(race_white_p ~ life_long, data = data, main = "Race White", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(born_foreign_p ~ life_long, data = data, main = "Foreign Born", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(born_foreign_not_citizen_p ~ life_long, data = data, main = "Foreign Born Not Citizen", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(born_different_state_p ~ life_long, data = data, main = "Born Different State", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(language_english_only_p ~ life_long, data = data, main = "English Only", cex.main = 2, 
        names = c("Long life No", "Long life Yes"),
        ylab = "Deaths/100K Pop")
boxplot(vetpop_p ~ life_long, data = data, main = "Veterans", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
dev.off()

# Education
dev.off() 
pdf("education.pdf", width = 11, height = 8.5)
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(2, 1))

boxplot(edu_nohs_p ~ life_long, data = data, main = "No High School", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
boxplot(edu_college_more ~ life_long, data = data, main = "College Degree+", cex.main = 2, 
        names = c("Long life No", "Long life Yes"), ylim=c(0,1),
        ylab = "Proportion of Counties")
dev.off()

# Save everything
setwd("C:/Users/marlastuart/Dropbox/SW282/Spring_2019/module_data_exploration")
write.csv(data, "master.csv")
