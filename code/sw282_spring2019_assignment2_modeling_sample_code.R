# Sample code for SW282, Spring 2019, Assignment 1 Data Exploration
# Data exploration
# 1/25/19
# Marla Stuart

# Research question
# What are the structural and social determinants of health disparities?
# Data: 51 tables of health data 
# PART 1 DATA WRANGLING

# PREPARE ENVIRONMENT #########################################################
# Clear the environment
rm(list=ls())

# Load packages
# for missingness function
library(matrixStats)
# For correlation plot
library(corrplot)
# For bivariate statistics
library(compareGroups)
# For univariate statistics
library(psych)
# For R2
library(DescTools)

# Produce personal functions

# My table -- so I don't need to type "useNA = always"
mytable <- function(x) {
  table(x, useNA = "always")
}

# Missingness -- to illustrate missing rate for each variable
missingness <- function(table) {
  na_count <- as.data.frame(sapply(table, function(y) sum(is.na(y))))
  names(na_count)[1] <- "number"
  na_count$percent <- round((na_count$number / nrow(table) * 100), digits = 2)
  na_count <- na_count[order(-na_count$percent), ]
  print(na_count)
}

# dichtomous prediction performance
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

# set the working directory
setwd("C:/Users/marlastuart/Dropbox/SW282/Spring_2019/module_data_exploration") 

# load the data
data <- read.csv("master.csv", header=TRUE)
names(data)

# take a look at all variables -- is it what I expect?
sapply(data, summary)

# delete this variable that R always adds
data$X <- NULL

##############################################################################
# Begin to under stand bivariate correlations

# correlation table and plot
mytable(complete.cases(data))
# we lose 9% just for this diagnostic visual
# also drop fips and na_count and county
data_cor <- data
data_cor$complete <- complete.cases(data_cor)
mytable(data_cor$complete)
names(data_cor)
data_cor <- data_cor[data_cor$complete == TRUE, 3:48]
mytable(complete.cases(data_cor))

# all variables must be numeric
str(data_cor)
data_cor$state_abbr <- as.numeric(data_cor$state_abbr)
data_cor$OMBcode2013 <- as.numeric(data_cor$OMBcode2013)
data_cor$region_4 <- as.numeric(data_cor$region_4)
data_cor$region_9 <- as.numeric(data_cor$region_9)
data_cor$metro_nonmetro <- as.numeric(data_cor$metro_nonmetro)
data_cor$shortage_dental <- as.numeric(data_cor$shortage_dental)
data_cor$shortage_mental_health <- as.numeric(data_cor$shortage_mental_health)
data_cor$shortage_primary_care <- as.numeric(data_cor$shortage_primary_care)
str(data_cor)

# drop complete and life_long
data_cor$complete <- NULL
data_cor$life_long <- NULL

# calculate the correlation between every column pair and look at it.
cor <- round(cor(data_cor), 2)
cor
summary(cor)
# plot the correlations -- visuals are easier to digest than numbers
dev.off()
par(xpd=TRUE)
corrplot(cor, method = "shade", type = "full",  
         diag = F, order = "hclust", tl.cex = .5, tl.col = "black",
         hclust.method = "ward.D2", mar = c(0,0,0,0),
         tl.offset = .4, title = "Social Determinants Correlations", addrect = 12)

# EXPLORE THE DATA ############################################################
# Look at distributions

# Make small multiples
# Set up the plot window

# data by DV
dev.off()
names(data)
par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
hist(data$life_expectancy, main = "Life Expectancy", xlab = "Mean Age", ylab = "Number of Counties",
     cex.main = 5, cex.lab = 2)
text(x=71, y=800, labels = round(mean(data$life_expectancy), 2))
text(x=70, y=800, labels = "mean = ")
lines(density(data$life_expectancy, bw = 1))

dev.off() 
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

boxplot(life_expectancy ~ state_abbr, data = data, main = "Life Expectancy by State", 
        las=2, cex.main = 3, cex.axis = 2)
text(x=4, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$age_under_25_p), 2))
text(x=4, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$state_abbr)^2, 2))
text(x=1, y=72, labels = "R = ", cex = 2)
text(x=1, y=70, labels = "R2 = ", cex = 2)

# age < 18
plot(data$age_under_25_p, data$life_expectancy,
     main = "Age Under 25", cex.main = 2, xlim = c(0, 1),
     xlab = "Age Under 25", ylab = "Mean Life Expectancy")
text(x=.2, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$age_under_25_p), 2))
text(x=.2, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$age_under_25_p)^2, 2))
text(x=.1, y=72, labels = "R = ", cex = 2)
text(x=.1, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~age_under_25_p,data=data)
abline(line)

# age > 64
plot(data$age_65_older_p, data$life_expectancy,
     main = "Age >64", cex.main = 2, xlim = c(0, 1),
     xlab = "Age >64", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$age_65_older_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$age_65_older_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~age_65_older_p,data=data)
abline(line)

# female
plot(data$gender_female_population_p, data$life_expectancy,
     main = "Females", cex.main = 2, xlim = c(0, 1),
     xlab = "Females", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$gender_female_population_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$gender_female_population_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~gender_female_population_p,data=data)
abline(line)

# dessert
plot(data$desert_pop_2010_p, data$life_expectancy,
     main = "Food Desert", cex.main = 2, xlim = c(0, 1),
     xlab = "Food Desert", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$desert_pop_2010_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$desert_pop_2010_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~desert_pop_2010_p,data=data)
abline(line)

# veterans
plot(data$vetpop_p, data$life_expectancy,
     main = "Veterans", cex.main = 2, xlim = c(0, 1),
     xlab = "Veterans", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$vetpop_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$vetpop_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~vetpop_p,data=data)
abline(line)

dev.off() 
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

# computer
plot(data$household_computer_p, data$life_expectancy,
     main = "Household Computer", cex.main = 2, xlim = c(0, 1),
     xlab = "Household Computer", ylab = "Mean Life Expectancy")
text(x=.2, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$household_computer_p), 2))
text(x=.2, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$household_computer_p)^2, 2))
text(x=.1, y=72, labels = "R = ", cex = 2)
text(x=.1, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~household_computer_p,data=data)
abline(line)

# internet
plot(data$household_internet_p, data$life_expectancy,
     main = "Household Internet", cex.main = 2, xlim = c(0, 1),
     xlab = "Household Internet", ylab = "Mean Life Expectancy")
text(x=.2, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$household_internet_p), 2))
text(x=.2, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$household_internet_p)^2, 2))
text(x=.1, y=72, labels = "R = ", cex = 2)
text(x=.1, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~household_internet_p,data=data)
abline(line)

# income100+
plot(data$income_100_over_p, data$life_expectancy,
     main = "High Income (>$100K)", cex.main = 2, xlim = c(0, 1),
     xlab = "High Income (>$100K)", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$income_100_over_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$income_100_over_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~income_100_over_p,data=data)
abline(line)

# college
plot(data$edu_college_more, data$life_expectancy,
     main = "Education College+", cex.main = 2, xlim = c(0, 1),
     xlab = "Education College+", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$edu_college_more), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$edu_college_more)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~edu_college_more,data=data)
abline(line)


dev.off() 
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

# disability
plot(data$disability_p, data$life_expectancy,
    main = "Disability", cex.main = 2, xlim = c(0, 1),
    xlab = "Disability", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$disability_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$disability_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~disability_p,data=data)
abline(line)

# obesity
plot(data$obese_population_20_over_p, data$life_expectancy,
     main = "Obesity", cex.main = 2, xlim = c(0, 1),
     xlab = "Obesity", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$obese_population_20_over_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$obese_population_20_over_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~obese_population_20_over_p,data=data)
abline(line)

#inactive
plot(data$inactive_population_20_over_p, data$life_expectancy,
     main = "Inactive", cex.main = 2, xlim = c(0, 1),
     xlab = "Inactive", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$inactive_population_20_over_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$inactive_population_20_over_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~inactive_population_20_over_p,data=data)
abline(line)

# diabetes
plot(data$diabetes_population_20_over_p, data$life_expectancy,
     main = "Diabetes", cex.main = 2, xlim = c(0, 1),
     xlab = "Diabetes", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$diabetes_population_20_over_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$diabetes_population_20_over_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~diabetes_population_20_over_p,data=data)
abline(line)


dev.off() 
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3, 2))

# births teen
plot(data$births_15_to_19_p, data$life_expectancy,
     main = "Teen Births", cex.main = 2, xlim = c(0, 1),
     xlab = "Teen Births", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$births_15_to_19_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$births_15_to_19_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~births_15_to_19_p,data=data)
abline(line)


# poverty
plot(data$poverty_p, data$life_expectancy,
     main = "Poverty", cex.main = 2, xlim = c(0, 1),
     xlab = "Poverty", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$poverty_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$poverty_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~poverty_p,data=data)
abline(line)

# income 24
plot(data$income_0_24_p, data$life_expectancy,
     main = "Low Income (<$24K)", cex.main = 2, xlim = c(0, 1),
     xlab = "Low Income (<$24K)", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$income_0_24_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$income_0_24_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~income_0_24_p,data=data)
abline(line)


# education hs
plot(data$edu_nohs_p, data$life_expectancy,
     main = "Education < High School", cex.main = 2, xlim = c(0, 1),
     xlab = "Education < High School", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$edu_nohs_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$edu_nohs_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~edu_nohs_p,data=data)
abline(line)


#unemployment
plot(data$unemployed_p, data$life_expectancy,
     main = "Unemployed", cex.main = 2, xlim = c(0, 1),
     xlab = "Unemployed", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$unemployed_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$unemployed_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~unemployed_p,data=data)
abline(line)


# female headed household
plot(data$household_female_head_p, data$life_expectancy,
     main = "Female-Headed Household", cex.main = 2, xlim = c(0, 1),
     xlab = "Female-Headed Household", ylab = "Mean Life Expectancy")
text(x=.8, y=72, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$household_female_head_p), 2))
text(x=.8, y=70, cex = 2, labels = round(cor(data_cor$life_expectancy, data_cor$household_female_head_p)^2, 2))
text(x=.7, y=72, labels = "R = ", cex = 2)
text(x=.7, y=70, labels = "R2 = ", cex = 2)
line <- lm(life_expectancy~household_female_head_p,data=data)
abline(line)


# exploring the DV by location

dev.off() 
#par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(3,2)) 

# level (3) # of counties
mytable(data$state_abbr)
plot(reorder(data$state_abbr, data$state_abbr, FUN = length), 
     main = "Counties in State", xaxt="n", cex.main = 3)
mytable(data$region_9)
plot(reorder(data$region_9, data$region_9, FUN = length),  
     main = "Counties in Region9", xaxt="n", cex.main = 3)

# level (3) population
county_state <- aggregate(data$state_abbr, by = list(data$state_abbr), FUN = length)
names(county_state) <- c("state_abbr", "counties_n")
pop_state <- aggregate(data$total_population, by = list(data$state_abbr), FUN = sum)
names(pop_state) <- c("state_abbr", "total_population")
county_state <- merge(county_state, pop_state)
county_state <- county_state[order(county_state$counties_n),] 
rm(pop_state)

county_region_9 <- aggregate(data$region_9, by = list(data$region_9), FUN = length)
names(county_region_9) <- c("region_9", "counties_n")
pop_region_9 <- aggregate(data$total_population, by = list(data$region_9), FUN = sum)
names(pop_region_9) <- c("region_9", "total_population")
county_region_9 <- merge(county_region_9, pop_region_9)
head(county_region_9)
county_region_9 <- county_region_9[order(county_region_9$counties_n),] 
rm(pop_region_9)

barplot(county_state$total_population, main = "Population by State", xaxt="n", cex.main = 3)
barplot(county_region_9$total_population, main = "Population by Region9", xaxt="n", cex.main = 3)

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


boxplot(life_expectancy ~ state_abbr, data = data, main = "Life Expectancy by State", 
        las=2, cex.main = 3, cex.axis = 2)
boxplot(life_expectancy ~ region_9, data = data, main = "Life Expectancy by Region9", 
        cex.main = 3, cex.axis = 2, names = c("NE", "MA", "PC", "ME", "ESC", "ENC", "WSC", "SA", "WNC"))
mytable(data$region_9)

# clean up environment
rm(county_state)
rm(county_region_9)

# create a dichotomous DV

summary(data$life_expectancy)
data$life_long <- 0
data$life_long[data$life_expectancy > 79.49] <- 1
summary(data$life_long)
mytable(data$life_long)
#plot(groups_table)



# MODEL DATA ##################################################################
# make model table
models <- data
names(models)
models$fips <- NULL
models$county_name <- NULL
models$region_4 <- NULL
# models$state_abbr <- NULL
models$metro_nonmetro <- NULL
names(models)

class(models$life_long)
mytable(models$life_long)
models$life_long <- factor(models$life_long, labels = c("no", "yes"))
mytable(models$life_long)
names(models)
str(models)

# short names for models
names(models)
names(models) <- c("state", "reg9", "metro", 
                   "pop", "age", "youth", 
                   "elders", "females", "desert",
                   "unemployed", "poverty", "income_low",
                   "income_high", "computer", "internet",
                   "disability", "inactive", "obese",
                   "diabetes", "drug_deaths", "teen_births", 
                   "uninsured", "dr_100", "proviers_100", 
                   "primary_short", "dental_short", "mh_short",
                   "life_expect", "house_elder", "house_child",
                   "house_female", "white", "never_mar_f", 
                   "never_mar_m", "born_f", "not_citizen", 
                   "born_state", "english", "house_alone", 
                   "vets", "educ_nohs", "edu_collete",
                   "life_long")
                   
                 
names(models)
# Try different models. Compare fits.
# missing values are a problem -- remove counties with any missing
# or could do multiple imputation

# do imputation
# requires that all variables are numeric
str(models)

mytable(models$state)
models$state <- as.numeric(models$state)
mytable(models$state)

mytable(models$metro)
models$metro <- as.numeric(models$metro)
mytable(models$metro)

mytable(models$reg9)
models$reg9 <- as.numeric(models$reg9)
mytable(models$reg9)

mytable(models$primary_short)
models$primary_short <- as.numeric(models$primary_short)
mytable(models$primary_short)

mytable(models$dental_short)
models$dental_short <- as.numeric(models$dental_short)
mytable(models$dental_short)

mytable(models$mh_short)
models$mh_short <- as.numeric(models$mh_short)
mytable(models$mh_short)

mytable(models$life_long)
models$life_long <- as.numeric(models$life_long)
mytable(models$life_long)

str(models)
lapply(models, summary)

library(mice)
imp <- mice(models, seed = 1234)

# look at the imputations
imp$imp$metro
imp$imp$desert
imp$imp$unemployed
imp$imp$poverty
imp$imp$inactive
imp$imp$obese
imp$imp$diabetes
# up to this point, 3 counties with missing: 82, 2413, 549
data[82, ] #kusilvak AK
data[2413, ] #oglala lakota county SD
data[549, ] #kalawao HI
# all 3 of these are native american heavy

imp$imp$drug_deaths
imp$imp$teen_births
imp$imp$not_citizen

# only a few vars have a lot of missing
dim(imp$imp$metro) #2
dim(imp$imp$desert) #2
dim(imp$imp$unemployed) #1
dim(imp$imp$poverty) #1
dim(imp$imp$inactive) #2
dim(imp$imp$obese) #2
dim(imp$imp$diabetes) #2
dim(imp$imp$drug_deaths) #158
158/3142 #5%
dim(imp$imp$teen_births) #114
114/3142 #4%
dim(imp$imp$not_citizen) #13
13/3142 #0%

complete(imp, action = 1)

# create a mean of the 5 imputations and use that
# imp$imp$metro$mean <- mean(imp$imp$metro[, 1:5])

# for presentation 2/14 just pick 1 imputation
# later figure out how to take an average -- or another way to pool with ML
  
models_imp <- complete(imp, action = 1)  
lapply(models_imp, summary)

models <- models_imp
names(models)

# remove vars
models$state <- NULL
models$life_expect <- NULL

# DV = factor
mytable(models$life_long)
models$life_long <- factor(models$life_long, labels = c("No", "Yes"))
mytable(models$life_long)

# IV = factor
mytable(models$primary_short)
models$primary_short <- factor(models$primary_short, labels = c("None", "Some", "All"))
mytable(models$primary_short)
        
mytable(models$dental_short)
models$dental_short <- factor(models$dental_short, labels = c("None", "Some", "All"))
mytable(models$dental_short)        
        
mytable(models$mh_short)
models$mh_short <- factor(models$mh_short, labels = c("None", "Some", "All"))
mytable(models$mh_short) 

mytable(data$region_9)
mytable(models$reg9)
models$reg9 <- factor(models$reg9, labels = c("NE", "MA", "PA", "ME", "SCE", "NCW", "SC", "SA", "WNC"))
mytable(models$reg9)

mytable(data$OMBcode2013)
mytable(models$metro)
models$metro <- factor(models$metro, labels = c("Central", "Fringe", "Medium", "Micro", "NonCore", "Small"))
mytable(models$metro)

str(models)

# create training and validation data sets
set.seed(1234)
test <- sample(nrow(models) , 0.7*nrow(models))
train <- models[test, ]
validate <- models[-test, ]
mytable(models$life_long)
780/3142
mytable(train$life_long)
550/2199
mytable(validate$life_long)
230/943

# Logistic regression
fit.log <- glm(life_long ~ . , 
               data = train, family = binomial())
summary(fit.log)
# test for overdispirsion -- if considerably larger than 1, evidence of overdispersion
deviance(fit.log)/df.residual(fit.log)
# check VIF
library(car)
vif(fit.log)

PseudoR2(fit.log, which = "McFadden")

# Predict
prob <- predict(fit.log, newdata=validate, type = "response")
log_pred <- factor(prob > .5, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
log_perf <- table(validate$life_long, log_pred, dnn=c("Actual", "Predicted"))
log_perf
performance(log_perf)

# Tune model -- remnove insignificant predictors
# this can't have missing values -- remove first
fit.log_reduced <- step(fit.log, direction = "backward")
summary(fit.log_reduced)
# test for overdispirsion -- if considerably larger than 1, evidence of overdispersion
deviance(fit.log_reduced)/df.residual(fit.log_reduced)

PseudoR2(fit.log_reduced, which = "McFadden")

# Predict
prob <- predict(fit.log_reduced, newdata=validate, type = "response")
log_pred <- factor(prob > .5, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
log_perf <- table(validate$life_long, log_pred, dnn=c("Actual", "Predicted"))
log_perf
performance(log_perf)

library(stargazer)
stargazer(fit.log, type = "text", summary = T, out = "logistic.doc", 
          align = T, ci = T, apply.coef = exp, model.names = T, no.space = T)

# supervised machine learning

# classical decision tree
library(rpart)
set.seed(1234)
dtree <- rpart(life_long ~ ., data = train, method = "class",
               parms = list(split="information"))
dtree$cptable
plotcp(dtree)

library(rpart.plot)
dev.off()
prp(dtree, type = 5, extra = 6, fallen.leaves = T, 
    cex = 1, main = "Classic Decision Tree", nn = F, yesno = 2)

# predict
dtree.pred <- predict(dtree, validate, type = "class")
dtree.perf <- table(validate$life_long, dtree.pred, dnn=c("Actual", "Predicted"))
dtree.perf
performance(dtree.perf)

# prune the tree
dtree_prune <- prune(dtree, cp = .0281)
prp(dtree_prune, type = 5, extra = 6, fallen.leaves = T, 
    cex = .6, main = "Classic Decision Tree", nn = F, yesno = 2)

# predict
dtree.pred <- predict(dtree_prune, validate, type = "class")
dtree.perf <- table(validate$life_long, dtree.pred, dnn=c("Actual", "Predicted"))
dtree.perf

performance(dtree.perf)

# Conditional
library(party)
set.seed(1234)
fit.ctree <- ctree(life_long ~., data = train)
fit.ctree
plot(fit.ctree, gp = gpar(fontsize = 10)) 

plot(fit.ctree, main = "Conditional Inference Tree", type = "simple",
     drop_terminal = F)

library(partykit)
st <- as.simpleparty(fit.ctree)
plot(st)

plot(as.simpleparty(fit.ctree))

# predict
ctree.pred <- predict(fit.ctree, validate, type = "response")
ctree.perf <- table(validate$life_long, ctree.pred, dnn = c("Actual", "Predicted"))
ctree.perf
performance(ctree.perf)

# random forest
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(life_long ~., data = train, na.action = na.roughfix,
                           importance = T)
fit.forest
fit.forest.import <- as.data.frame(importance(fit.forest, type=2))

# predict
forest.pred <- predict(fit.forest, validate)
forest.perf <- table(validate$life_long, forest.pred, dnn=c("Actuall", "Predicted"))
forest.perf
performance(forest.perf)

varImpPlot(fit.forest)









for(i in 1:ntree){
  pt <- prettytree(cf@ensemble[[i]], names(cf@data@get("input"))) 
  nt <- new("Random Forest BinaryTree") 
  nt@tree <- pt 
  nt@data <- cf@data 
  nt@responses <- cf@responses 
  
  pdf(file=paste0("filex",i,".pdf"))
  plot(nt, type="simple")
  dev.off()
  
}








# make a table to compare fits
fit_table <- data.frame(matrix(ncol = 5, nrow = 10))
names(fit_table) <- c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy")


# MAPPING ####################################################################
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(rgdal)
library(stringr)
setwd("C:/Users/marlastuart/Dropbox/GBIC/GMU_demonostration_122018/dc-data-journalism-urban-rural-health-and-demographic-data/maps")
dir()
formap <- data
county <- readOGR('cb_2017_us_county_20m.shp')
state <- readOGR('cb_2017_us_state_500k.shp')
division <- readOGR('cb_2017_us_division_500k.shp')
# m is the base map here
m <- leaflet()
m
m<-leaflet() %>%
  addTiles() %>%
  setView(lng=-96,lat=37.8,zoom=4)#show the blank area
m#print m

#####################divison level######################
division_life <- formap %>%
  group_by(region_9) %>%
  summarise(life_mean = mean(life_expectancy),
            life_median=median(life_expectancy))
table(is.element(division$NAME,division_life$region_9))
division$NAME# the first letter is capital
division_life$region_9_new <- str_to_title(division_life$region_9,locale = "en")
table(is.element(division$NAME,division_life$region_9_new))#all match
division_life <- division_life[order(match(division_life$region_9_new,division$NAME)),]
#assign color and labels
summary(division_life$life_mean)
bins <- seq(from=75,to=80,by=1)
pal_division <- colorBin('RdYlBu',domain = division_life$life_mean, bins = bins)
labels <- paste("<p>", division_life$region_9_new,"</p>",
                "<p>", "Mean:",round(division_life$life_mean,digits = 2),"</p>",
                "<p>", "Median:", round(division_life$life_median,digits = 2),"</p>",
                sep='')
labels_division_static <- paste(division_life$region_9_new)
#establish map
m_division <-m %>%
  addPolygons(data = division,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.8,
              fillColor = pal_division(division_life$life_mean),
              label = lapply(labels, HTML))

m_division
#### *** IMPORTANT if you need static map with labels run this
m_division_static <-m %>%
  addPolygons(data = division,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.8,
              fillColor = pal_division(division_life$life_mean),
              label = labels_division_static) %>%
  addLabelOnlyMarkers(lng = ~lon, lat = ~lat, label = ~labels_division_static,
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                  direction = "bottom", offset = c(0,5)))
m_division_static
##################### state level ######################
# add county to the map
m_state <-m %>%addPolygons(data = state,
                           color = '#66000',
                           weight = 1,
                           smoothFactor = 0.5)
m_state#print region county
#group by state and mean life expectancy
state_life <- formap %>%
  group_by(state_abbr) %>%
  summarise(life_mean = mean(life_expectancy),
            life_median=median(life_expectancy))

#check shapefile and new group name
table(is.element(state$STUSPS,state_life$state_abbr))
state <- subset(state,is.element(state$STUSPS,state_life$state_abbr))# This is necessary and important to macth
length(state$STUSPS)
state_life <- state_life[order(match(state_life$state_abbr,state$STUSPS)),]
#assign color & labels
summary(state_life$life_mean)
bins <- seq(from=74,to=82,by=2)
pal_state <- colorBin('RdYlBu',domain = state_life$life_mean, bins = bins)
labels_state <- paste("<p>", state_life$state_abbr,"</p>",
                      "<p>", "Mean:",round(state_life$life_mean,digits = 2),"</p>",
                      "<p>", "Median:", round(state_life$life_median,digits = 2),"</p>",
                      sep='')
labels_state_static <- paste(state_life$state_abbr)
#establish map
m_state_life <-m %>%
  addPolygons(data = state,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.8,
              fillColor = pal_state(state_life$life_mean),
              label = lapply(labels_state, HTML))%>%
  addLegend(pal = pal_state,
            values =round(state_life$life_mean,digits = 2),
            opacity = 0.7,
            position = "topright")
m_state_life

#### *** IMPORTANT if you need static map with labels run this
m_state_static <-m %>%
  addPolygons(data = state,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.8,
              fillColor = pal_state(state_life$life_mean),
              label = labels_state_static,
              labelOptions = labelOptions(permanent = T, textOnly = T, direction="auto", sticky= T,offset=c(0,0)))
m_state_static

#####################county level#######################


# add county to the map
m_county <-m %>%addPolygons(data = county,
                            color = '#66000',
                            weight = 1,
                            smoothFactor = 0.5)
m_county#print region county
#lots of counties use the same name, thus use fips to match
#group by region_4 and mean life expectancy
county_life <- formap %>%
  group_by(fips) %>%
  summarise(life_mean = mean(life_expectancy),
            life_median=median(life_expectancy))
county_life$name <- formap$county_name
# some fips lose digits start with 0
table(nchar(county_life$fips))
table(nchar(as.character(county$GEOID)))
length(county$GEOID)#3220 counties in census while 3142 in data provide
#add 0 to those 4 digits
t_4 <- which(nchar(county_life$fips)==4)
county_life$fips_5 <- county_life$fips
county_life$fips_5[t_4] <- paste("0",county_life$fips[t_4],sep="")
table(nchar(county_life$fips_5))
#check shapefile and new grop name
table(is.element(county$GEOID,county_life$fips_5))#all the county fips in data matched
is.element(county$GEOID,county_life$fips_5)
county <- subset(county,is.element(county$GEOID,county_life$fips_5))# This is necessary and important to macth
length(county$GEOID)
county_life <- county_life[order(match(county_life$fips_5,county$GEOID)),]
#assign color & labels
summary(county_life$life_mean)
bins <- seq(from=66,to=87,by=3)
pal_county <- colorBin('RdYlBu',domain = county_life$life_mean, bins = bins)
labels_county <- paste("<p>", county_life$name,"</p>",
                       "<p>", "Mean:",round(county_life$life_mean,digits = 2),"</p>",
                       "<p>", "Median:", round(county_life$life_median,digits = 2),"</p>",
                       sep='')
#establish map
m_county_life <-m %>%
  addPolygons(data = county,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.8,
              fillColor = pal_county(county_life$life_mean),
              label = lapply(labels_county, HTML))%>%
  addLegend(pal = pal_county,
            values =round(county_life$life_mean,digits = 2),
            opacity = 0.7,
            position = "topright")
m_county_life

#########factor#########################
# add county to the map primary care shortage
m_county <-m %>%addPolygons(data = county,
                            color = '#66000',
                            weight = 1,
                            smoothFactor = 0.5)
m_county#print region county
#lots of counties use the same name, thus use fips to match
#group by county and shortage_primary_care
county_pc <- formap %>%
  group_by(fips) %>%
  summarise(short_pc = shortage_primary_care)
county_pc$name <- formap$county_name
# some fips lose digits start with 0
table(nchar(county_pc$fips))
table(nchar(as.character(county$GEOID)))
length(county$GEOID)#3220 counties in census while 3142 in data provide
#add 0 to those 4 digits
t_4 <- which(nchar(county_pc$fips)==4)
county_pc$fips_5 <- county_pc$fips
county_pc$fips_5[t_4] <- paste("0",county_pc$fips[t_4],sep="")
table(nchar(county_pc$fips_5))
#check shapefile and new grop name
table(is.element(county$GEOID,county_pc$fips_5))#all the county fips in data matched
is.element(county$GEOID,county_pc$fips_5)
county <- subset(county,is.element(county$GEOID,county_pc$fips_5))# This is necessary and important to macth
length(county$GEOID)
county_pc <- county_pc[order(match(county_pc$fips_5,county$GEOID)),]
#assign color & labels
# the easiest way is to change colorFactor not bins
pal_county_pc <- colorFactor('RdYlBu',domain = county_pc$short_pc)
labels_county_pc <- paste("<p>", county_pc$name,"</p>",
                          "<p>", "Shortage Status:",county_pc$short_pc,"</p>",
                          sep='')
#establish map
m_county_pc <-m %>%
  addPolygons(data = county,
              color = 'grey',
              weight = 1,
              smoothFactor = 0.5,
              fillOpacity = 0.8,
              fillColor = pal_county_pc(county_pc$short_pc),
              label = lapply(labels_county_pc, HTML))%>%
  addLegend(pal = pal_county_pc,
            opacity = 0.7,
            values = county_pc$short_pc,
            position = "bottomleft")
m_county_pc




###################################################################################3
# Clusters of counties that are similar? LCA?




# ASSIGNMENT 1: 7.	Produce univariate statistics and visualizations
# Univariate statistics
psych::describe(data, omit = T)

# data by DV
hist(data$life_expectancy)
summary(data$life_expectancy)
data$life_long <- 0
data$life_long[data$life_expectancy > 79.49] <- 1 #(greater than 3rd quartile)
summary(data$life_long)
mytable(data$life_long)

groups_table <- compareGroups(life_long ~ .-county_name -state_abbr,
                              max.ylev = 55, data = models)

groups_table <- createTable(groups_table,hide.no = "no",type =  1, show.all = T)       
groups_table