# Sample code for SW282, Spring 2019, 
# FOR NUMERIC DATA Assignment 1: Data Exploration
# Data wrangling (from raw data to model dataset)
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
# for reshape()
library(tidyr)
# delete empty rows
library(janitor)
# codebook
library(codebook)

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
setwd("C:/Users/marlastuart/Dropbox/GBIC/GMU_demonostration_122018/dc-data-journalism-urban-rural-health-and-demographic-data/data")

################################################################################
################################################################################
# LOAD DATA ###################################################################
# Find the data
temp <- list.files(pattern=".csv")
temp
# 51 tables 

# Open all the data in a list 
data <- lapply(temp, read.csv, header=TRUE)

# I get the following warning messages
# Warning messages:
# 1: In read.table(file = file, header = header, sep = sep, quote = quote,  :
#   incomplete final line found by readTableHeader on 'Food Insecure Households.csv'
# 2: In read.table(file = file, header = header, sep = sep, quote = quote,  :
#   incomplete final line found by readTableHeader on 'Health Workforce - nurses.csv'
# 3: In read.table(file = file, header = header, sep = sep, quote = quote,  :
#   incomplete final line found by readTableHeader on 'Substance Abuse Treatment.csv'
# I will ignore these messages becuase I delete these tables later

################################################################################
################################################################################
# ASSIGNMENT 1: 1a.	Finish data wrangling: Review and clean up your R code.
# Usually this includes finding and fixing errors.
# But this data has no errors.
# Wrangling also includes structuring the data for the analysis which I do here

# only keep tables with data by county
lapply(data, dim)
# there are 3007 counties in the US
dim_rows <- lapply(data, dim)
dim_rows_2 <- as.data.frame(t(as.data.frame(dim_rows)))
mytable(dim_rows_2$V1)
# keep any table that has more than 30 rows
dim_rows_2 <- cbind(dim_rows_2, temp)
head(dim_rows_2)
temp <- as.character(dim_rows_2[dim_rows_2$V1>30, 3])
temp
# now just 34 tables that have data by county

# clean up environment
rm(dim_rows)
rm(dim_rows_2)

# reload only tables with data by county
data <- lapply(temp, read.csv, header=TRUE)
# now there are just 34 loaded tables
# name each dataframe in the list
names(data) <- temp

# check that all tables have a column called "county_name"
names <- lapply(data, names)
names <- as.data.frame(unlist(names))
names(names)
mytable(names$`unlist(names)`)
# county name n = 33 +1 county -- they all have county
# year n = 34
# clean up the environment
rm(names)

# remove all empty rows and columns
data <- lapply(data, remove_empty)

# In each table, just keep the data for the most recent year
mytable(data$Age.csv$year) #2010 2015
head(data$Age.csv)
dim(data$Age.csv)
age <- data$Age.csv[data$Age.csv$year == 2015, ]
dim(age)
mytable(age$year) 

mytable(data$`AI_AN age.csv`$year) #2015
head(data$`AI_AN age.csv`)
# not useful do not use

mytable(data$`AI_AN Income.csv`$year) #2015
head(data$`AI_AN Income.csv`)
# not useful do not use

head(data$`Census data use.csv`)
# census does not have year
# so just keep all of it
census <- data$`Census data use.csv`

mytable(data$DiabetesPrev.csv$year) #2006-2013
head(data$DiabetesPrev.csv)
diabetes <- data$DiabetesPrev.csv[data$DiabetesPrev.csv$year == 2013, ]
mytable(diabetes$year)

mytable(data$`Drug Poisoning Deaths.csv`$year) #2006-2015
drug_poison_deaths <- data$`Drug Poisoning Deaths.csv`[data$`Drug Poisoning Deaths.csv`$year == 2015, ]
mytable(drug_poison_deaths$year)

mytable(data$Education.csv$year) #2010 2015
education <- data$Education.csv[data$Education.csv == 2015, ]
mytable(education$year)

mytable(data$`Food Desert.csv`$year) #2015
food_desert <- data$`Food Desert.csv`
mytable(food_desert$year)

mytable(data$Gender.csv$year) #2010 2015
gender <- data$Gender.csv[data$Gender.csv$year == 2015, ]
mytable(gender$year)

mytable(data$`Health insurance (18 to 64).csv`$year) #2008-2015
insurance <- data$`Health insurance (18 to 64).csv`[data$`Health insurance (18 to 64).csv`$year == 2015, ]
mytable(insurance$year)

mytable(data$`Health Professional Shortage Areas.csv`$year) # 2016
health_professional_shortage_areas <- data$`Health Professional Shortage Areas.csv`
mytable(health_professional_shortage_areas$year)

mytable(data$`Health Workforce Demographics.csv`$year) #2014
head(data$`Health Workforce Demographics.csv`)
physicians <- data$`Health Workforce Demographics.csv`
mytable(physicians$year)

mytable(data$`Health Workforce.csv`$year) #2014 2015
health_workforce <- data$`Health Workforce.csv`[data$`Health Workforce.csv`$year == 2015, ]
mytable(health_workforce$year)

mytable(data$`Hispanic origin.csv`$year)#2010 2015
hispanic <- data$`Hispanic origin.csv`[data$`Hispanic origin.csv`$year == 2015, ]
mytable(hispanic$year)

mytable(data$`HIV Prevalence.csv`$year) #2014
head(data$`HIV Prevalence.csv`)
hiv <- data$`HIV Prevalence.csv`
mytable(hiv$year)

mytable(data$`HS Graduates.csv`$year) #2015
hs_grads <- data$`HS Graduates.csv`
mytable(hs_grads$year)

mytable(data$Income.csv$year) #2010 2015
income <- data$Income.csv[data$Income.csv$year == 2015, ]
mytable(income$year)

mytable(data$`Life Expectancy.csv`$year) # 2010 2014
head(data$`Life Expectancy.csv`)
life_expectancy <- data$`Life Expectancy.csv`[data$`Life Expectancy.csv`$year == 2014, ]
mytable(life_expectancy$year)

mytable(data$`Median Age.csv`$year) #2010-2015
age_median <- data$`Median Age.csv`[data$`Median Age.csv`$year == 2015, ]
mytable(age_median$year)

mytable(data$ObesityPrev.csv$year) #2006-2013
head(data$ObesityPrev.csv)
obesity <- data$ObesityPrev.csv[data$ObesityPrev.csv$year == 2013, ]
mytable(obesity$year)

mytable(data$`OMB Metro Status.csv`$year) #2013
head(data$`OMB Metro Status.csv`)
metro_code <- data$`OMB Metro Status.csv`
mytable(metro_code$year)

mytable(data$PhysicalInactivity.csv$year) #2006-2013
head(data$PhysicalInactivity.csv)
inactivity <- data$PhysicalInactivity.csv[data$PhysicalInactivity.csv$year == 2013, ]
mytable(inactivity$year)

mytable(data$Population.csv$year) #2015
population <- data$Population.csv
mytable(population$year)

mytable(data$Poverty.csv$year) #2009-2015
poverty <- data$Poverty.csv[data$Poverty.csv$year == 2015, ]
mytable(poverty$year)

mytable(data$Race_ethnicity.csv$year) #2010 2015
race <- data$Race_ethnicity.csv[data$Race_ethnicity.csv$year == 2015,]
mytable(race$year)

mytable(data$`Teen Births.csv`$year) #2014
head(data$`Teen Births.csv`)
teen_births <- data$`Teen Births.csv`
mytable(teen_births$year)

mytable(data$Unemployment.csv$year) #2007-2015
unemployment <- data$Unemployment.csv[data$Unemployment.csv$year == 2015, ]
mytable(unemployment$year)

mytable(data$`Veteran Gender.csv`$year) #2015
gender_veteran <- data$`Veteran Gender.csv`
mytable(gender_veteran$year)

mytable(data$`Veteran Population.csv`$year) #2015
pop_veteran <- data$`Veteran Population.csv`
mytable(pop_veteran$year)

mytable(data$`Veteran Race_ethnicity.csv`$year) #2015
race_veteran <- data$`Veteran Race_ethnicity.csv`
mytable(race_veteran$year)

mytable(data$`Veteran Service Disability.csv`$year) #2015
disability_veteran <- data$`Veteran Service Disability.csv`
mytable(disability_veteran$year)

# goal for each table is 1 row per county
# for tables with more than 1 row per county, convert from long to wide
# and check that it is 1 year
# I know if there is more than 1 row per county if there are more than 3142 rows

# age
head(age)
mytable(age$year)

# 4 age groups
# calculate age group as percent of population
# check if any missing
summary(age$age_group_population)
summary(age$total_population_all_ages)
age$age_group_p <- age$age_group_population/age$total_population_all_ages
head(age)

################################################################################
################################################################################
# ASSIGNMENT 1: 1b. Transform from long to wide or wide to long if necessary
# I do this transformation a few times throughout the code

# spread age group and add to other unique age data
names(age)
age_unique <- unique(age[ , c(1, 2, 5, 7, 8, 11)])
test <- age[, c(2, 9, 12)]
test <- reshape(test, v.names = "age_group_p", idvar = "fips", 
                timevar = "age_group", direction = "wide")
names(test)
names(test) <- c("fips", "age_65_older_p", "age_under_25_p", "age_25_44_p", "age_45_64_p")
names(test)
age <- cbind(age_unique, test[, 2:5])
names(age)
names(age)[6] <- "total_population"
head(age)
age$year <- NULL
# clean environment
rm(age_unique)

# age_median
head(age_median)
mytable(age_median$year)
# just keep new variables and validation variables
age_median <- age_median[, c(2, 9)]
head(age_median)

# census
head(census)
names(census) <- c("county_name", "state_name", "fips", "household_internet", "household_computer",
                   "disability", "household_with_65_older", "household_with_under_18", "household_female_head",
                   "household_lives_alone", "language_english_only", "language_other", "language_spanish",
                   "married_female_never", "married_male_never", "born_foreign", "born_different_state",
                   "born_foreign_not_citizen")
names(census)
census <- census[, 3:18]

# diabetes
head(diabetes)
diabetes$diabetes_population_20_over_p <- diabetes$diabetes_population_20_over/diabetes$total_population_inferred_20_over
head(diabetes)
diabetes <- diabetes[, c(2, 11)]
head(diabetes)

# disablity_veteran
disability_veteran[1:21, ]
mytable(disability_veteran$year)

# 7 diability levels -- collapse to 3 (no, yes, unknown)
# calculate disablity group as percent of population
# check if any missing
mytable(disability_veteran$disability_level)
summary(disability_veteran$vet_population_all_disability_levels)
disability_veteran$disability_level_p <- disability_veteran$vet_dislevel_population/disability_veteran$vet_population_all_disability_levels
head(disability_veteran)

# spread disablity group, condense 
names(disability_veteran)
disability_veteran_unique <- as.data.frame(unique(disability_veteran[ , 2]))
names(disability_veteran_unique)
names(disability_veteran_unique) <- c("fips")
head(disability_veteran_unique)
test <- disability_veteran[, c(2, 9, 12)]
names(test)
head(test)
test <- reshape(test, v.names = "disability_level_p", idvar = "fips", 
                timevar = "disability_level", direction = "wide")
names(test)
test$disabled_vet_no_p <- test$`disability_level_p.0%` + test$disability_level_p.None
test$disabled_vet_yes_p <- test$`disability_level_p.10 or 20%` + test$`disability_level_p.30 or 40%` + test$`disability_level_p.50 or 60%` + test$`disability_level_p.70% and over`
test$disabled_vet_unknown_p <- test$disability_level_p.Unknown
test$test <- test$disabled_vet_no_p + test$disabled_vet_unknown_p + test$disabled_vet_yes_p
head(test)  
summary(test$test)
names(test)
disability_veteran <- cbind(disability_veteran_unique, test[, 9:11])
head(disability_veteran)
# clean environment
rm(disability_veteran_unique)
rm(test)

# drug_poison_deaths
head(drug_poison_deaths)
mytable(drug_poison_deaths$year)
drug_deaths <- drug_poison_deaths[ , c(2, 10)]
head(drug_deaths)
names(drug_deaths)[2] <- "drug_deaths_100K"
head(drug_deaths)
rm(drug_poison_deaths)

# education
head(education)
mytable(education$year)
education[is.na(education$year), ]
education <- education[!is.na(education$year), ]
head(education)

# 6 education groups
# calculate education group as percent of population
# check if any missing
mytable(education$edu_group)
summary(education$total_population_all_edu_groups)
summary(education$edu_group_population)
education$edu_group_p <- education$edu_group_population/education$total_population_all_edu_groups
head(education)

# spread education group and add to other education data
names(education)
education_unique <- as.data.frame(unique(education[ , 2]))
head(education_unique)
names(education_unique) <- "fips"
test <- education[, c(2, 9, 12)]
test <- reshape(test, v.names = "edu_group_p", idvar = "fips", 
                timevar = "edu_group", direction = "wide")
names(test)
names(test) <- c("fips", "edu_some_college_p", "edu_AA_p", "edu_BABS_p", "edu_grad_p",
                 "edu_nohs_p", "edu_hs_p")
names(test)
test$test <- test$edu_some_college_p + test$edu_AA_p + test$edu_BABS_p + test$edu_grad_p + test$edu_nohs_p + test$edu_hs_p
summary(test$test)
education <- cbind(education_unique, test[, 2:7])
head(education)
# clean environment
rm(education_unique)
rm(test)

# food_desert
head(food_desert)
mytable(food_desert$year)
# calculate percent of pop in food desert
food_desert$desert_pop_2010_p <- food_desert$desert_pop_2010 / food_desert$total_pop_2010
summary(food_desert$desert_pop_2010_p)
food_desert[food_desert$desert_pop_2010_p == 1, c(5, 6, 9, 10)]
names(food_desert)
food_desert <- food_desert[, c(2, 11)]
names(food_desert)
# what is the definition or measurement of food desert?

# gender
names(gender)
head(gender)
mytable(gender$year)
gender <- gender[, c(2, 9, 10)]
head(gender)

# health_professional-shortage_areas
head(health_professional_shortage_areas)
# 3 shortage groups
# spread shortage group and add to other health_professional-shortage_areas data
names(health_professional_shortage_areas)
health_professional_shortage_areas_unique <- as.data.frame(unique(health_professional_shortage_areas[ , 2]))
head(health_professional_shortage_areas_unique)
names(health_professional_shortage_areas_unique) <- "fips"
test <- health_professional_shortage_areas[, c(2, 9, 10)]
test <- reshape(test, v.names = "shortage_designation", idvar = "fips", 
                timevar = "shortage_area_type", direction = "wide")
names(test)
names(test) <- c("fips", "shortage_dental", "shortage_primary_care", "shortage_mental_health")
names(test)
mytable(test$shortage_dental)
mytable(test$shortage_primary_care)
mytable(test$shortage_mental_health)
health_professional_shortage <- cbind(health_professional_shortage_areas_unique, test[, 2:4])
head(health_professional_shortage)
# clean environment
rm(health_professional_shortage_areas_unique)
rm(test)
rm(health_professional_shortage_areas)

# health_workforce
head(health_workforce)
mytable(health_workforce$year)
mytable(health_workforce$provider_type)
mytable(health_workforce$provider_type_category)
names(health_workforce)

health_workforce_unique <- as.data.frame(unique(health_workforce[ , c(2, 12)]))
head(health_workforce_unique)

attach(health_workforce)
test <-aggregate(health_workforce[, c(2, 11)], by=list(fips),
                 FUN=sum, na.rm=TRUE)
detach(health_workforce)
head(test)
head(health_workforce)
names(test)[1] <- "fips"
head(test)
health_workforce <- cbind(health_workforce_unique, test[3])
head(health_workforce)
health_workforce$providers_100k <- ((health_workforce$providers / health_workforce$population) * 100000)
head(health_workforce)
health_workforce <- health_workforce[ , c(1, 4)]
head(health_workforce)
# clean environment
rm(health_workforce_unique)
rm(test)

# hispanic
head(hispanic)
mytable(hispanic$year)
names(hispanic)
hispanic <- hispanic[hispanic$origin == "Hispanic or Latino", c(2, 9, 10, 11)]
head(hispanic)
hispanic$hispanic_origin_p <- hispanic$origin_population / hispanic$total_population
summary(hispanic$hispanic_origin_p)
names(hispanic)
hispanic <- hispanic[, c(1, 5)]
head(hispanic)

# hiv
head(hiv)
mytable(hiv$year)
hiv$hiv_p <- hiv$cases / hiv$totpop
summary(hiv$hiv_p)
names(hiv)
hiv <- hiv[, c(2, 11)]
head(hiv)

# hs_grads
head(hs_grads)
# already have this in education
rm(hs_grads)

# inactivity
head(inactivity)
inactivity$inactive_population_20_over_p <- inactivity$inactive_population_20_over / inactivity$total_population_inferred_20_over
summary(inactivity$inactive_population_20_over_p)
mytable(inactivity$year)
inactivity <- inactivity[, c(2, 11)]
head(inactivity)

# income
head(income)
mytable(income$year)

# 4 income groups
# calculate income group as percent of population
# check if any missing
mytable(income$income_group)
summary(income$income_group_population)
summary(income$total_population_all_income_groups)
income$income_group_p <- income$income_group_population/income$total_population_all_income_groups
head(income)

# spread income groups and add to other income data
names(income)
income_unique <- as.data.frame(unique(income[ , 2]))
head(income_unique)
names(income_unique) <- "fips"
test <- income[, c(2, 9, 12)]
test <- reshape(test, v.names = "income_group_p", idvar = "fips", 
                timevar = "income_group", direction = "wide")
names(test)
names(test) <- c("fips", "income_100_over_p", "income_0_24_p", "income_25_50_p", "income_50_100_p")
head(test)
test$test <- test$income_100_over_p + test$income_0_24_p + test$income_25_50_p + test$income_50_100_p
summary(test$test)
income <- cbind(income_unique, test[, 2:5])
head(income)
# clean environment
rm(income_unique)
rm(test)

# income_aian
rm(income_aian)

# insurance
head(insurance)
insurance$uninsured_p <- insurance$uninsured / insurance$population_18to64
summary(insurance$uninsured_p)
insurance[is.na(insurance$uninsured_p), ]
# uninsured and population is 0 -- find population
age[age$fips == 15005, ]
which(insurance$fips == 15005)
names(insurance)
insurance[549, 11] <- 0
summary(insurance$uninsured_p)
uninsured <- insurance[, c(2, 11)]
head(uninsured)
rm(insurance)

# life_expectancy
head(life_expectancy)
mytable(life_expectancy$year)
life_expectancy <- life_expectancy[, c(2, 9)]
head(life_expectancy)

# metro_code
head(metro_code)
mytable(metro_code$OMBcode2013)
metro_code <- metro_code[, c(2, 9)]
head(metro_code)

# obesity
head(obesity)
mytable(obesity$year)
summary(obesity$obese_population_20_over)
summary(obesity$total_population_inferred_20_over)
obesity$obese_population_20_over_p <- obesity$obese_population_20_over / obesity$total_population_inferred_20_over
summary(obesity$obese_population_20_over_p)
head(obesity)
obesity <- obesity[ , c(2, 11)]
head(obesity)

# physicians
head(physicians)
mytable(physicians$year)
physicians_unique <- unique(physicians[ , c(2, 11)])
head(physicians_unique)
physicians <- physicians_unique
names(physicians)[2] <- "physicians"
head(physicians)
names(age)
age_pop <- age[, c(1, 5)]
head(age_pop)
physicians <- merge(physicians, age_pop)
head(physicians)
physicians$physicians_100k <- (physicians$physicians / physicians$total_population) * 100000
head(physicians)
physicians <- physicians[, c(1, 4)]
head(physicians)
rm(physicians_unique)

# pop_veteran
head(pop_veteran)
mytable(pop_veteran$year)
summary(pop_veteran$vetpop)
summary(pop_veteran$totpop)
pop_veteran$vetpop_p <- pop_veteran$vetpop / pop_veteran$totpop
head(pop_veteran)
pop_veteran <- pop_veteran[, c(2, 11)]
head(pop_veteran)

# population
head(population)
# check to see if same as age
population <- population[, c(2, 9)]
head(population)
head(age)
age <- merge(age, population, all = T)
head(age)
age$test <- age$total_population == age$poptotal
mytable(age$test)
age$test <- NULL
age$poptotal <- NULL
rm(population)

# poverty
head(poverty)
mytable(poverty$year)
poverty <- poverty[, c(2, 9)]
head(poverty)
# later in code create the poverty_pop using population in age since it is inferred here

# race
head(race)
mytable(race$year)
# calculate race group as percent of population
# check if any missing
mytable(race$race)
summary(race$race_population)
summary(race$total_population)
race$race_population_p <- race$race_population/race$total_population
head(race)

# spread race group and add to other race data
names(race)
race_unique <- as.data.frame(unique(race[ , 2]))
head(race_unique)
names(race_unique) <- "fips"
test <- race[, c(2, 9, 12)]
test <- reshape(test, v.names = "race_population_p", idvar = "fips", 
                timevar = "race", direction = "wide")
names(test)
names(test) <- c("fips", "race_white_p", "race_AIAN_p", "race_asian_p", "race_black_p",
                 "race_HPI_p", "race_multiple_other_p")
head(test)
test$test <- test$race_white_p + test$race_AIAN_p + test$race_asian_p + test$race_black_p + test$race_HPI_p + test$race_multiple_other_p
summary(test$test)
race <- cbind(race_unique, test[, 2:7])
head(race)
# clean environment
rm(race_unique)
rm(test)

# teen_births
head(teen_births)
mytable(teen_births$year)
teen_births$births_15_to_19_p <- teen_births$births_15_to_19 / teen_births$female_population_15_to_19
summary(teen_births$births_15_to_19_p)
head(teen_births)
teen_births <- teen_births[, c(2, 11)]
head(teen_births)

# unemployment
head(unemployment)
mytable(unemployment$year)
unemployment$unemployed_p <- unemployment$unemployed / unemployment$civilian_labor_force
summary(unemployment$unemployed_p)
head(unemployment)
unemployment <- unemployment[, c(2, 11)]
head(unemployment)

################################################################################
################################################################################
# ASSIGNMENT 1: 1c.	Merge data if necessary
# create a single table
# start with the table with the most counties
master <- age
master <-  merge(master, age_median, all = T)
head(master)
master <- merge(master, census, all = T)
head(master)
master <- merge(master, diabetes, all = T)
head(master)
master <- merge(master, disability_veteran, all = T)
head(master)
master <- merge(master, drug_deaths, all = T)
head(master)
master <- merge(master, education, all = T)
head(master)
master <- merge(master, food_desert, all = T)
head(master)
master <- merge(master, gender, all = T)
head(master)
master <- merge(master, health_professional_shortage, all = T)
head(master)
master <- merge(master, health_workforce, all = T)
head(master)
master <- merge(master, hispanic, all = T)
head(master)
master <- merge(master, hiv, all = T)
head(master)
master <- merge(master, inactivity, all = T)
head(master)
master <- merge(master, income, all = T)
head(master)
master <- merge(master, life_expectancy, all = T)
head(master)
master <- merge(master, metro_code, all = T)
head(master)
master <- merge(master, obesity, all = T)
head(master)
master <- merge(master, physicians, all = T)
head(master)
master <- merge(master, pop_veteran, all = T)
head(master)
master <- merge(master, poverty, all = T)
head(master)
master <- merge(master, race, all = T)
head(master)
master <- merge(master, teen_births, all = T)
head(master)
master <- merge(master, unemployment, all = T)
head(master)
master <- merge(master, uninsured, all = T)
head(master)
# 3157x65

# how many variables does each county have?
master$na_count <- apply(master, 1, function(x) sum(is.na(x)))
mytable(master$na_count)
# delete counties that are mostly missing -- it is 6 counties - which?
which(master$na_count>8)
master[90, 1:2]
master[93, 1:2]
master[96, 1:2]
master[98, 1:2]
master[2432, 1:2]
master[2931, 1:2]
# the 6 with >7 missing don't have a county name
# fips - Federal Information Processing Series
# fips codes are occationally updated -- and counties merge with others
# it would take a little work to figure out if these 6 are a problem
# for now delete the rows

# delete rows with missing > 7
master <- master[master$na_count < 9, ]
#3151x66

################################################################################
################################################################################
# ASSIGNMENT 1: 1d. De-identify your data
# Not necessary here -- it is not sensitive or protected data

################################################################################
################################################################################
# ASSIGNMENT 1: 1e.	Properly structure all final variables.
# assign a logical class to each variable
str(master)
master$fips <- as.character(master$fips)
master$county_name <- as.character(master$county_name)
master$state_abbr <- as.character(master$state_abbr)
str(master)
master$na_count <- NULL

# calculate a few final percents
master$gender_male_population_p <- master$male_population / master$total_population
master$gender_female_population_p <- master$female_population / master$total_population
summary(master$gender_male_population_p)
summary(master$gender_female_population_p)
master$male_population <- NULL
master$female_population <- NULL
summary(master$povertyest)
master$poverty_p <- master$povertyest / master$total_population
summary(master$poverty_p)
master$povertyest <- NULL
names(master)

# add new variables
# add regions
# https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=2ahUKEwi6hMWq15rfAhVLz1QKHWFNDI8QFjABegQIABAC&url=https%3A%2F%2Fwww2.census.gov%2Fgeo%2Fpdfs%2Fmaps-data%2Fmaps%2Freference%2Fus_regdiv.pdf&usg=AOvVaw3kWNnXhSx03aUpYciUqVG5
master$region_4[master$state_abbr == "AK"] <- "west"
master$region_4[master$state_abbr == "AL"] <- "south"
master$region_4[master$state_abbr == "AR"] <- "south"
master$region_4[master$state_abbr == "AZ"] <- "west"
master$region_4[master$state_abbr == "CA"] <- "west"
master$region_4[master$state_abbr == "CO"] <- "west"
master$region_4[master$state_abbr == "CT"] <- "northeast"
master$region_4[master$state_abbr == "DC"] <- "south"
master$region_4[master$state_abbr == "DE"] <- "south"
master$region_4[master$state_abbr == "FL"] <- "south"
master$region_4[master$state_abbr == "GA"] <- "south"
master$region_4[master$state_abbr == "HI"] <- "west"
master$region_4[master$state_abbr == "IA"] <- "midwest"
master$region_4[master$state_abbr == "ID"] <- "west"
master$region_4[master$state_abbr == "IL"] <- "midwest"
master$region_4[master$state_abbr == "IN"] <- "midwest"
master$region_4[master$state_abbr == "KS"] <- "midwest"
master$region_4[master$state_abbr == "KY"] <- "south"
master$region_4[master$state_abbr == "LA"] <- "south"
master$region_4[master$state_abbr == "MA"] <- "northeast"
master$region_4[master$state_abbr == "MD"] <- "south"
master$region_4[master$state_abbr == "ME"] <- "northeast"
master$region_4[master$state_abbr == "MI"] <- "midwest"
master$region_4[master$state_abbr == "MN"] <- "midwest"
master$region_4[master$state_abbr == "MO"] <- "midwest"
master$region_4[master$state_abbr == "MS"] <- "south"
master$region_4[master$state_abbr == "MT"] <- "west"
master$region_4[master$state_abbr == "NC"] <- "south"
master$region_4[master$state_abbr == "ND"] <- "midwest"
master$region_4[master$state_abbr == "NE"] <- "midwest"
master$region_4[master$state_abbr == "NH"] <- "northeast"
master$region_4[master$state_abbr == "NJ"] <- "northeast"
master$region_4[master$state_abbr == "NM"] <- "west"
master$region_4[master$state_abbr == "NV"] <- "west"
master$region_4[master$state_abbr == "NY"] <- "northeast"
master$region_4[master$state_abbr == "OH"] <- "midwest"
master$region_4[master$state_abbr == "OK"] <- "south"
master$region_4[master$state_abbr == "OR"] <- "west"
master$region_4[master$state_abbr == "PA"] <- "northeast"
master$region_4[master$state_abbr == "RI"] <- "northeast"
master$region_4[master$state_abbr == "SC"] <- "south"
master$region_4[master$state_abbr == "SD"] <- "midwest"
master$region_4[master$state_abbr == "TN"] <- "south"
master$region_4[master$state_abbr == "TX"] <- "south"
master$region_4[master$state_abbr == "UT"] <- "west"
master$region_4[master$state_abbr == "VA"] <- "south"
master$region_4[master$state_abbr == "VT"] <- "northeast"
master$region_4[master$state_abbr == "WA"] <- "west"
master$region_4[master$state_abbr == "WI"] <- "midwest"
master$region_4[master$state_abbr == "WV"] <- "south"
master$region_4[master$state_abbr == "WY"] <- "west"

master$region_9[master$state_abbr == "AK"] <- "pacific"
master$region_9[master$state_abbr == "AL"] <- "east south central"
master$region_9[master$state_abbr == "AR"] <- "west south central"
master$region_9[master$state_abbr == "AZ"] <- "mountain"
master$region_9[master$state_abbr == "CA"] <- "pacific"
master$region_9[master$state_abbr == "CO"] <- "mountain"
master$region_9[master$state_abbr == "CT"] <- "new england"
master$region_9[master$state_abbr == "DC"] <- "south atlantic"
master$region_9[master$state_abbr == "DE"] <- "south atlantic"
master$region_9[master$state_abbr == "FL"] <- "south atlantic"
master$region_9[master$state_abbr == "GA"] <- "south atlantic"
master$region_9[master$state_abbr == "HI"] <- "pacific"
master$region_9[master$state_abbr == "IA"] <- "west north central"
master$region_9[master$state_abbr == "ID"] <- "mountain"
master$region_9[master$state_abbr == "IL"] <- "east north central"
master$region_9[master$state_abbr == "IN"] <- "east north central"
master$region_9[master$state_abbr == "KS"] <- "west north central"
master$region_9[master$state_abbr == "KY"] <- "east south central"
master$region_9[master$state_abbr == "LA"] <- "west south central"
master$region_9[master$state_abbr == "MA"] <- "new england"
master$region_9[master$state_abbr == "MD"] <- "south atlantic"
master$region_9[master$state_abbr == "ME"] <- "new england"
master$region_9[master$state_abbr == "MI"] <- "east north central"
master$region_9[master$state_abbr == "MN"] <- "west north central"
master$region_9[master$state_abbr == "MO"] <- "west north central"
master$region_9[master$state_abbr == "MS"] <- "east south central"
master$region_9[master$state_abbr == "MT"] <- "mountain"
master$region_9[master$state_abbr == "NC"] <- "south atlantic"
master$region_9[master$state_abbr == "ND"] <- "west north central"
master$region_9[master$state_abbr == "NE"] <- "west north central"
master$region_9[master$state_abbr == "NH"] <- "new england"
master$region_9[master$state_abbr == "NJ"] <- "middle atlantic"
master$region_9[master$state_abbr == "NM"] <- "mountain"
master$region_9[master$state_abbr == "NV"] <- "mountain"
master$region_9[master$state_abbr == "NY"] <- "middle atlantic"
master$region_9[master$state_abbr == "OH"] <- "east north central"
master$region_9[master$state_abbr == "OK"] <- "west south central"
master$region_9[master$state_abbr == "OR"] <- "pacific"
master$region_9[master$state_abbr == "PA"] <- "middle atlantic"
master$region_9[master$state_abbr == "RI"] <- "new england"
master$region_9[master$state_abbr == "SC"] <- "south atlantic"
master$region_9[master$state_abbr == "SD"] <- "west north central"
master$region_9[master$state_abbr == "TN"] <- "east south central"
master$region_9[master$state_abbr == "TX"] <- "west south central"
master$region_9[master$state_abbr == "UT"] <- "mountain"
master$region_9[master$state_abbr == "VA"] <- "south atlantic"
master$region_9[master$state_abbr == "VT"] <- "new england"
master$region_9[master$state_abbr == "WA"] <- "pacific"
master$region_9[master$state_abbr == "WI"] <- "east north central"
master$region_9[master$state_abbr == "WV"] <- "south atlantic"
master$region_9[master$state_abbr == "WY"] <- "mountain"
master$region_4 <- as.factor(master$region_4)
master$region_9 <- as.factor(master$region_9)
mytable(master$region_4)
mytable(master$region_9)
table(master$region_9, master$region_4, useNA = "always")

# convert the census percents to proportions
summary(master$married_female_never)
master$married_female_never_p <- master$married_female_never / 100
summary(master$married_female_never_p)
master$married_female_never <- NULL
master$married_male_never_p <- master$married_male_never / 100
master$married_male_never <- NULL
master$born_foreign_p <- master$born_foreign / 100
master$born_foreign <- NULL
master$born_foreign_not_citizen_p <- master$born_foreign_not_citizen / 100
master$born_foreign_not_citizen <- NULL
master$born_different_state_p <- master$born_different_state / 100
master$born_different_state <- NULL
master$language_english_only_p <- master$language_english_only / 100
master$language_english_only <- NULL
master$language_other_p <- master$language_other / 100
master$language_other <- NULL
master$language_spanish_p <- master$language_spanish / 100
master$language_spanish <- NULL
master$household_with_65_older_p <- master$household_with_65_older / 100
master$household_with_65_older <- NULL
master$household_with_under_18_p <- master$household_with_under_18 / 100
master$household_with_under_18 <- NULL
master$household_computer_p <- master$household_computer / 100
master$household_computer <- NULL
master$household_female_head_p <- master$household_female_head / 100
master$household_female_head <- NULL
master$household_internet_p <- master$household_internet / 100
master$household_internet <- NULL
master$household_lives_alone_p <- master$household_lives_alone / 100
master$household_lives_alone <- NULL
master$disability_p <- master$disability / 100
master$disability <- NULL
master$providers_100k_p <- master$providers_100k / 100
master$providers_100k <- NULL
head(master)
# remove a couple
master$disabled_vet_no_p <- NULL
master$disabled_vet_unknown_p <- NULL

# establish categories and reorganize for easy use
# ID, community, economic stability, health and healthcare, social context, education
# ID = fips
# population = county, state, region, pop, metro class, pop in food dessert
# economic = unemploy, poverty, income over$100K, computer, internet
# health = disabled, inactive, obese, diabetic, drug, teen births, uninsured, 
# physicians/100k, other provi/100k, primary care shortage, MH, dental
# social = age under 25, age over 64, gender female, femaled headed, race white
# female never married, undoc, language other than English, lives alone
# education = educ < hs, educ => college

names(master)
master_full <- master
master <- master_full[, c("fips",  #ID
                          "county_name",  #community
                          "state_abbr",
                          "region_4",
                          "region_9",
                          "metro_nonmetro",
                          "OMBcode2013",
                          "total_population",
                          "medianage",
                          "age_under_25_p",
                          "age_65_older_p",
                          "gender_female_population_p",
                          "desert_pop_2010_p",
                          "unemployed_p", #economic stability
                          "poverty_p",
                          "income_0_24_p",
                          "income_100_over_p",
                          "household_computer_p",
                          "household_internet_p",
                          "disability_p", #health and health care
                          "inactive_population_20_over_p",
                          "obese_population_20_over_p",
                          "diabetes_population_20_over_p",
                          "drug_deaths_100K",
                          "hiv_p",
                          "births_15_to_19_p",
                          "uninsured_p",
                          "physicians_100k",
                          "providers_100k_p", 
                          "shortage_primary_care",
                          "shortage_dental",
                          "shortage_mental_health",
                          "life_expectancy",
                          "household_with_65_older_p", #social context
                          "household_with_under_18_p",
                          "household_female_head_p",
                          "race_white_p",
                          "married_female_never_p",
                          "married_male_never_p",
                          "born_foreign_p",
                          "born_foreign_not_citizen_p",
                          "born_different_state_p",
                          "language_english_only_p",
                          "household_lives_alone_p",
                          "vetpop_p",
                          "edu_nohs_p", #education
                          "edu_BABS_p",
                          "edu_grad_p")]
names(master)
# 3151 x 48

# Are the fips unique?
test <- as.data.frame(unique(master))
# nope -- which are duplicate?
master$counter <- with(master, ave(fips, fips, FUN = seq_along))
mytable(master$counter)
which(master$counter > 1)
master[c(723:725, 728:730, 732:734), 1:5] 
# which ones can I delete?
master[master$fips == 18049, ]
master[master$fips == 18053, ]
master[master$fips == 18055, ]
# all the same. Delete coutner > 
master <- master[master$counter == 1, ]
mytable(master$counter)
master$counter <- NULL

# round all continuous variables to 2 digits
# I already have all the vars in the order I want
# so just split up, round, and put back together
str(master)
names(master)
master1 <- master[, 1:7]
master2 <- master[, 8:29] #round
master3 <- master[, 30:32]
master4 <- master[, 33:48] #round

master2 <- as.data.frame(lapply(master2, round, 2))
master4 <- as.data.frame(lapply(master4, round, 2))

master <- cbind(master1, master2, master3, master4)
head(master)
names(master)

# combine the education
master$edu_college_more <- master$edu_BABS_p + master$edu_grad_p
summary(master$edu_BABS_p)
summary(master$edu_grad_p)
summary(master$edu_college_more)
master$edu_BABS_p <- NULL
master$edu_grad_p <- NULL

# investigate the missingness
missingness(master)
# they are true missing -- not in the original data
# decide how to resolve during modeling

################################################################################
################################################################################
# ASSIGNMENT 1: 2. Produce a final codebook.
codebook <- codebook_table(master)
names(codebook)
codebook$name

# Delete the columns we don't need.
head(codebook)
codebook$ordered <- NULL
codebook$complete <- NULL
codebook$hist <- NULL
codebook$top_counts <- NULL
codebook$empty <- NULL
codebook$min <- NULL
codebook$max <- NULL
names(codebook)

# add category
codebook$concept <- NA
names(codebook)
codebook$name
codebook[1, 14] <- "id"
codebook[2:13,14] <- "community"
codebook[14:19,14] <- "economic stability"
codebook[20:33,14] <- "health"
codebook[34:45,14] <- "social context"
codebook[46:47,14] <- "education"
mytable(codebook$concept)
class(codebook$concept)

codebook$description <- NA
codebook$name
codebook[1,15] <- "Federal Information Processing Series"
codebook[2,15] <- "County name"
codebook[3,15] <- "State"
codebook[4,15] <- "Region 4 per US Census"
codebook[5,15] <- "Region 9 per US Census"
codebook[6,15] <- "Metropolitan category 2"
codebook[7,15] <- "Metropolitan category 6"
codebook[8,15] <- "Total population number"
codebook[9,15] <- "Median age"
codebook[10,15] <- "Proportion of population that is age 0-24"
codebook[11,15] <- "Proportion of population that is age 65+"
codebook[12,15] <- "Proportion of population that is female"
codebook[13,15] <- "Proportion of population that lives in a food dessert"
codebook[14,15] <- "Proportion of population that is unemployed"
codebook[15,15] <- "Proportion of population that is meets definition for poverty"
codebook[16,15] <- "Proportion of population that is income over under $25k"
codebook[17,15] <- "Proportion of population that is income over $100k"
codebook[18,15] <- "Proportion of population that is household has a computer"
codebook[19,15] <- "Proportion of population that is household has internet" 
codebook[20,15] <- "Proportion of population that is disabled"
codebook[21,15] <- "Proportion of population that is inactive and age 20+"
codebook[22,15] <- "Proportion of population that is obese and age 20+"
codebook[23,15] <- "Proportion of population that has diabetes and is age 20+"
codebook[24,15] <- "Number of drug deaths per 100 residents"
codebook[25,15] <- "Proportion of female population age with HIV"
codebook[26,15] <- "Proportion of female population age 15-19 that has had a birth"
codebook[27,15] <- "Proportion of the population that is uninsured" 
codebook[28,15] <- "Number of physicians per 100k resident" 
codebook[29,15] <- "Number of other medical providers per 100k residents"
codebook[30,15] <- "Status regarding primary care provider shortage"
codebook[31,15] <- "Status regarding dental provider shortage"
codebook[32,15] <- "Status regarding mental health provider shortage" 
codebook[33,15] <- "Mean life expectancy" 
codebook[34,15] <- "Proportion of population that is female never married"
codebook[35,15] <- "Proportion of population that is male never married"
codebook[36,15] <- "Proportion of population that is household with resident age 65+"
codebook[37,15] <- "Proportion of population that is household with resident under age 18"
codebook[38,15] <- "Proportion of population that is household with female head"
codebook[39,15] <- "Proportion of population that is White"
codebook[40,15] <- "Proportion of population that is foreign born"
codebook[41,15] <- "Proportion of population that is foreign born not citizen"
codebook[42,15] <- "Proportion of population that is born different state"
codebook[43,15] <- "Proportion of population that is home language only English"
codebook[44,15] <- "Proportion of population that is householder lives alone"
codebook[45,15] <- "Proportion of population that is veteran"
codebook[46,15] <- "Proportion of population that is less than high school education"
codebook[47,15] <- "Proportion of population that is college or more education"

# add variable role
codebook$role <- "independent"
codebook[33, 16] <- "dependent"

'''
# add description of data construction
codebook$variable_construction <- "in the original data"
codebook[4, 17] <- "assigned per US Census Bureau"
codebook[5, 17] <- "assigned per US Census Bureau"
codebook[10, 17] <- "in original data long, transformed to wide, then divided by total pop"
codebook[11, 17] <- "in original data long, transformed to wide, then divided by total pop"
codebook[12, 17] <- "in original data then divided by total pop"
codebook[13, 17] <- "in original data then divided by total pop"
codebook[14, 17] <- "in original data then divided by total pop"
codebook[15, 17] <- "in original data then divided by total pop"
codebook[16, 17] <- "in original data long, transformed to wide, then divided by total pop"
codebook[17, 17] <- "in original data long, transformed to wide, then divided by total pop"
codebook[18, 17] <- "in original data then divided by total pop"
codebook[19, 17] <- "in original data then divided by total pop"
codebook[20, 17] <- "in original data long, transformed to wide, then divided by total pop"

# START HERE


codebook[, 17] <- "in original data then divided by total pop"
codebook[, 17] <- "in original data then divided by total pop"
codebook[, 17] <- "in original data then divided by total pop"
codebook[, 17] <- "in original data then divided by total pop"

codebook[, 17] <- "in original data long, transformed to wide, then divided by total pop"
codebook[, 17] <- "in original data long, transformed to wide, then divided by total pop"


codebook[7, 17] <- "male popluation / population"
codebook[8, 17] <- "female popluation / population"
codebook[9, 17] <- "in original data"
codebook[10, 17] <- "age 65+ population / population"
codebook[11, 17] <- "age under 25 population / population"
codebook[12, 17] <- "age 25-44 population / population"
codebook[13, 17] <- "age 45-64 population / population"
codebook[14, 17] <- "population hispanic origin / population"
codebook[15, 17] <- "population race white / population"
codebook[16, 17] <- "population race AIAN / population"
codebook[17, 17] <- "population race Asian / population"
codebook[18, 17] <- "population race Black / population"
codebook[19, 17] <- "population race HIP / population"
codebook[20, 17] <- "population race multiple other / population"
codebook[21, 17] <- "population female never married / population"
codebook[22, 17] <- ""
codebook[23, 17] <- ""
codebook[24, 17] <- ""
codebook[25, 17] <- ""
codebook[26, 17] <- ""
codebook[27, 17] <- ""
codebook[28, 17] <- ""
codebook[29, 17] <- ""
codebook[30, 17] <- ""
codebook[31, 17] <- ""
codebook[32, 17] <- ""
codebook[33, 17] <- ""
codebook[34, 17] <- ""
codebook[35, 17] <- ""
codebook[36, 17] <- ""
codebook[37, 17] <- ""
codebook[38, 17] <- ""
codebook[39, 17] <- ""
codebook[40, 17] <- ""
codebook[41, 17] <- ""
codebook[42, 17] <- ""
codebook[43, 17] <- ""
codebook[44, 17] <- ""
codebook[45, 17] <- ""
codebook[46, 17] <- ""
codebook[47, 17] <- ""
'''

# add problems
# I still need to do this

# add validity
# I still need to do this

# add reliablity
# I still need to do this

################################################################################
################################################################################
# ASSIGNMENT 1: 3.	Save your final dataset. Save your final data wrangling code.
setwd("C:/Users/marlastuart/Dropbox/SW282/Spring_2019/module_data_exploration")
write.csv(codebook, "codebook.csv")
write.csv(master, "master.csv")
