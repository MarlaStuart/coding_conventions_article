# Sample code for SW282, Spring 2019, 
# FOR TEXT DATA Assignment 1: Data Exploration
# Make a codebook
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
# codebook
library(codebook)

# Create personal functions

# My table -- so I don't have to always type useNA="always"
# But this only works for a single variable -- not for a cross table with 2 variables
mytable <- function(x) {
  table(x, useNA = "always")
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
# change the class for some
data$fips <- as.character(data$fips)
data$county_name <- as.character(data$county_name)
data$state_abbr <- as.character(data$state_abbr)
str(data)

names(data)
data$X <- NULL
names(data)

################################################################################
################################################################################
# ASSIGNMENT 1: 4. Produce a final codebook.
codebook <- codebook_table(data)
class(codebook)
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

# save your codebook
setwd("C:/Users/marlastuart/Dropbox/SW282/Spring_2019/module_data_exploration")
write.csv(codebook, "codebook.csv")
write.csv(data, "master.csv")
