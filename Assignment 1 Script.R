## Author: Julian Sauvage
## Class: Econ 613
## Date 1/21/2022

install.packages("tidyverse")
library(tidyverse)
library(knitr)
setwd("/Users/juliansauvage/Desktop/Econ613/Assignment 1/Data/")

##############
##Exercise 1##7
##############

# Number of HH surveyed in 200
hh2007 <- read.csv("dathh2007.csv")
require(dplyr)
n_distinct(hh2007)

# Number of "Couple, with Kids" individuals in 2005
hh2005 <- read.csv("dathh2005.csv")
table(hh2005$mstatus)

# Number of individuals surveyed in 2008
ind2008 <- read.csv("datind2008.csv")
n_distinct(ind2008)
table(ind2008$respondent)

# Number of individuals between 25/35 in 2016
ind2016 <- read.csv("datind2016.csv")
sum(ind2016$age >= 25 & ind2016$age <= 35)

ind2009 <- read.csv("datind2009.csv")
table1 <- table(ind2009$profession, ind2009$gender, deparse.level = 2)

ind2005 <- read.csv("datind2005.csv")
ind2019 <- read.csv("datind2019.csv")

library(reldist)
# Installs and loads DescTools package to call Gini function
install.packages("DescTools")
library(DescTools)

# 2005
# Produce the Inter-decile ratio
# Dropped all observations where wage is NA or 0
# If zeros are not omitted, wages for the first decile are 0 and thus the d9/d1 ratio does not exist
d9d12005 = quantile(ind2005[ind2005$wage != 0, ]$wage, probs = seq(.1, .9, by = .1), na.rm=TRUE)[[9]]/quantile(ind2005[ind2005$wage != 0, ]$wage, probs = seq(.1, .9, by = .1), na.rm=TRUE)[[1]]

# The following lines produce the mean and standard deviation
# I again omit NAs and 0's
sd2005 =  sd(ind2005[ind2005$wage != 0, ]$wage, na.rm = TRUE)
mean2005 =  mean(ind2005[ind2005$wage != 0, ]$wage, na.rm = TRUE)

#Calculates Gini Coefficient
# Only omit NA's here, and keep 0's to measure inequality amongst entire population,
# rather than solely among those earning wages.
gini2005 = Gini(ind2005$wage, na.rm = TRUE)

# 2019
# Produce the Inter-decile ratio
# Dropped all observations where wage is NA or 0
# If zeros are not omitted, wages for the first decile are 0 and thus the d9/d1 ratio does not exist
d9d12019 = quantile(ind2019[ind2019$wage != 0, ]$wage, probs = seq(.1, .9, by = .1), na.rm=TRUE)[[9]]/quantile(ind2019[ind2019$wage != 0, ]$wage, probs = seq(.1, .9, by = .1), na.rm=TRUE)[[1]]

# The following lines produce the mean and standard deviation
# I again omit NAs and 0's
sd2019 =  sd(ind2019[ind2019$wage != 0, ]$wage, na.rm = TRUE)
mean2019 =  mean(ind2019[ind2019$wage != 0, ]$wage, na.rm = TRUE)

#Calculates Gini Coefficient
gini2019 = Gini(ind2019$wage, na.rm = TRUE)

ind2010 <- read.csv("/Users/juliansauvage/Desktop/Econ613/Assignment 1/Data/datind2010.csv")

age2010 <- ind2010$age
female2010 <- subset(ind2010, gender == "Female")
male2010 <- subset(ind2010, gender == "Male")

hist(age2010, main="French Age Distribution in 2010", xlab="Age")
hist(female2010$age, main="French Age Distribution in 2010 (Female)", xlab="Age", freq = F)
hist(male2010$age, main="French Age Distribution in 2010 (Male)", xlab="Age", freq = F)
table(ind2010[ind2010$age > 80,]$age, ind2010[ind2010$age > 80,]$gender)
  

ind2011 <- read.csv("/Users/juliansauvage/Desktop/Econ613/Assignment 1/Data/datind2011.csv")
hh2011 <- read.csv("/Users/juliansauvage/Desktop/Econ613/Assignment 1/Data/dathh2011.csv")

# Merge individual and household datasets to link individual to location
tot2011 <- merge(hh2011, ind2011, by="idmen")
# Pare down merged dataset to solely individuals located in Paris
paris2011 <- subset(tot2011, location == "Paris")
# Return the number of distinct individuals in Paris dataset
n_distinct(paris2011)

##############
##Exercise 2##
##############

# Initialize two empty dataframes to hold individual/household data respectively
allind <- data.frame(NULL)
allhh <- data.frame(NULL)

# years is the sequence to iterate over
years <- 2004:2019

# Read in all datasets, and append all datasets
# Creates allhh and allind to store household/individual data respectively
for (i in years) {
  # temporary name variables represent filenames to be read in
  nameind <- paste0("datind", i,".csv")
  namehh <- paste0("dathh", i,".csv")
  
  # Read in individual data, and append to existing ind df
  tmpdat <- read.csv(nameind, header=T, stringsAsFactors=F)
  allind <- rbind(allind, tmpdat)
  
  # Read in household data, and append to existing hh df
  tmpdat <- read.csv(namehh, header=T, stringsAsFactors=F)
  allhh <- rbind(allhh, tmpdat)
}

# Find variables present in both datasets
intersect(colnames(allhh), colnames(allind))
# Merge datasets by household identifier and year
alldat <- merge(allhh, allind, by=c("idmen","year"))

# Add count of individuals per household, by year
alldat <- alldat %>%
  add_count(idmen, year)

alldat <- arrange(alldat, year, idmen)
# Filter dataframe to get only households with more than 4 family members
tmpdat <- filter(alldat, n>4)
# Find number of distinct households
n_distinct(tmpdat)

# Number of households with at least 1 unemployed member
alldat %>%
  group_by(idmen, year) %>%
  filter(any(empstat != "Employed")) %>%
  summarize() #35,039 groups, grouped by idmen. 139,312 rows

alldat %>%
  filter(mstatus == "Couple, with Kids") %>%
  count()

alldat %>%
  filter(location == "Paris") %>%
  count()


# Find maximum household size
alldat <- alldat %>% 
  group_by(year, idmen) %>% 
  mutate(group_size = n_distinct(idind, gender, age, wage))

max_group <- max(alldat$group_size)
tmpdat = data.frame(NULL)
tmpdat <- filter(alldat, group_size == max_group)

tmpdat$idmen <- format(tmpdat$idmen, scientific = F) #Largest household has 14 members, idmen: 2207811124040100

allhh %>%
  filter(year == 2010 | year == 2011) %>%
  group_by(idmen) %>%
  mutate(group_size = n()) %>%
  filter(group_size == 2) %>%
  n_distinct() #17,968 households in both 2010 and 2011


##############
##Exercise 3##
##############

# Create variables representing year of panel entry and exit
# Calculate time spent in panel, represented by time_spent
tmpdat <- allhh %>%
  group_by(idmen) %>%
  mutate(enter = min(year)) %>%
  mutate(exit = max(year)) %>%
  mutate(time_spent = exit - enter)

time <-tmpdat$time_spent
hist(time, breaks = 9, main="Years Spent in Survey (Household)", xlab="Years", freq = F)

#identify households that moved into current dwelling in year of survey
tmpdat <- allhh %>%
  mutate(ident = ifelse(year == datent, 1, 0)) %>%
  group_by(year) %>%
  mutate(perc = mean(ident, na.rm = T))
head(tmpdat, 10)

tmpdat <- alldat %>%
  mutate(ident = ifelse(year == datent, 1, 0)) %>%
  group_by(year) %>%
  mutate(perc = mean(ident, na.rm = T))

ggplot(tmpdat, aes(x = year, y = perc)) + geom_point(color = "darkorchid4") + labs(x = "Year", y = "Share of Individuals", title = "Share of Individuals who moved into dwelling in survey year")

#identify households that migrated in year of survey
tmpdat2 <- data.frame(NULL)
tmpdat2 <- allhh %>%
  mutate(ident = ifelse((year == myear & year < 2015) | (!is.na(move) & move == 2), 1, 0)) %>%
  group_by(year) %>%
  mutate(perc = mean(ident, na.rm = T))
head(tmpdat2, 10)

tmpdat2 <- alldat %>%
  mutate(ident = ifelse((year == myear & year < 2015) | (!is.na(move) & move == 2), 1, 0)) %>%
  group_by(year) %>%
  mutate(perc = mean(ident, na.rm = T))

ggplot(tmpdat2, aes(x = year, y = perc)) + geom_point(color = "darkorchid4") + 
  labs(x = "Year", y = "Share of Individuals", title = "Share of Individuals who migrated in survey year")

# Add identifier to 
tmpdat <- mutate(tmpdat, a = 0)
tmpdat2 <- mutate(tmpdat2, a = 1)
tmpdat2 <- rbind(tmpdat, tmpdat2)

tmpdat2 <- distinct(tmpdat2, year, perc, a)

qplot(
  x = year,
  y = perc,
  data = tmpdat2,
  color = as.factor(tmpdat2$a)
) + labs(x = "Year", y = "Share of Individuals", title = "Share of Individuals who migrated in survey year", color = "Legend\n") +  scale_color_manual(labels = c("Based on datent", "Based on myear/move"), values = c("blue", "red"))


##############
##Exercise 4##
##############

tmpind <- allind %>%
  group_by(idind) %>%
  mutate(enter = min(year)) %>%
  mutate(exit = max(year)) %>%
  mutate(time_spent = exit - enter)

tmpind <- tmpind %>%
  group_by(year) %>%
  mutate(panel_size = n()) %>%
  mutate(new_participants = sum(year == enter))

tmpind %>%
  mutate(new_ind = ifelse(enter == year, 1, 0)) %>%
  mutate(lagged_panel_size = lag(panel_size, n=)) %>%
  View()

View(distinct(tmpind,panel_size))


attrition <- data.frame(year = 2004:2019)
attrition <- cbind(attrition, distinct(tmpind,panel_size)$panel_size)
attrition <- mutate(attrition, lagged_panel = lag(attrition$`distinct(tmpind, panel_size)$panel_size`))
attrition <- cbind(attrition, distinct(tmpind,new_participants)$new_participants)
colnames(attrition) <- c("year", "panel_size", "lagged_panel", "new_participants")

# Compute attrition rate
# Attrition Rate = (Lagged Panel Size - Existing Participants)/Lagged Panel Size
attrition <- attrition %>%
  mutate(existing_participants = panel_size - new_participants) %>%
  mutate(attritionRate = (lagged_panel - existing_participants)/lagged_panel)

attrition[,c("year", "attritionRate")]


