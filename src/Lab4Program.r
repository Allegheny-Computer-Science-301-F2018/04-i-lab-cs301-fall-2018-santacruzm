# Name: Marisol Santa Cruz 
# Date: 10/12/18

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.

#The code creates an object data that is made up of only the Measles data simulateously excluding Alaska and Hawaii 

data <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska" & state != "Hawaii")

# The folliowing code is computing and adding in a new variable which is the weeks reporting column.

dat <- mutate(data, per100000rate = ((count*100000)/population)*((weeks_reporting)/52))

#Question 2.

#The code filters out everything except the Measles disease rates per year for California

californiaData <- filter(us_contagious_diseases, disease == "Measles", state == "California")

#This section of the code adds a vertival line in the plot to showcase the relation.

ggplot(data = californiaData) + geom_point(mapping = aes(x = year, y = count )) + geom_vline(xintercept = 1965)

#Question 3.

#Similar to the above code: the filtering command is leaving california data in main focus 

dat_caliFocus <- filter(us_contagious_diseases, state == "California")

#The below code is focusing on a the decade that is specified in the line of code

dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"

#The code plots the focused and specified years in the data (with the square root transformation of california)

ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = 1960), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.10))

#without the square root transformation of california 

ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = 1960), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.10))

#Question 4.

# Creates an object to show U.S. contagious diseases data for all states. 

dat_allStates <- filter(us_contagious_diseases)

# Narrows data down to the 1950’s, 1960’s, and 1970’s. 

dat_allStates$yearBlock[dat_allStates$year == 1950] <- "1950’s"
dat_allStates$yearBlock[dat_allStates$year == 1960] <- "1960’s"
dat_allStates$yearBlock[dat_allStates$year == 1970] <- "1970’s"

# Plots a bar graph to display data. 

ggplot(data = allStates) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 5.

# Rate equation (per 100,000 people rate). 

dat_allStates <- mutate(dat_allStates, rate = ((count*100000)/population)* (weeks_reporting/52))

# Plotting a graph to display data. 

ggplot(data = dat_allStates) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + geom_tile(mapping = aes(x = state, y = count, color = rate)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 6.

http://www.invw.org/2010/02/03/autism-explosion-starts-to-look-like-its-the-environment-stupid-not-the-vaccinations/
  
#I was unable to plot a graph sue to the limited about amount of articles that I encountered. However, the article that i read dies indicate an increase of autism in the area of california due to vaccinations. 
  
