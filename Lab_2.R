#########################################
#########################################
#
# LAB 2: VISUALIZATION AND TRANSFORMATION
#
#########################################
#########################################


##############################################################################
## This chunk of code reads the data set into the R session. Don't edit this part. 
##############################################################################

# link to description of source data
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md

if(!require(tidyverse)) install.packages('tidyverse')

library(tidyverse)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

##############################################################################
## Now follow the prompts below to write your own R commands 
## When you're done, save this file and put it in the Lab 2 dropbox on D2L
## 
## If you can't get something to work, google it, refer to the book, then reach out to me. If you 
## feel the need to explain something you did, write it as a comment next to your code. 
##############################################################################


########## 1A
########## Get rid of the rows that do not have a "time_overall" measurement

tdf_winners <- tdf_winners %>%
  filter(!is.na(time_overall))

########## 1B
########## Create a column that gives the cyclist's average speed in km/hr

tdf_winners <- tdf_winners %>%
  mutate('avg_speed_(km/h)' = distance / time_overall)

########## 1C
########## Get rid of the columns full_name, nickname, birth_town, and birth_country 
tdf_winners_no_name <- tdf_winners %>%
  select(-full_name, -nickname, - birth_town, -birth_country)

########## 1D
########## Print the row(s) that has the fastest Tour de France winner alive today
tdf_winners_fastest_alive <- tdf_winners %>%
  filter(is.na(died)) %>%
  arrange(desc(`avg_speed_(km/h)`))

head(tdf_winners_fastest_alive, 1)

########## 1E
########## Order the data set from least to greatest in terms of how close the runner up was to the winning time


########## 1F
########## Print the names and ages of the top 10 oldest cyclists 
tdf_winners_top_10 <- tdf_winners %>%
  arrange(desc(age)) %>%
  select(full_name, age) %>%
  top_n(10)

########## 1G
########## Group the data set by nationality with the count of the number of times each country has won 


#------------------------------------------------------------
#------------------------------------------------------------
#------------------------------------------------------------

########## 2A
########## Use the object you created in 1B to create a line graph showing speed by year


########## 2B
########## Make a scatter plot of weight against speed, and sized by age


########## 2C
########## Create a plot to show how the distances have changed over the years


########## 2D (extra credit)
########## Create a plot of your choosing as close to reporting-quality as you can get. 



#------------------------------------------------------------------------
# END OF LAB
#-----------------------------------------------------------------------
