# loading initial packages I know I'm going to be using
if (!require(readr)) install.packages('readr')
if (!require(ggplot2)) install.packages('ggplot2')

library(readr)
library(ggplot2)

# reading in covid data
covidData <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')

# making OK specific covid data
covidData_OK <- covidData[covidData$state == 'Oklahoma',]

# initial cases graph
ggplot(data = covidData_OK) + 
  geom_line(mapping = aes(x = date, y = cases))

# NY specific covid data
covidData_NY <- covidData[covidData$state == 'New York',]

# NY cases graph
ggplot(data = covidData_NY) + 
  geom_line(mapping = aes(x = date, y = cases))

# making yesterday snapshot
covidData_yesterday <- covidData[covidData$date == as.character(Sys.Date()-1),]

# adding trend line
ggplot(data = covidData_yesterday) + 
  geom_point(mapping = aes(x = cases, y = deaths)) + 
  geom_smooth(mapping = aes(x = cases, y = deaths))

# needed to load this package to filter()
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

# filtering NY out of yesterday snapshot
covidData_yesterdayNoNY <- covidData_yesterday %>% filter(state != 'New York')

# same plot as before but without Ny
ggplot(data = covidData_yesterdayNoNY) + 
  geom_point(mapping = aes(x = cases, y = deaths)) + 
  geom_smooth(mapping = aes(x = cases, y = deaths))

# finding which state was so high in deaths
covidData_yesterday[covidData_yesterday$deaths>30000,]

# looking at OK
covidData_yesterday[covidData_yesterday$state=='Oklahoma',]

# getting state populations data
stPops <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2020_Fall/20200821_Analytics%20with%20Excel_2/nst-est2019-alldata.csv')

# filtering out rows and columns we don't need
stPops <- stPops[-c(1:5,nrow(stPops)),] %>% select(NAME, POPESTIMATE2019)

# joining to main dataset
covidData <- left_join(covidData,stPops, by = c('state'='NAME'))

# calculating a concentrations column
covidData$per100k <- round(covidData$cases / covidData$POPESTIMATE2019 * 1000,2)

# getting rid of rows where per100k column is NA 
covidData <- covidData[!is.na(covidData$per100k),]

# reading in voting data
votes <- read_csv('https://raw.githubusercontent.com/drehow/CSC-201/master/2021_01/20210201_RStudio/1976-2020-president.csv')


# needed some functions from these libraries based on examples I found online
if (!require(scales)) install.packages('scales')
if (!require(tidyr)) install.packages('tidyr')

library(scales)
library(tidyr)

# cleaning votes data set
votes <- votes %>% 
  filter(year == 2020, writein == F, (candidate == 'TRUMP, DONALD J.' & party_detailed =='REPUBLICAN') | (candidate == 'BIDEN, JOSEPH R. JR' & party_detailed=='DEMOCRAT')) %>%
  select(state, party_detailed, candidatevotes) %>%
  spread(party_detailed, candidatevotes, fill=NA) 

names(votes) <- tolower(names(votes))
# setting to binary dem/rep column
votes <- votes %>% 
  mutate(voted = ifelse(votes$democrat > votes$republican,'democrat','republican')) %>% 
  select(state, voted)

# need the capitalization in both state columns to be the same so they will join correctly
votes$state <- tolower(votes$state)
covidData$state <- tolower(covidData$state)

# joining to main dataset
covidData <- left_join(covidData, votes)

# get rid of rows with NA in voted column
covidData <- covidData[!is.na(covidData$voted),]

# changing to better column name
covidData$population <- covidData$POPESTIMATE2019
covidData$POPESTIMATE2019 <- NULL

# starting to build my final plot
ggplot(data = covidData) + 
  geom_point(mapping = aes(x = date, y = per100k
                           #, group = state, color = state
  ))

# changing to lines
ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k
  )
  )

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k
                          , group = state)
  )

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state, color = voted))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
  ))+
  labs(title="COVID Cases by State", subtitle="Data as of 2/5/2021", y="Cases per 100k People", x="Date", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") 

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
  ))+
  labs(title="COVID Cases by State", subtitle="Data as of 2/5/2021", y="Cases per 100k People", x="Date", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) 

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
  ))+
  labs(title="COVID Cases by State", subtitle="Data as of 2/5/2021", y="Cases per 100k People", x="Date", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%B"),
               limits = as.Date(c('2020-03-15','2021-02-04')))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
  ))+
  labs(title="COVID Cases by State", subtitle="Data as of 2/5/2021", y="Cases per 100k People", x="Date", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%B"),
               limits = as.Date(c('2020-03-15','2021-02-04'))) +
  theme(legend.position = c(0.11, 0.67))

ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, y = per100k, group = state
                          , color = voted, alpha = population
  ))+
  labs(title="COVID Cases by State", subtitle="Data as of 2/5/2021", y="Cases per 100k People", x="Date", caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%B"),
               limits = as.Date(c('2020-03-15','2021-02-04'))) +
  theme_classic() +
  theme(legend.position = c(0.11, 0.67))


# this plot uses hex colors. Google "hex color picker" and you'll see how it works. 
ggplot(data = covidData) + 
  geom_line(mapping = aes(x = date, 
                          y = per100k, 
                          group = state
                          , color = voted, 
                          alpha = population
  )) + 
  scale_alpha_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",
               labels = date_format("%B"),
               limits = as.Date(c('2020-03-15','2021-02-04'))) +
  scale_color_manual(values=c('#002fd9','#d90000')) +
  labs(title="COVID Cases by State", 
       subtitle="Data as of 2/5/2021", 
       y="Cases per 100k People", 
       x="Date", 
       caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  theme_classic() +
  theme(legend.position = c(0.11, 0.67))
########################################################################

# extra plots just for exploring the data

covidData_party <- covidData %>% 
  group_by(date, voted) %>% 
  summarise(per100k = mean(per100k))

ggplot(data = covidData_party) + 
  geom_line(mapping = aes(x = date, y = per100k, group = voted, color = voted))


#########################################################################
# Starting on boxplot vis
covidData_yesterday <- covidData[covidData$date == Sys.Date()-1,]

ggplot(covidData_yesterday, aes(voted, per100k)) +
  geom_boxplot() 

ggplot(covidData_yesterday, aes(voted, per100k)) +
  geom_boxplot() +
  geom_point() 

ggplot(covidData_yesterday, aes(voted, per100k)) +
  geom_boxplot() +
  geom_point(aes(color = voted)) 

ggplot(covidData_yesterday, aes(voted, per100k)) +
  geom_boxplot() +
  geom_point(aes(color = voted)) + 
  theme_classic()

ggplot(covidData_yesterday, aes(voted, per100k)) +
  geom_boxplot() +
  geom_point(aes(color = voted)) + 
  scale_color_manual(values=c('#002fd9','#d90000')) +
  theme_classic()

ggplot(covidData_yesterday, aes(voted, per100k)) +
  geom_boxplot() +
  geom_point(aes(color = voted)) + 
  scale_color_manual(values=c('#002fd9','#d90000')) +
  labs(title="Cases Distribution by Party/State", 
       subtitle="Data as of 2/5/2021", 
       y="Cases per 100k People", 
       x="2016 Popular Vote", 
       caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  theme_classic()
###############################################################################


# starting concentrations by population
ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k))

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population))

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22)

covidData_yesterday_OK <- covidData_yesterday %>% filter(state == 'oklahoma')

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred') 

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) 

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) + 
  geom_text(data = covidData_yesterday_OK, aes(x = population * 1.63, y = per100k, label = state)) 

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22, show.legend = FALSE) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) + 
  geom_text(data = covidData_yesterday_OK, aes(x = population * 1.63, y = per100k*1.07, label = state)) 

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22, show.legend = FALSE) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) + 
  geom_text(data = covidData_yesterday_OK, aes(x = population * 1.63, y = per100k*1.07, label = state)) +
  labs(title="Concentration by Population", 
       subtitle="Size by Population", 
       y="Cases per 100k People", 
       x="2019 Population", 
       caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") 

ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22, show.legend = FALSE) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) + 
  geom_text(data = covidData_yesterday_OK, aes(x = population * 1.63, y = per100k*1.07, label = state)) +
  labs(title="Concentration by Population", 
       subtitle="Size by Population", 
       y="Cases per 100k People", 
       x="2019 Population", 
       caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  theme_light()

g <- ggplot() + 
  geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22, show.legend = FALSE) +
  geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) + 
  geom_text(data = covidData_yesterday_OK, aes(x = population * 1.63, y = per100k*1.07, label = state)) +
  labs(title="Concentration by Population", 
       subtitle="Size by Population", 
       y="Cases per 100k People", 
       x="2019 Population", 
       caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
  scale_x_continuous(labels = comma) +
  theme_light()

(g <- ggplot() + 
    geom_point(data = covidData_yesterday, aes(x = population, y = per100k, size = population),colour = 'black', pch=22, show.legend = FALSE) +
    geom_point(data = covidData_yesterday_OK, aes(x = population, y = per100k), color = 'darkred',fill = 'darkred', size = 3, pch=22) + 
    geom_text(data = covidData_yesterday_OK, aes(x = population * 1.63, y = per100k*1.07, label = state)) +
    labs(title="Concentration by Population", 
         subtitle="Size by Population", 
         y="Cases per 100k People", 
         x="2019 Population", 
         caption="Sources: New York Times, U.S. Census Bureau, Harvard Dataverse") +
    scale_x_continuous(labels = comma) +
    theme_light())


#install.packages('ggExtra')
if(!require(ggExtra)) install.packages('ggExtra')
library(ggExtra)
ggMarginal(g, type = "density", fill="transparent", size = 10)

ggMarginal(g, type = "boxplot", fill="transparent")

ggMarginal(g, type = "boxplot", fill="transparent", size = 11)

################################################################################################

# Start lollipop chart
covidData$percChange <- NA
for(i in 1:nrow(covidData)){
  dt <- covidData$date[i]
  st <- covidData$state[i]
  if(st %in% covidData$state[1:(i-1)]){
    dts <- which(covidData$state[1:(i-1)] == st)
    covidData$percChange[i] <- round(covidData$cases[i]/covidData$cases[max(dts)]-1,4)
  }
}
covidData_yesterday <- covidData[covidData$date == Sys.Date()-1,]

ggplot(covidData_yesterday, aes(x=state, y=percChange)) + 
  geom_point(color = 'black', size=3)  

ggplot(covidData_yesterday, aes(x=state, y=percChange)) + 
  geom_point(color = 'black', size=3)  +
  geom_segment(aes(y = 0, 
                   x = state, 
                   yend = percChange, 
                   xend = state), 
               color = "red") 

ggplot(covidData_yesterday, aes(x=state, y=percChange)) + 
  geom_point(color = 'black', size=3)  +
  geom_segment(aes(y = 0, 
                   x = state, 
                   yend = percChange, 
                   xend = state), 
               color = "red") +
  labs(title="Diverging Lollipop Chart")

ggplot(covidData_yesterday, aes(x=state, y=percChange)) + 
  geom_point(color = 'black', size=3)  +
  geom_segment(aes(y = 0, 
                   x = state, 
                   yend = percChange, 
                   xend = state), 
               color = "red") +
  labs(title="Diverging Lollipop Chart")+ 
  coord_flip()

ggplot(covidData_yesterday, aes(x=state, y=percChange)) + 
  geom_point(color = 'black', size=3)  +
  geom_segment(aes(y = 0, 
                   x = state, 
                   yend = percChange, 
                   xend = state), 
               color = "red") +
  labs(title="Diverging Lollipop Chart")+ 
  coord_flip()+
  theme_light() 

covidData_yesterday <- covidData_yesterday[order(desc(covidData_yesterday$percChange)),] 
covidData_yesterday$state <- factor(covidData_yesterday$state[order(desc(covidData_yesterday$percChange))], levels = factor(covidData_yesterday$state[order(desc(covidData_yesterday$percChange))]))


ggplot(covidData_yesterday, aes(x=state, y=percChange)) + 
  geom_point(color = 'black', size=3)  +
  geom_segment(aes(y = 0, 
                   x = state, 
                   yend = percChange, 
                   xend = state), 
               color = "red") +
  labs(title="Lollipop Chart", 
       subtitle="% Changes in cases from prior day") +
  # geom_text(color="white", size=1.5
  #           ,label=round(covidData_yesterday$percChange,1)) +
  coord_flip()+
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
################################################################################
