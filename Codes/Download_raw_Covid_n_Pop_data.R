########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 1            ##
## Get the data       ##
########################


# Clear memory and call packages
rm(list=ls())
library(WDI)
library(tidyverse)


# Download COVID cross-sectional data
date <- '10-20-2020'
covid_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
                    , date,'.csv')
covid_raw <- read.csv(covid_url)


# Download population data for 2019
pop_raw <- WDI(indicator = c('SP.POP.TOTL'),
               country = "all", start = 2019, end = 2019)

# Save raw files
getwd()
write.csv(covid_raw,paste0(getwd(), "/", 'Covid_10_20_2020_raw.csv'))

write.csv(pop_raw,paste0(getwd(), "/", 'Population_10_20_2020_raw.csv'))


# Load raw files for cleaning
CovidRaw <- read.csv('Covid_10_20_2020_raw.csv')
PopRaw <- read.csv('Population_10_20_2020_raw.csv')

View(CovidRaw)
View(PopRaw)
