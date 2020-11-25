########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 2            ##
## Clean   data       ##
########################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)


# Read Raw Files
CovidCSV <- "https://raw.githubusercontent.com/BrunoHelmeczy/CEU_DA2_Assignment_1/main/2_Submit_Folder/Data/Raw/Covid_10_20_2020_raw.csv"
PopCSV <- "https://raw.githubusercontent.com/BrunoHelmeczy/CEU_DA2_Assignment_1/main/2_Submit_Folder/Data/Raw/Population_10_20_2020_raw.csv"

# Covid
  Covid_raw <- read.csv(CovidCSV)
# Population
  Pop_raw <- read.csv(PopCSV)
  
#### Clean Covid Data ####
  
# Check covid data
  glimpse( Covid_raw )
  
# Drop not needed variables
  Covid_raw <- Covid_raw %>% select( -c(FIPS,Admin2,Last_Update,Lat, Long_, Combined_Key, Incidence_Rate, Case.Fatality_Ratio))
  
# Create new data table with 1 observation per country
  Covid_raw_grouped <- Covid_raw %>% group_by(Country_Region) %>% 
    summarise(Confirmed   = sum(Confirmed, na.rm = T),
              Death       = sum(Deaths, na.rm = T),
              Recovered   = sum(Recovered, na.rm = T),
              Active      = sum(Active, na.rm = T))
    
# Re-Summarize data table - Rename Country column
  Covid_raw_grouped <- Covid_raw_grouped %>% summarise(Country   = Country_Region,
                                                       Confirmed = Confirmed,
                                                       Death     = Death,
                                                       Recovered = Recovered,
                                                       Active    = Active)
  View(Covid_raw_grouped)
#### Clean WDI population data ####
      ## Check the observations:
  
# 1) Filter out grouping observations based on using digits
  Pop_raw <- Pop_raw %>% filter( !grepl("[[:digit:]]", Pop_raw$iso2c)  )
View(Pop_raw)
  
  # Some grouping observations are still there, check each of them
  #   HK - Hong Kong, China
  #   OE - OECD members
  #   all with starting X, except XK which is Kosovo
  #   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe
  
# 2) drop specific values
  drop_id <- c("EU","HK","OE")
  Pop_raw <- Pop_raw %>% filter( !grepl( paste( drop_id , collapse="|"), Pop_raw$iso2c ) ) 
  
# 3) drop values with certain starting characters
  # Get the first letter from iso2c
  fl_iso2c <- substr(Pop_raw$iso2c, 1, 1)
  retain_id <- c("XK","ZA","ZM","ZW")
  # Filter out everything which starts X or Z except countries in retain_id
  Pop_raw <- Pop_raw %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                              !grepl( paste( retain_id , collapse="|"), Pop_raw$iso2c ) ) ) 
  
  rm( drop_id, fl_iso2c , retain_id )
  
  # Retain and rename variables which are going to be used later
  Pop_raw <-Pop_raw %>% transmute( Country    = country,
                                   Population = SP.POP.TOTL )



#### MERGE 2 data.tables together ####
  
  
df <- full_join(Covid_raw_grouped,Pop_raw, by = c("Country" = "Country"))
  
  # Correct some country names by hand
  use_name <- c("Congo, Rep.","Congo, Dem. Rep.","Czech Republic","Korea, Rep.","Kyrgyz Republic",
                "Laos","St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines",
                "Slovak Republic","United States","Myanmar")
  
  alter_name <- c("Congo (Brazzaville)","Congo (Kinshasa)","Czechia","Korea, South","Kyrgyzstan",
                  "Lao PDR","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
                  "Slovakia","US","Burma")
  
  # Simply use a for-cycle to change the name for the countries (note: ordering is important)
  for ( i in seq_along( use_name ) ){
    df$Country[ df$Country == alter_name[ i ] ] <- use_name[ i ]
  }
  
  
# Write a for-loop to find those which are partial or complete matches!
# 1) auxillary table for countries without any population value
  aux <- df %>% filter( is.na(Population) )
# 2) Get the name of the countries
  countries_nm <- aux$Country
# 3) Iterate through all potential partial matches
  for ( i in seq_along( countries_nm ) ){
    # Select those observations where partial match exists
    log_select <- str_detect( df$Country , countries_nm[ i ] )
    # Get the population values for partial matches
    c_partial <- df$Population[ log_select ]
    # If there is a match: only two countries are selected and one is missing the other has population:
    if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
      # Replace the missing value with the match
      df$Population[ log_select & is.na(df$Population)] = c_partial[ !is.na( c_partial ) ]
      # Remove the replaced variable
      df <- df %>% filter( !(log_select & is.na( df$Confirmed ) ) )
    }
  }
  
# 4) Check the results:
  df %>% filter( is.na(Population) )
  # These are:
  #   a) cruiser ships which stuck in national territory (Diamond Princess, MS Zaandam )
  #   b) disputed territories which are accepted by covid statistics but not by world bank 
  #       (Western Sahara, Taiwan or Kosovo)
  #   c) we have no population data on them (Ertirea, Holy See (Vatican))
  
#####
# Handle missing values:
  View( df %>% filter( !complete.cases(df) ) )
# Drop if population, confirmed cases or death is missing
  df <- df %>% filter( !( is.na( Population ) | is.na( Confirmed ) | is.na( Death ) ))

  
# Add 1 to Death / Recovered / Active to enable log transformations
    # Deaths: 12x 0s / Recovered: 3x 0s / Active: 2x 0s
  df <- df %>% transmute( Country     = Country,
                          Confirmed   = Confirmed/1000,
                          Death       = (Death)/1000,
                          Recovered   = (Recovered)/1000,
                          Active      = (Active)/1000,
                          Population  = Population/1000000)    
  summary(df)

  View(df)  
# Save clean data
getwd()
setwd("C:/Users/helme/Desktop/CEU/FALL_TERM/Data_Analysis/DA2/CEU_DA2_Assignment_1/2_Submit_Folder")
write.csv( df , 'Data/Clean/covid_pop_10_20_2020_clean.csv')
  
  