########################
## Assignment for DA2 ##
##  & for Coding 1    ##
##                    ##
##   NO. 3            ##
## Analysis           ##
########################


# Clear memory
  rm(list=ls())

# Install needed Packages
  # install.packages("lspline")
  # install.packages("estimatr")
  # install.packages("texreg")
  # install.packages(ggthemes)


# Packages to use
  library(tidyverse)
  require(scales)         # For scaling ggplots
  library(lspline)        # To Estimate piece-wise linear splines
  library(estimatr)       # Estimate robust SE
  library(texreg)         # Compare models with robust SE
  library(ggthemes)       # For different themes

# Read Cleaned Data file from GitHub
  CovidCSV <- "https://raw.githubusercontent.com/BrunoHelmeczy/CEU_DA2_Assignment_1/main/2_Submit_Folder/Data/Clean/covid_pop_10_20_2020_clean.csv"
  df <- read_csv(CovidCSV)
  df <- df %>% select( -X1)
  View(df)
  ####
  # 
# Quick check on all HISTOGRAMS
  df  %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    facet_wrap(~key, scales = "free") +
    geom_histogram()+
    theme_wsj() + 
    scale_fill_wsj()
  
  summary( df )
  




