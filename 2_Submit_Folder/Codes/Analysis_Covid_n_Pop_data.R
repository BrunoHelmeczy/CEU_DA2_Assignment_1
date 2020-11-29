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
  library(knitr)

# Read Cleaned Data file from GitHub
  CovidCSV <- "https://raw.githubusercontent.com/BrunoHelmeczy/CEU_DA2_Assignment_1/main/2_Submit_Folder/Data/Clean/covid_pop_10_20_2020_clean.csv"
  df <- read_csv(CovidCSV)
  df <- df %>% select( -X1)
  
  ####
  # 
# Quick check on all HISTOGRAMS
  df  %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    geom_histogram(bins = 100)+
    facet_wrap(~key, scales = "free") +
    theme_wsj() + 
    scale_fill_wsj()
  
      # All histograms show Power-law distribution at different Bin numbers
        # Covid variables scaled by 1000
          # +1 added to Death / Recovered / Active to enable log-transformation
        # Population by 1000000


            
### Histogram check Deaths & Confirmed Cases

# Confirmed Deaths (1000s)  
  df %>% ggplot() +
    geom_histogram(aes(x = Death), fill = "red"
                   ,  alpha = 0.3, binwidth = 10) +
    labs(x = "Number of Confirmed Deaths (1.000s)",
         y = "Number of Countries") 

# Death Stats:
DeathStats <-  
  df %>% summarise(
    Variable  = "Covid-19 Deaths (1000s)",
    Min       = min(Death,na.rm = T),
    '1st IQR'  = quantile(Death, 0.25, na.rm = T),
    Median    = median(Death,na.rm = T),
    '3rd IQR' = quantile(Death,0.75, na.rm = T),
    Max       = max(Death,na.rm = T),
    Mean      = mean(Death,na.rm = T),
    StDev     = sd(Death,na.rm = T)
  )

# Confirmed Cases (1000s)
  df %>% ggplot() +
    geom_histogram(aes(x = Confirmed), fill = "blue"
                   , alpha = 0.3, binwidth = 500) +
    labs(x = "Number of Confirmed Cases (1.000s)",
         y = "Number of Countries")
  
# CONIFRMED Stats
CasesStats <-  
  df %>% summarise(
    Variable  = "Covid-19 Cases (1000s)",
    Min       = min(Confirmed,na.rm = T),
    '1st IQR'  = quantile(Confirmed, 0.25, na.rm = T),
    Median    = median(Confirmed,na.rm = T),
    '3rd IQR' = quantile(Confirmed,0.75, na.rm = T),
    Max       = max(Confirmed,na.rm = T),
    Mean      = mean(Confirmed,na.rm = T),
    StDev     = sd(Confirmed,na.rm = T)
  )
  
  
# Population
  df %>% ggplot() +
    geom_histogram(aes(x = Population), fill = "green"
                   , alpha = 0.3, bins = 50) +
    labs(x = "Population (1.000.000s)",
         y = "Number of Countries")
  
# Population Stats
  
PopStats <- df %>% summarise(
    Variable  = "Population (1M)",
    Min       = min(Population,na.rm = T),
    '1st IQR'  = quantile(Population, 0.25, na.rm = T),
    Median    = median(Population,na.rm = T),
    '3rd IQR' = quantile(Population,0.75, na.rm = T),
    Max       = max(Population,na.rm = T),
    Mean      = mean(Population,na.rm = T),
    StDev     = sd(Population,na.rm = T)
  )
  
SummStats <- PopStats %>% add_row(CasesStats) %>% add_row(DeathStats)
  
  
  
  summary( df )
  
######
# Check basic scatter-plots!
  # Y = Number of Registered Deaths
  # X = Number of Registered Cases
  # Where to use log-transformation? - 
    # level-level vs level-log vs log-level vs log-log
  
  
# 1) Deaths - Confirmed Cases -> Level-Level 
df %>% ggplot(aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method = "loess") +
    labs(x = "Nr. of Confirmed Cases (1000s)",
         y = "Nr. of Registered Deaths (1000s)")


# 2) Deaths - Confirmed Cases -> Level-Log
  df %>% ggplot(  aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x = "Nr. of Confirmed Cases (1000s) - ln scale",
         y = "Nr. of Registered Deaths (1000s)")  +
    scale_x_continuous( trans = log_trans(),  
                        breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 3) Deaths - Confirmed Cases -> Log-Level
  df %>% ggplot(  aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x = "Nr. of Confirmed Cases (1000s) - ln scale",
         y = "Nr. of Registered Deaths (1000s)")  +
    scale_y_continuous( trans = log_trans(),  
                        breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )
  
# 4) Deaths - Confirmed Cases -> Log-Log
  df %>% ggplot(  aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x = "Nr. of Confirmed Cases (1000s) - ln scale",
         y = "Nr. of Registered Deaths (1000s)")  +
    scale_x_continuous( trans = log_trans(),  
                        breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) ) + 
    scale_y_continuous(trans = log_trans(),
                       breaks = c(1,2,5,10,20,50,100,200,500,1000,10000))

####
# Conclusions:
# Log-Log transformation seems most sensible
#   - Both Var.s show strong power-law distribution 
#        - LOESS visual suggests 
#           - very Weakly S-shaped curve
#             - could be due fewer observations in bin-interval 
#           - Linear Model seems a good fit - loess-tails notably inaccurate
#           - Other Candidates: 
#             - Quadratic: Slight positive curvature
#             - Spline: Slight curvature could be due to break in middle
#             +1: USE is notoriously badly-handling virus 
#                   - weight by population may impact results much
  
  
# Take log of Deaths & Confirmed Cases - Filter out Countries with 0 Deaths:
  df <- df %>% filter(Death > 0) %>% 
                  mutate(Ln_Death     = log(Death),
                         Ln_Confirmed = log(Confirmed)) %>% 
                    arrange(Ln_Death)

  
# Outline Models:

  # Make some models:
  
# Log-Log Simple Linear Regression
  # reg1: Ln_Death = alpha + beta * Ln_Confirmed

# Log-Log Linear Regression - Quadratic Form
  # reg2: Ln_Death = alpha + beta_1 * Ln_Confirmed + beta_2 * Ln_Confirmed^2

# Log-Log Linear Regression - Piece-wise Linear Splines - Knots at 2 & 30 (1000s Scale)
  # reg3: Ln_Death = alpha_1 + beta_1 * Ln_Confirmed  * 1(Ln_Confirmed < 2) +
    #               (alpha_2 + beta_2 * Ln_Confirmed) * 1(Ln_Confirmed < 30) +
    #               (alpha_3 + beta_3 * Ln_Confirmed) * 1(Ln_Confirmed >= 30)
  
# Log-Log Weighted Linear Regression - Weights = Population 
  # reg4: Ln_Death = alpha + beta * Ln_Confirmed, weights: Population
  

# Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( Ln_Confirmed_sq = Ln_Confirmed^2)


#### MODELS ####

###  REG 1: Log-Log Linear Regression ###
reg1 <- lm_robust( Ln_Death ~ Ln_Confirmed , data = df , se_type = "HC2" )
reg1            # Summary statistics
summary( reg1 ) # Visual inspection:
ggplot( data = df, aes( x = Ln_Confirmed, y = Ln_Death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )


###  REG 2: Log-Log Linear Regression ###
reg2 <- lm_robust( Ln_Death ~ Ln_Confirmed + Ln_Confirmed_sq , data = df, se_type = "HC2" )
summary( reg2 )
ggplot( data = df, aes( x = Ln_Confirmed, y = Ln_Death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

###  REG 3: Log-Log Linear Regression - Piece-wise Linear Spline ###
  # Create Cutoff & Log Cutoffs 
cutoff <- c(2,20)
cutoff_ln<- log( cutoff )

reg3 <- lm_robust(Ln_Death ~ lspline( Ln_Confirmed , cutoff_ln ), data = df, se_type = "HC2"  )
reg3
summary( reg3 )
ggplot( data = df, aes( x = Ln_Confirmed, y = Ln_Death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )


###  REG 4: Log-Log Linear Regression - Population as Weights ###
reg4 <- lm_robust(Ln_Death ~ Ln_Confirmed, data = df , weights = Population, se_type = "HC2" )
reg4
summary( reg4 )

ggplot(data = df, aes(x = Ln_Confirmed, y = Ln_Death)) +
  geom_point(data = df, aes(size= Population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = Population), method = "lm", color='red')


### REG 1 vs REG 4 ###
ggplot(data = df, aes(x = Ln_Confirmed, y = Ln_Death)) +
  geom_point(data = df, aes(size= Population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = Population), method = "lm", color='red') +
  geom_smooth(method = "lm", color = 'green')


# Model Summary using TexReg #
library(jtools)
library(huxtable)
exptbl <- export_summs(reg1, reg2,reg3,reg4,
                       model.names = c("Ln_Deaths",
                                       "Ln_Deaths",
                                       "Ln_Deaths",
                                       "Ln_Deaths"))





######
# Based on model comparison our chosen model is reg4 - Ln_Death ~ Ln_Confirmed (weights = Population)
#   Substantive: - log-log interpretation works properly for countries 
#                   - magnitude of coefficients are meaningful
#                   - Weights change regression interpretation to country individuals
#                     - Residual analysis reflects on individuals likelihood to die
#                   - Interpretation: With 10% increase in cases, 
#                     - deaths are expected to increase 9.55% among population 
#   Statistical: - Highest R2 + negates need to remove outlier observations
#                   - Weights minimize effect of small but unusually well-preforming countries


paste0("ln(Death) = ",round(as.numeric(reg4$coefficients[1]),2)
       ," + ", round(as.numeric(reg4$coefficients[2]),2)
       ," * ln(Confirmed Cases)")

reg4$coefficients[2]

names(reg4$coefficients[2])

######
# Residual analysis.

df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model

df$Ln_Death - df$reg4_y_pred 

df$reg4_res <- df$Ln_Death - df$reg4_y_pred 

# Find countries with largest negative errors - reg4 - Log-Log W-OLS
best <- df %>% top_n( -5 , reg4_res ) %>% 
  select( Country,  Ln_Death , reg4_y_pred , reg4_res)  %>% 
  summarise( BestCountries = Country,
             Ln_Deaths = Ln_Death,
             Pred_Value = reg4_y_pred,
             Pred_Error = reg4_res)

# Find countries with largest positive errors
worst <- df %>% top_n( 5 , reg4_res ) %>% 
  select( Country, Ln_Death , reg4_y_pred , reg4_res) %>% 
  summarise( WorstCountries = Country,
             Ln_Deaths = Ln_Death,
             Pred_Value = reg4_y_pred,
             Pred_Error = reg4_res)

df %>% 
  select( Country, Ln_Death , reg4_y_pred , reg4_res) %>% 
  summarise( WorstCountries = Country,
             Ln_Deaths = Ln_Death,
             Pred_Value = reg4_y_pred,
             Pred_Error = reg4_res,
             rank(df$reg4_res)) %>% 
  arrange(Pred_Error) %>% 
  filter(WorstCountries ==  c("China", "United States" ))

View(df)


rank(df$reg4_res)


cbind(best, worst)


#################################
#### HYPOTHESIS TESTING ####

# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg4 )
round(as.data.frame(summary( reg4 )[[12]]),4)




pnorm(5.4,0,1)


