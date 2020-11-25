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
    geom_histogram(bins = 100)+
    facet_wrap(~key, scales = "free") +
    theme_wsj() + 
    scale_fill_wsj()
  
      # All histograms show Power-law distribution at different Bin numbers
        # Covid variables scaled by 1000
          # +1 added to Death / Recovered / Active to enable log-transformation
        # Population by 1000000

  View(df)
          
### Histogram check Deaths & Confirmed Cases
  df %>% ggplot() +
    geom_density(aes(x = Ln_Death), fill = "red", bins = 100, alpha = 0.3) +
    geom_density(aes(x = Ln_Confirmed), fill = "blue", bins = 100, alpha = 0.3) +
    labs(x = "Number of Deaths & Confirmed Cases - ln Scales")
  
  
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
    labs(x = "Number of Confirmed Cases (1000s)",
         y = "Number of Registered Deaths (1000s)")


# 2) Deaths - Confirmed Cases -> Level-Log
  df %>% ggplot(  aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x = "Number of Confirmed Cases (1000s) - ln scale",
         y = "Number of Registered Deaths (1000s)")  +
    scale_x_continuous( trans = log_trans(),  
                        breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 3) Deaths - Confirmed Cases -> Log-Level
  df %>% ggplot(  aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x = "Number of Confirmed Cases (1000s) - ln scale",
         y = "Number of Registered Deaths (1000s)")  +
    scale_y_continuous( trans = log_trans(),  
                        breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )
  
# 4) Deaths - Confirmed Cases -> Log-Log
  df %>% ggplot(  aes(x = Confirmed, y = Death)) +
    geom_point() +
    geom_smooth(method="loess") +
    labs(x = "Number of Confirmed Cases (1000s) - ln scale",
         y = "Number of Registered Deaths (1000s)")  +
    scale_x_continuous( trans = log_trans(),  
                        breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) ) + 
    scale_y_continuous(trans = log_trans())

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
  
  
# Take log of Deaths & Confirmed Cases:
  df <- df %>% mutate(Ln_Death     = log(Death),
                      Ln_Confirmed = log(Confirmed))

  
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

data_out <- paste0(getwd(),"/")
htmlreg( list(reg1 , reg2 , reg3 , reg4 ),
         type = 'html',
         custom.model.names = c("Nr. of Reg. Cases Log-Log linear",
                                "Nr. of Reg. Cases Log-Log - quadratic",
                                "Nr. of Reg. Cases Log-Log - PLS",
                                "Nr. of Reg. Cases Log-Log linear - Pop. weighted"),
         caption = "Modelling Countries' Covid-Related Deaths & Registered Cases ",
         file = paste0( data_out ,'CovidDeaths_vs_Cases_model_compare.html'), include.ci = FALSE)


######
# Based on model comparison our chosen model is reg4 - Ln_Death ~ Ln_Confirmed (weights = Population)
#   Substantive: - log-log interpretation works properly for countries 
#                   - magnitude of coefficients are meaningful
#                   - Weights changes regression interpretations to country individuals
#                     - Residual analysis reflects on individuals likelihood to die
#                   - Interpretation: With 10% increase in cases, 
#                     - deaths are expected to increase 9.55% among population 
#   Statistical: - Highest R2 + negates need to remove outlier observations
#                   - Weights minimize effect of small but unusually well-preforming countries




######
# Residual analysis.

# lm_robust output is an `object` or `list` with different elements
# Check the `Value` section
?lm_robust


df$reg1_y_pred <- reg1$fitted.values
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_y_pred

# df$Ln_Death - df$reg1_y_pred
df$Ln_Death - df$reg4_y_pred 

# df$reg1_res <- df$Ln_Death - df$reg1_y_pred 
df$reg4_res <- df$Ln_Death - df$reg4_y_pred 

## Residual Plotting - Simple vs Wegihted Log-Log Regression  
df %>% ggplot(aes(x = Ln_Confirmed), alpha = 0.5) +
  geom_point(aes(y = reg1_res), color = 'red', alpha = 0.5) +
  geom_point(aes(y = reg4_res), color = 'blue', alpha = 0.5) +
  geom_hline(yintercept = 0) +
  labs(x = "Nr of Confirmed Cases - ln Scale",
       y = "Regression Residuals - Models 1 & 4")
      # Identical residual distribution w shifting mean

## Residual Histograms
df %>% ggplot( alpha = 0.5) +
  geom_density(aes(x = reg1_res), fill = 'red', alpha = 0.3) + 
  geom_vline(xintercept = 0.1) +
  geom_density(aes(x = reg4_res), fill = 'blue', alpha = 0.3) +
  geom_vline(xintercept = -0.3) +
  labs(x = "Regression Model residuals",
       y = "Errors' Kernel Density") +
  scale_x_continuous(breaks = (-8:8)/2)

rbind(summary(df$reg1_res),
summary(df$reg4_res))


# Find countries with largest negative errors - reg4 - Log-Log W-OLS
df %>% top_n( -5 , reg4_res ) %>% 
  select( Country , Ln_Death , reg4_y_pred , reg4_res)

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
  select( Country , Ln_Death , reg4_y_pred , reg4_res)


#################################
#### HYPOTHESIS TESTING ####

# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg4 )
summary(reg1)

# 2) Coefficient is equal to your favorite value
# Let test: H0: ln_gdppc = 5, HA: ln_gdppc neq 5
linearHypothesis( reg4 , "Ln_Confirmed = 0")








