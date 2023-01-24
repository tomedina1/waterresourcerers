
# DATA FITS
# Taylor Medina

# load packages
library(tidyverse)
library(readxl)

# load data
fits <- read_xlsx('linearmodels.xlsx')
bwro <- read_xlsx('bwro.xlsx')

#############################################################
# BWRO CAPEX Linear Model

# visualize log fit data to make sure there is a linear fit
plot(log(fits$capacity), log(fits$capex))

# modify the data frame to get log data for the OLS
fits_modified <- fits %>% 
  mutate(logx = log(capacity),
         logy = log(capex))

# perform OLS (log-log) to get the model
bwrocapex.lm <- lm(logy ~ logx, data = fits_modified) 
bwrocapex <- broom::tidy(bwrocapex.lm)

