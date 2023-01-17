
# Data Fitting for functions

library(tidyverse)
library(readxl)

#############################################################
# BWRO CAPEX Linear Model
fits <- read_xlsx('linearmodels.xlsx')

plot(log(fits$capacity), log(fits$capex))

fits_modified <- fits %>% 
  mutate(logx = log(capacity),
         logy = log(capex))

bwrocapex.lm <- lm(logy ~ logx, data = fits_modified) 
bwrocapex <- broom::tidy(bwrocapex.lm)

#############################################################
# BWRO OMEX MODEL