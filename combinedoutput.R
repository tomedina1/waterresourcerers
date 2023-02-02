
library(tidyverse)

source('economics.R')
source('energy.R')



table_output <- function(econ_data, energy_data) {
  
  econ_sum <- econ_data %>% 
    group_by(technology) %>% 
    summarize(capex_sum = round(sum(capex),2),
              opex_sum = round(sum(omex),2))
  
  energy_sum <- energy_data %>% 
    group_by(technology) %>% 
    summarize(energy_sum = round(sum(energyreq),2))
  
  combined <- merge(energy_sum, econ_sum)
  
  return(combined)
}


