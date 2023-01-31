
library(tidyverse)

source('economics.R')
source('energy.R')

econ <- economics_techplot(data$a, data$b, data$c, 50, data$oma,
                   data$omb, data$omc, data$name, c('ultrafiltration', 'reverse osmosis', 'uv oxidation'), 
                   c('microfiltration', 'reverse osmosis', 'uv oxidation', 'groundwater recharge'), 
                   c('groundwater pumping', 'brackish water desalination'), c('seawater desalination'), tech, 
                   c('Direct Potable Reuse', 'Indirect Potable Reuse', 'Groundwater Desalination', 'Ocean Desalination'), data$model)

plot_data <- technology_plot(c('ultrafiltration', 'reverse osmosis', 'uv oxidation'), 
                             c('microfiltration', 'reverse osmosis', 'uv oxidation', 'groundwater recharge'), 
                             c('groundwater pumping', 'brackish water desalination'), c('seawater desalination'),
                             data, tech, 50, 0.6, 0.06, 0.2, 
                             50, 0.7, c('Direct Potable Reuse', 'Indirect Potable Reuse', 'Groundwater Desalination', 'Ocean Desalination'))


table_output <- function(econ_data, energy_data) {
  
  econ_sum <- econ_data %>% 
    group_by(technology) %>% 
    summarize(capex_sum = sum(capex),
              opex_sum = sum(omex))
  
  energy_sum <- energy_data %>% 
    group_by(technology) %>% 
    summarize(energy_sum = sum(energyreq))
  
  combined <- merge(energy_sum, econ_sum)
  
  return(combined)
}

test <- table_output(econ, plot_data)