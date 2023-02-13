
library(tidyverse)

source('economics.R')
source('energy.R')



table_output <- function(econ_data, energy_data, error_data) {
  
  econ_sum <- econ_data %>% 
    group_by(technology) %>% 
    summarize(capex_sum = round(sum(capex),2),
              opex_sum = round(sum(omex),2),
              capex_error = 0.5 * capex_sum,
              c_lower = 0.3 * capex_sum,
              opex_error = 0.5 * opex_sum,
              o_lower = 0.3 * opex_sum) %>% 
    mutate(capex_final = paste0(capex_sum, ' +', capex_error , '/', '-', c_lower),
           opex_final = paste0(opex_sum, ' +', opex_error, '/', '-', o_lower)) %>% 
    select(technology, capex_final, opex_final)
  
  final <- full_join(energy_data, error_data, by = 'process') %>% 
    drop_na(energyreq)
  
  energy_sum <- final %>% 
    group_by(technology) %>% 
    summarize(energy_sum = round(sum(energyreq),2),
              sd = round(sqrt(sum(var ^ 2, na.rm = TRUE)),2)) %>% 
    mutate(total = paste(energy_sum, '±', sd)) %>% 
    select(technology, total)
  
  combined <- merge(energy_sum, econ_sum)
  
  return(combined)
}
