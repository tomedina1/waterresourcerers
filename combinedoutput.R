
library(tidyverse)

source('economics.R')
source('energy.R')

###################################################################################

# This function generates a dataframe that is the dt layout 
table_output <- function(econ_data, energy_data, error_data) {
  
  econ_sum <- econ_data %>% 
    group_by(technology) %>% 
    summarize(capex_sum = round(sum(capex),2), # calculates total CAPEX
              opex_sum = round(sum(omex),2), # calculates total OPEX
              capex_error = 0.5 * capex_sum, # upper bounds for CAPEX
              c_lower = 0.3 * capex_sum, # lower bounds for CAPEX
              opex_error = 0.5 * opex_sum, # upper bounds for CAPEX
              o_lower = 0.3 * opex_sum) %>%  # lower bounds for OPEX
    mutate(capex_final = paste0(capex_sum, ' +', capex_error , '/', '-', c_lower), # binds the capex and its error together
           opex_final = paste0(opex_sum, ' +', opex_error, '/', '-', o_lower)) %>%  # binds the opex and its error together
    select(technology, capex_final, opex_final) # filters final dataframe
  
  # joins the energy data and the error data
  final <- full_join(energy_data, error_data, by = 'process') %>% 
    drop_na(energyreq)
  
  energy_sum <- final %>% 
    group_by(technology) %>% 
    summarize(energy_sum = round(sum(energyreq),2), # calcualtes the total energy requirement
              sd = round(sqrt(sum(var ^ 2, na.rm = TRUE)),2)) %>% # calculates the total error (standard deviation)
    mutate(total = paste(energy_sum, '±', sd)) %>% # outputs the final dataframe (mean +/- SD)
    select(technology, total)
  
  combined <- merge(energy_sum, econ_sum) # merges the energy data and the economic data
  
  return(combined)
}
