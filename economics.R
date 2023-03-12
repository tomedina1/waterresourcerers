 
# ECONOMICS 
# Taylor Medina

# load packages
library(tidyverse)
# source energy
source('energy.R')

# FUNCTION 1: GROUNDWATER PUMPING O&M

gw_om <- function(q, k, E, k_f, L) {
  
  energy <- e_gwpump(q, system_losses(k_f, q, k, L), E)
  om <- (0.1974 * energy)  # $ / m3
  om <- om * 3785.41178 # $/MG
  om <- om * 1e-6 # $MM/MGD
  
}



######################################################################################
# FUNCTION 2: CAPEX AND O&M COST CALCULATIONS
# this function calculates the capital costs and the o&m costs depending on the inputs
# Do not be intimidated by the for loops - I hope the code is commented clearly enough
# This function requires vector inputs for a, b, and c. 
# a, b, and c are fitted constants unique to each unit process
#######################################################################################

economics_plot <- function(a, b, c, x, oma, omb, omc, name, model, pump_om) {
  
  process.df <- data.frame() # generate a blank df for the names of the processes
  capex.df <- data.frame() # generate a blank df for the capex values
  om.df <- data.frame() # generate a blank df for the o&m values
  
  for (i in 1:length(name)) {
    
    # if there is no model in the data frame do not run this function
    if (is.na(model[i])) {
      
      next
      
      # if there is a model then run the function 
    } else {
    
    if (model[i] == 1) {
      
      x <- x * 3785.4 # converts from MGD to m3/d
      
      # William's Power Log Rule
      # CAPEX Calculation
      y <- a[i] * log10(x) ^ (b[i]) + c[i] # calculates log(y)
      unlog_y <- 10 ^ y 
      final_y <- 1.25 * unlog_y * 1e-6 # 2014 dollar to 2022 dollar (October)
      
      # O&M Calculation
      omy <- oma[i] * log10(x) ^ (omb[i]) + omc[i] # calculates log(y)
      unlog_omy <- 10 ^ omy 
      final_omy <- 1.25 * unlog_omy * 1e-6 # 2014 dollar to 2022 dollar (October)
      
      x <- x / 3785.4 # convert back to MGD
      
      final_y <- final_y / x
      final_omy <- final_omy / x
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df
      
    } else if (model[i] == 2) {
      
      # Using equation from Plumlee et. al. (2014)
      # CAPEX calculation
      y_value <- a[i] * x ^ (b[i]) 
      y_conversion <- 1.25 * y_value # converts from 2014 dollars to current dollar (2022 October)
      
      # O&M calculation
      omy_value <- oma[i] * x ^ (omb[i]) 
      omy_conversion <- 1.25 * omy_value # converts from 2014 dollars to current dollar (2022 October)
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, y_conversion) # binds capex cost to df
      om.df <- rbind(om.df, omy_conversion) # binds o&m cost to df 
      
    } else if (model[i] == 3) {
      
      # Models  seawater reverse osmosis 
      y <- a[i] * x ^ (b[i])
      final_y <- 1.38 * y / x # converts to 2022 dollars
      
      omy <- oma[i] * x ^ (omb[i])
      final_omy <- 1.38 * omy / x # converts to 2022 dollars
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df
      
    } else if (model[i] == 4) {
      
      # models brackish water (groundwater) desalination
      xm3d <- x * 3785.4 # convert MGD to m3/d
      y <- a[i] * xm3d ^ (b[i])
      omy <- oma[i] * xm3d + omb[i]
      
      # calculates final values 
      final_y <- y / x
      final_omy <- omy * 365 * 1e-6 / x 
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df

    } else if (model[i] == 5) {
      
      omy <- pump_om  
      
      process.df <- rbind(process.df, name[i])
      capex.df <- rbind(capex.df, 0)
      om.df <- rbind(om.df, omy)
      
      
    } else {next}}}
  
  # create column names for the dataframe
  colnames(process.df) <- 'process'
  colnames(capex.df) <- 'capex'
  colnames(om.df) <- 'omex'
  
  # combine data frames together to make the plot data frame
  graph.df <- cbind(process.df, capex.df) %>% 
    cbind(om.df)
  
  return(graph.df)
  
}

#######################################################################################
# FUNCTION 2: CAPEX AND O&M BASED ON TECHNOLOGY
# This function takes the inputs from the shiny app and makes the df for the plots
#######################################################################################

economics_techplot <- function(a, b, c, x, oma, omb, omc, name, input1, 
                               input2, input3, input4, tech, tech_input, model, pump_om) {
  
  # outputs plot data
  plot.data <- economics_plot(a, b, c, x, oma, omb, omc, name, model, pump_om)
  
  # turns inputs into a vector
  input.vector <- c(input1, input2, input3, input4)
  
  # generates a blank dataframe for the output
  tech.df <- data.frame()
  
  for (i in 1:length(input.vector)) {
    
    if (i <= length(input1)) {
      
      data <- plot.data %>% 
        filter(process == input.vector[i]) %>% 
        mutate(technology = tech[1])
      
      tech.df <- rbind(tech.df, data) # adds to the blank data frame
      
    } else if (i > length(input1) & i <= length(input1) + length(input2)) {
      
      data <- plot.data %>% 
        filter(process == input.vector[i]) %>% 
        mutate(technology = tech[2])
      
      tech.df <- rbind(tech.df, data) # add to the blank data frame
      
    } else if (i > length(input1) + length(input2) & i <= length(input.vector) - length(input4)) {
      
      # data wrangling to filter the dataset
      data <- plot.data %>% 
        filter(process == input.vector[i]) %>% 
        mutate(technology = tech[3])
      
      tech.df <- rbind(tech.df, data) # add to the blank data frame
      
    } else {
      
      # data wrangling to filter the dataset
      data <- plot.data %>% 
        filter(process == input.vector[i]) %>% 
        mutate(technology = tech[4])
      
      tech.df <- rbind(tech.df, data) # add to the blank data frame
      
    }}
  
  tech.df <- tech.df %>% 
    filter(technology %in% tech_input)
  
  return(tech.df)
        
}

#######################################################################################
# FUNCTION 3: This function makes the economics error bars for the plot
# this function uses economics plot data and generates the error bars (-30% / + 50%)
#######################################################################################

econ_errorbars <- function(econ_data) {
  
  error <- econ_data %>% 
    group_by(technology) %>% 
    summarize(capex_sum = sum(capex),
              omex_sum = sum(omex),
              capex_lower = capex_sum - 0.3 * capex_sum,
              capex_upper = capex_sum + 0.5 * capex_sum,
              omex_lower = omex_sum - 0.3 * omex_sum,
              omex_upper = omex_sum + 0.5 * omex_sum)

  return(error)
  
}

#######################################################################################
# FUNCTION 4: This function is used for biological activated carbon and modifies the 
# frame depending on what the selected flow rate is and the empty bed contact time 
#######################################################################################

# BIOLOGICAL ACTIVATED CARBON ECONOMIC DATA
bac_econ <- function(flowrate, ebct, data) {
  
  if (flowrate <= 10) {
    
    bac_data <- data %>% 
      mutate(a = replace(a, name == 'biological activated carbon', 
                         ifelse(ebct == 20, 3.03, 2.92)),
             b = replace(b, name == 'biological activated carbon',
                         ifelse(ebct == 20, -0.48, -0.52)),
             oma = replace(oma, name == 'biological activated carbon',
                         ifelse(ebct == 20, 0.085, 0.074)),
             omb = replace(omb, name == 'biological activated carbon',
                         ifelse(ebct == 20, -0.16, -0.19)))
  }
  
  else {
    
    bac_data <- data %>% 
      mutate(a = replace(a, name == 'biological activated carbon', 
                         ifelse(ebct == 20, 1.52, 1.43)),
             b = replace(b, name == 'biological activated carbon',
                         ifelse(ebct == 20, -0.15, -0.17)),
             oma = replace(oma, name == 'biological activated carbon',
                           ifelse(ebct == 20, 0.070, 0.059)),
             omb = replace(omb, name == 'biological activated carbon',
                           ifelse(ebct == 20, -0.036, -0.044)))
  }
  
  return(bac_data)
  
}


