 
# ECONOMICS 
# Taylor Medina

# load packages
library(tidyverse)

# source the fits script - used in the equation
source('fits.R')

######################################################################################
# FUNCTION 1: CAPEX AND O&M COST CALCULATIONS
# this function calculates the capital costs and the o&m costs depending on the inputs
# Do not be intimidated by the for loops - I hope the code is commented clearly enough
# This function requires vector inputs for a, b, c, and year. 
# a, b, and c are fitted constants unique to each unit process
#######################################################################################

economics_plot <- function(a, b, c, x, oma, omb, omc, name, model) {
  
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
      
      # Models purely swro 
      y <- a[i] * x ^ (b[i])
      final_y <- 1.38 * y / x
      
      omy <- oma[i] * x ^ (omb[i])
      final_omy <- 1.38 * omy / x
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df
      
    } else if (model[i] == 4) {
      
      # bwro model 
      # based off of the fits.R file - log-log fit of Texas BWRO data
      y <- exp(bwrocapex$estimate[2] * log(x) + bwrocapex$estimate[1]) / 1e6
      
      omy <- oma[i] * x + omb[i] 
      
      final_y <- y / x
      final_omy <- omy / (x * 1e6)
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df
      
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
                               input2, input3, input4, tech, tech_input, model) {
  
  # outputs plot data
  plot.data <- economics_plot(a, b, c, x, oma, omb, omc, name, model)
  
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





