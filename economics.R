 
# ECONOMICS 
# Taylor Medina

library(tidyverse)

# SECTION 1: ECONOMIC EQUATIONS AND FUNCTIONS
# ------------------------------------------------------------------------------------------------------------------------

# FUNCTION 1: CAPEX AND O&M COST CALCULATIONS
# this function calculates the capital costs and the o&m costs depending on the inputs
# Do not be intimidated by the for loops - I hope the code is commented clearly enough
# This function requires vector inputs for a, b, c, and year. 
# a, b, and c are fitted constants unique to each unit process
# year is needed to account for inflation 

calculate_costs <- function(a, b, c, x, year, model){
  
  # generate a blank data frame
  costs <- data.frame()
  
  for (i in 1:length(a)) { # iterates through every input in the vector (a, b, c, and year should be the same length***)

      if (model == 1) { # c is only a constant in the Williams Power Logarithmic Rule Equation (Guo et. al. 2014)
        
        x <- x * 3785.4 # convert MGD to m3/d (the equation only works in m3/d)
        
        # William's Power Logarithmic Rule
        # log(y) = a * log(x) ^ b + c
        # y: $
        # x: volumetric flow rate (m3/d)
        
        y <- a[i] * log10(x) ^ (b[i]) + c[i] # calculates log(y)
        unlog_y <- 10 ^ y 
        final_y <- 1.25 * unlog_y * 1e-6 # 2014 dollar to 2022 dollar (October)
        costs <- rbind(costs, final_y) # binds the output for each iteration to the blank df
        
        x <- x / 3785.4 # convert back to MGD
        
      } else if (model == 2) { # the rest of the equations do not have a value for c
        
        # based off of the equation y = a * x ^ b (Plumlee et. al. 2014, Hilbig et. al. 2020)
        # y: $M/MGD
        # multiply y by 1e6 and by x to get costs
        
        y_value <- a[i] * x ^ (b[i]) 
        y_conversion <- 1.25 * y_value # converts from 2014 dollars to current dollar (2022 October)
        costs <- rbind(costs, y_conversion) # binds the output for each iteration to the blank df
      
      } else {
      
        y <- a[i] * x ^ (b[i])
        final_y <- 1.38 * y / x
        
        omy <- oma[i] * x ^ (omb[i])
        final_omy <- 1.38 * y / x
        
        process.df <- rbind(process.df, name[i]) # binds process name to df
        capex.df <- rbind(capex.df, final_y) # binds capex cost to df
        om.df <- rbind(om.df, final_omy) # binds o&m cost to df
    }}
  
  costs_sum <- sum(costs) # sums the dataframe to get the total cost of the system
  return(costs_sum)
  
}


# FUNCTION 2: PLOT FOR CAPEX AND O&M
# this function calculates the CAPEX and O&M and puts them in a df to be plotted
# The equations are the same as in FUNCTION 1

economics_plot <- function(a, b, c, x, oma, omb, omc, name, model) {
  
  process.df <- data.frame() # generate a blank df for the names of the processes
  capex.df <- data.frame() # generate a blank df for the capex values
  om.df <- data.frame() # generate a blank df for the o&m values
  
  for (i in 1:length(name)) {
    
    if (is.na(model[i])) {
      next
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
      
      y <- a[i] * x ^ (b[i])
      final_y <- 1.38 * y / x
      
      omy <- oma[i] * x ^ (omb[i])
      final_omy <- 1.38 * omy / x
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df
      
    } else if (model[i] == 4) {
      
      x <- x * 3785.4 
      y <- exp(a[i] * log(x) + b[i]) 
      
      omy <- oma[i] * x + omb[i]

      x <- x / 3785.4 
      final_y <- y / x
      final_omy <- omy / x
      
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

# This function generates the dataframe for the economics plot
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


# this function uses economics plot data and generates the error bars (-30% / + 50%)
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

