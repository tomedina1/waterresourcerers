 
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

calculate_costs <- function(a, b, c, x, year){
  
  # generate a blank data frame
  costs <- data.frame()
  
  for (i in 1:length(a)) { # iterates through every input in the vector (a, b, c, and year should be the same length***)

      if (c[i] != 0) { # c is only a constant in the Williams Power Logarithmic Rule Equation (Guo et. al. 2014)
        
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
        
      } else { # the rest of the equations do not have a value for c
        
        # based off of the equation y = a * x ^ b (Plumlee et. al. 2014, Hilbig et. al. 2020)
        # y: $M/MGD
        # multiply y by 1e6 and by x to get costs
        
        y_value <- a[i] * x ^ (b[i]) 
        y_conversion <- 1.25 * y_value # converts from 2014 dollars to current dollar (2022 October)
        costs <- rbind(costs, y_conversion) # binds the output for each iteration to the blank df
      
    }}
  
  costs_sum <- sum(costs) # sums the dataframe to get the total cost of the system
  return(costs_sum)
  
}


# FUNCTION 2: PLOT FOR CAPEX AND O&M
# this function calculates the CAPEX and O&M and puts them in a df to be plotted
# The equations are the same as in FUNCTION 1

economics_plot <- function(a, b, c, x, oma, omb, omc, name) {
  
  process.df <- data.frame() # generate a blank df for the names of the processes
  capex.df <- data.frame() # generate a blank df for the capex values
  om.df <- data.frame() # generate a blank df for the o&m values
  
  for (i in 1:length(name)) {
    
    if (c[i] != 0) {
      
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
      
      process.df <- rbind(process.df, name[i]) # binds process name to df
      capex.df <- rbind(capex.df, final_y) # binds capex cost to df
      om.df <- rbind(om.df, final_omy) # binds o&m cost to df
      
      x <- x / 3785.4 # convert back to MGD
      
    } else {
      
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
      
    }}
  
  # create column names for the dataframe
  colnames(process.df) <- 'process'
  colnames(capex.df) <- 'capex'
  colnames(om.df) <- 'omex'
  
  # add uncertainty values to CAPEX and O&M
  # the uncertainty is (Keller et. al. 2021, Plumlee et. al. 2021) -30/+50%
 # capex.df <- capex.df %>% 
  #  mutate(lower = 0.3 * capex,
         #  upper = 0.5 * capex)
 # om.df <- om.df %>% 
  #  mutate(lowerom = 0.3 * omex,
      #     upperom = 0.5 * omex)
  
  # combine data frames together to make the plot data frame
  graph.df <- cbind(process.df, capex.df) %>% 
    cbind(om.df)
  
  return(graph.df)
  
}

# This function generates the dataframe for the economics plot
economics_techplot <- function(a, b, c, x, oma, omb, omc, name, input1, 
                               input2, input3, input4, tech, tech_input) {
  
  # outputs plot data
  plot.data <- economics_plot(a, b, c, x, oma, omb, omc, name)
  
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
  
  
  


# SECTION 2: DATA INPUT AND DATAFRAME GENERATION
# ------------------------------------------------------------------------------------------------------------------------

# Reverse Osmosis (Guo et. al. 2014)
ro <- data.frame('name' = 'reverse osmosis',
                 'a' = 0.966,
                 'b' = 0.929,
                 'c' = 3.082,
                 'oma' = 0.534,
                 'omb' = 1.253,
                 'omc' = 2.786,
                 'year' = NA,
                 'yearom' = NA)

# Membrane Ultrafiltration (Guo et. al. 2014)
uf <- data.frame('name' = 'ultrafiltration',
                 'a' = 1.003,
                 'b' = 0.830,
                 'c' = 3.832,
                 'oma' = 1.828,
                 'omb' = 0.598,
                 'omc' = 1.876,
                 'year' = NA,
                 'yearom' = NA)

# Granular Activated Carbon (Guo et. al. 2014)
gac <- data.frame('name'= 'granular activated carbon',
                  'a' = 0.722,
                  'b' = 1.023,
                  'c' = 3.443,
                  'oma' = 1.669,
                  'omb' = 0.559,
                  'omc' = 2.371,
                  'year' = NA,
                  'yearom' = NA)

# ozonation (Plumlee et. al. 2014)
o3 <- data.frame('name' = 'ozonation',
                 'a' = 2.26,
                 'b' = -0.54,
                 'c' = 0,
                 'oma' = 0.0068,
                 'omb' = -0.051,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)

# UV Disinfection + Hydrogen Peroxide
# Capital Cost (Plumlee et. al. 2014)
# O&M (Plumlee et. al. 2014)
uvh2o2 <- data.frame('name' = 'uv oxidation',
                 'a' = 0.474,
                 'b' = -0.056,
                 'c' = 0,
                 'oma' = 0.038,
                 'omb' = -0.052,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)

# Membrane Microfiltration
# Capital Cost (Plumlee et. al. 2014)
# O&M (Plumlee et. al. 2014)
mf <- data.frame('name' = 'microfiltration',
                 'a' = 3.57,
                 'b' = -0.22,
                 'c' = 0,
                 'oma' = 0.3,
                 'omb' = -0.22,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)

gw <- data.frame('name' = 'groundwater pumping',
                 'a' = 0, 'b' = 0, 'c' = 0,
                 'oma' = 0, 'omb' = 0, 'omc' = 0,
                 'year' = NA, 'yearom' = NA)


# combines the dataframes together
total <- rbind(ro, uf, gac, o3, uvh2o2, mf, gw)

# tests the functions using a flow rate of 10 MGD
cctest <- calculate_costs(total$a, total$b, total$c, 10, total$year)
omtest <- calculate_costs(total$oma, total$omb, total$omc, 10, total$yearom)
graphtest <- economics_plot(total$a, total$b, total$c, 10, total$oma, total$omb, total$omc, total$name)

plottestecon <- economics_techplot(total$a, total$b, total$c, 10, total$oma, total$omb, total$omc, total$name,
                                   a, b, c, d, tech, c('Direct Potable Reuse', 'Indirect Potable Reuse', 'Groundwater Desalination'))

econ_errorbars(plottestecon)
