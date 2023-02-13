
# Energy
# Taylor Medina

# load packages
library(tidyverse)

#######################################################################################
# Pumping Energy Requirement
e_gwpump <- function(q, k, E) {
  
  # q is the volumetric flow rate (m3/s), k is the total system losses and E is the pump efficiency
  # 0.018238673 is the area of a 6" pipe
  # 9.81 is the gravitation constant
  h <- (k * (q / 0.018238673) ^ 2) / (2 * 9.81)
  P <- (h * 9.81 * 1000) / (E * 1e3 * 3600)
  return(P)

}


# Additional Information for the friction factor of a pipe - this will affect the total system losses
friction_factor <- function(q, k) {
  
  # q is the volumetric flow rate (m3/d) and k is the pipe roughness 
  v <- q / 0.018238673 # convert flow rate to instantaneous velocity 
  Re <- (v * 0.1524) / 1.0035e-6 # 0.1524 is the diameter of a 6 inch pipe, 1e-6 is the kinematic viscosity of water
  f <- 0.25 / ((log10(k / (3.75 * 0.1524)) + 5.74 / (Re ^ 0.9)) ^ 2) # friction factor equation f(k, Re)
  return(f)
  
}


# Calculating system losses
system_losses <- function(k_f, q, k, L) {

  # calculates the system losses given L (length of a pipe in m)
  # also needs k_f (fittings losses), volumetric flow rate (m3/s), and pipe roughness
  f <- friction_factor(q, k) # calculates the friction factor
  k_p <- (L * f) / 0.1524 # calculates the pipe losses
  k_t <- k_p + k_f # total losses is a summation of pipe losses and fittings losses
  return(k_t)
  
}
#######################################################################################

energy_req <- function(a, x , pump, k_f, k, L, E){
  
  x <- x * 3785.4 # convert from MGD to m3/d
  
  e_req <- data.frame()
  
  for (i in 1:length(a$name)) {

    if (a$name[i] == 'groundwater pumping') { # runs the pump requirement equation
      
      pump_req <- e_gwpump(pump, system_losses(k_f, pump, k, L), E)
      e_req <- rbind(e_req, pump_req)
      
    } else { # runs the scaling energy requirement
      
      o_req <- a$req[i] 
      e_req <- rbind(e_req, o_req)
      
    }}
  
  tot_energy <- sum(e_req)
  return(tot_energy)
  
}

#######################################################################################

energy_plot <- function(a, x, k_f, pump, k, L, E) {
  
  x <- x * 3785.4 # MGD to m3/d
  
  graph.df <- data.frame()
  name.df <- data.frame()
  
  for (i in 1:length(a$name)) {
    
    if (a$name[i] == 'groundwater pumping') {
      
      # groundwater pumping functions
      pump_req <- e_gwpump(pump, system_losses(k_f, pump, k, L), E)
      graph.df <- rbind(graph.df, pump_req)
      name.df <- rbind(name.df, a$name[i])
    
    } else {
      
      # run the equations for the rest of the processes
      o_req <- a$req[i] 
      graph.df <- rbind(graph.df, o_req)
      name.df <- rbind(name.df, a$name[i])
      
    }}
  
  graph.df <- cbind(name.df, graph.df)
  colnames(graph.df) <- c('process', 'energyreq')
  return(graph.df)
  
}

#######################################################################################
# load names for each technology into a character vector
tech <- c('Direct Potable Reuse', 'Indirect Potable Reuse', 'Groundwater Desalination',
          'Ocean Desalination')
#######################################################################################

# This is the function that generates the plot on the shiny app
technology_plot <- function(a, b, c, d, process, tech, x, k_f, pump, k, L, E, tech_input) {
  
  # generates the energy requirement data frame based off of the selected inputs
  plot.data <- energy_plot(process, x, k_f, pump, k, L, E)
  
  # condenses the 4 technology drop down menu inputs into a vector
  input.vector <- c(a, b, c, d)
  
  # generate a blank data frame
  tech.df <- data.frame()
  
  # loops through the input vector and filters the plot.data to match 
  # then attaches the associated technology to it
  for (i in 1:length(input.vector)) {
    
    if (i <= length(a)) {
      
      # data wrangling to filter the dataset
      data <- plot.data %>% 
        filter(process == input.vector[i]) %>% 
        mutate(technology = tech[1])
      
      tech.df <- rbind(tech.df, data) # add to the blank data frame
      
    } else if (i > length(a) & i <= length(a) + length(b)) {
      
      # data wrangling to filter the dataset
      data <- plot.data %>% 
        filter(process == input.vector[i]) %>% 
        mutate(technology = tech[2])
      
      tech.df <- rbind(tech.df, data) # add to the blank data frame
      
    } else if (i > length(a) + length(b) & i <= length(input.vector) - length(d)) {
      
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
  
  # filters the dataframe for the technologies selected on the shiny app
  tech.df <- tech.df %>% 
    filter(technology %in% tech_input)
  return(tech.df)
  
}

# Combines the variance of the energy requirements to the final dataframe
energy_error <- function(energy_data, error_df) {
  final <- full_join(energy_data, error_df, by = 'process') %>% 
    drop_na(energyreq)
  return(final)
}

# Calculates the standard deviation
energy_sd <- function(error_data) {
  
  
  final <- error_data %>% 
    group_by(technology) %>% 
    summarize(energyreq = sum(energyreq),
              sd = sqrt(sum(var ^ 2, na.rm = TRUE))) %>% # sd is the sum of the variances ^ 2
    mutate(upper = energyreq + sd,
           lower = energyreq - sd,
           total = paste(energyreq, "%+-%", sd))
  
  return(final)
}

