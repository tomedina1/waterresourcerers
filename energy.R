
# Energy
# Taylor Medina


# SECTION 1: Energy Calculating Functions
# ---------------------------------------------------------------------------------------------------------
# Pumping Energy Requirement
e_gwpump <- function(q, k, E) {
  
  # q is the volumetric flow rate (m3/d), k is the total system losses and E is the pump efficiency
  # 0.018238673 is the area of a 6" pipe
  # 9.81 is the gravitation constant
  h <- (k * (q / 0.018238673) ^ 2) / (2 * 9.81)
  P <- (q * h * 9.81 * 1000) / E
  P <- P / 1e3
  return(P)

}


# Additional Information for the friction factor of a pipe - this will affect the total system losses
friction_factor <- function(q, k) {
  
  # q is the volumetric flow rate (m3/d) and k is the pipe roughness 
  v <- q / 0.018238673 # convert flow rate to instantaneous velocity 
  Re <- (v * 0.1524) / 1.0035e-6 # 0.1524 is the diameter of a 6 inch pipe, 1e-6 is the kinematic viscosity of water
  f <- 0.25 / ((log10(k / (3.75 * 0.1524)) + 5.74 / (Re ^ 0.9))^ 2) # friction factor equation f(k, Re)
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


# RO Energy Requirement
r_o <- function(RR, eta, osp, x) {
  
  # calculates the RO energy requirements modeled as a semibatch process
  # This is noted in Gu et al. 2021 that Potable Reuse follows SB processes
  
  sb <- (1 / eta) * (x + osp * (1 + RR / (2 * (1 - RR))))
  sb_out <- sb / 24 * x
  return(sb_out)
  
}


# SECTION 2: Energy Calculation Functions
# -------------------------------------------------------------------------------------
energy_req <- function(a, x , pump, RR, eta, osp, k_f, k, L, E){
  
  x <- x * 3785.4 # convert from MGD to m3/d
  
  e_req <- data.frame()
  
  for (i in 1:length(a$name)) {
    
    if (a$name[i] == 'reverse osmosis') { # runs the semibatch reactor equation
      
      ro_req <- r_o(RR, eta, osp, x)
      e_req <- rbind(e_req, ro_req)
      
    } else if (a$name[i] == 'groundwater pumping') { # runs the pump requirement equation
      
      pump_req <- e_gwpump(pump, system_losses(k_f, pump, k, L), E)
      e_req <- rbind(e_req, pump_req)
      
    } else { # runs the scaling energy requirement
      
      o_req <- a$req[i] * x / 24 * 365
      e_req <- rbind(e_req, o_req)
      
    }}
  
  tot_energy <- sum(e_req)
  return(tot_energy)
  
}


# ---------------------------------------------------------------------------------
# Individual energy requirements for each unit process
gwpump <- data.frame('name' = 'groundwater pumping', 'req' = NA)
ro <- data.frame('name' = 'reverse osmosis', 'req' = NA)
coag <- data.frame('name' = 'coagulation', 'req' = mean(0.4, 0.7))
uv <- data.frame('name' = 'uv oxidation', 'req' = mean(0.01, 0.05))
o3 <- data.frame('name' = 'ozonation', 'req' = mean(0.05 * 3.79, 0.12 * 3.79) / 24)
uf <- data.frame('name' = 'ultrafiltration', 'req' = mean(0.07, 0.1, 0.2))
mf <- data.frame('name' = 'microfiltration', 'req' = 0.18 / 24)
gac <- data.frame('name' = 'granular activated carbon', 'req' = 0.37)
recharge <- data.frame('name' = 'groundwater recharge', 'req' = 0.48)

# combine each of the unit processes into a consolidated data frame
energy_reqs <- rbind(gwpump, ro, coag, uv, o3, uf, mf, gac, recharge)


# FUNCTION 
# ---------------------------------------------------------------------------------
energy_plot <- function(a, x, RR, eta, osp, k_f, pump, k, L, E) {
  
  x <- x * 3785.4 # MGD to m3/d
  
  graph.df <- data.frame()
  name.df <- data.frame()
  
  for (i in 1:length(a$name)) {
    
    if (a$name[i] == 'groundwater pumping') {
      
      # groundwater pumping functions
      pump_req <- e_gwpump(pump, system_losses(k_f, pump, k, L), E)
      graph.df <- rbind(graph.df, pump_req)
      name.df <- rbind(name.df, a$name[i])
    
    } else if (a$name[i] == 'reverse osmosis') { 
      
      # run the semibatch equation 
      ro_req <- r_o(RR, eta, osp, x)
      graph.df <- rbind(graph.df, ro_req)
      name.df <- rbind(name.df, a$name[i])
                     
    } else {
      
      # run the equations for the rest of the processes
      o_req <- a$req[i] * x / 24 * 365
      graph.df <- rbind(graph.df, o_req)
      name.df <- rbind(name.df, a$name[i])
      
    }}
  
  graph.df <- cbind(name.df, graph.df)
  colnames(graph.df) <- c('process', 'energyreq')
  return(graph.df)
  
}

# ---------------------------------------------------------------------------------------
# a test for the energy_plot function
test <- energy_plot(energy_reqs, 10, 0.5, 0.5, 100, 0.3, 0.5, 0.6, 100, 0.4)


# load names for each technology into a character vector
tech <- c('Direct Potable Reuse', 'Indirect Potable Reuse', 'Groundwater Desalination',
          'Ocean Desalination')


# FUNCTION
# --------------------------------------------------------------------------------------
technology_plot <- function(a, b, c, d, process, tech, x, RR, 
                            eta, osp, k_f, pump, k, L, E, tech_input) {
  
  # generates the energy requirement data frame based off of the selected inputs
  plot.data <- energy_plot(process, x, RR, eta, osp, k_f, pump, k, L, E)
  
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
    }
    
  }
  
  
  tech.df <- tech.df %>% 
    filter(technology %in% tech_input)
  

  return(tech.df)
  
}

# test
#a <- c('microfiltration', 'reverse osmosis', 'uv oxidation')
#b <- c('microfiltration', 'reverse osmosis', 'uv oxidation')
# <- c('groundwater pumping', 'reverse osmosis')
#d <- c('reverse osmosis')
#listtest <- technology_plot(a, b, c, d, energy_reqs, tech, 10, 0.5, 0.5, 100, 0.3, 0.5, 0.6, 100, 0.4)

