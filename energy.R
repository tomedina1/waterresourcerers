
# Energy
# Taylor Medina


# Pumping Energy Requirement
e_gwpump <- function(q, k, E){
  
  # q is the volumetric flow rate (m3/d), k is the total system losses and E is the pump efficiency
  # 0.018238673 is the area of a 6" pipe
  # 9.81 is the gravitation constant
  h <- (k * (q / 0.018238673) ^ 2) / (2 * 9.81)
  P <- (q * h * 9.81 * 1000) / E
  P <- P / 1e6
  return(P)

}


# Additional Information for the friction factor of a pipe - this will affect the total system losses
friction_factor <- function(q, k){
  
  # q is the volumetric flow rate (m3/d) and k is the pipe roughness 
  v <- q / 0.018238673 # convert flow rate to instantaneous velocity 
  Re <- (v * 0.1524) / 1e-6 # 0.1524 is the diameter of a 6 inch pipe, 1e-6 is the kinematic viscosity of water
  f <- 0.25 / (log(k / (3.75 * 0.1524)) + 5.74 / (Re ^ 0.9)) ^ 2 # friction factor equation f(k, Re)
  return(f)
  
}


# Calculating system losses
system_losses <- function(k_f, q, k ,L){
  
  # calculates the system losses given L (length of a pipe in m)
  # also needs k_f (fittings losses), volumetric flow rate (m3/s), and pipe roughness
  f <- friction_factor(q, k) # calculates the friction factor
  k_p <- (L * f) / 0.1524 # calculates the pipe losses
  k_t <- k_p + k_f # total losses is a summation of pipe losses and fittings losses
  return(k_t)
  
}


# RO Energy Requirement
ro <- function(RR, eta, pi, x) {
  
  # calculates the RO energy requirements modeled as a semibatch process
  # This is noted in Gu et al. 2021 that Potable Reuse follows SB processes
  
  sb <- (1 / eta) * (x + pi (1 + RR / (2 * (1 - RR))))
  return(sb)
  
}

# Energy Requirement Calculations
energy_req <- function(a, x, RR, eta, pi, k, E){
  
  e_req <- data.frame()
  
  for (i in 1:length(a$name)){
    if (a$name[i] == 'reverse osmosis'){
      ro_req <- ro(RR, eta, pi, x)
      e_req <- rbind(e_req, ro_req)
    }
    
    else if (a$name[i] == 'groundwater pumping'){
      pump_req <- e_gwpump(x, k, E)
      e_req <- rbind(e_gwpump, k, E)
    }
    
    else {
      o_req <- req[i] * x
      e_req <- rbind(e_req, o_req)
    }
  }
  
  tot_energy <- sum(e_req)
  return(tot_energy)
}

# Individual energy requirements for each unit process
gwpump <- data.frame('name' = 'groundwater pumping', 'req' = NA)
ro <- data.frame('name' = 'reverse osmosis', 'req' = NA)
coag <- data.frame('name' = 'coagulation', 'req' = mean(0.4, 0.7))
uv <- data.frame('name' = 'uv oxidation', 'req' = mean(0.01, 0.05))
o3 <- data.frame('name' = 'ozonation', 'req' = mean(0.03, 0.1))
uf <- data.frame('name' = 'ultrafiltration', 'req' = mean(0.07, 0.1, 0.2))
mf <- data.frame('name' = 'microfiltration', 'req' = 0.18)
gac <- data.frame('name' = 'granular activated carbon', 'req' = 0.37)
gmf <- data.frame('name' = 'gmf', 'req' = mean(0.16, 0.32))
recharge <- data.frame('name' = 'groundwater recharge', 'req' = 0.48)
desal <- data.frame('name' = 'saltwater desalination', 'req' = mean(3.5, 4.5))

# combine each of the unit processes into a consolidated data frame
energy_reqs <- rbind(gwpump, coag, uv, o3, uf, mf, gac, gmf, recharge, desal)

