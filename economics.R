 
# ECONOMICS 
# Taylor Medina

library(tidyverse)

# Function 1: integrate(y = a*x^b) from 0 to x
# This is a general function format where a and b are constants and x is the flow rate
# y = 
# The function is integrated to get the total cost
cost_function <- function(a, b, x){
  
  int <- integrate(function(y)
    {a * y ^ b}, lower = 0, upper = x)
  return(int$value)
  
}


calculate_costs <- function(a, b, c, x, year){
  
  costs <- data.frame()
  
  for (i in 1:length(a)) {
    # iterates through every input
      if (c[i] != 0){
        
        x <- x * 3785.4 # convert MGD to m3/d
        
        # William's Power Logarithmic Rule
        # log(y) = a(log(x))^b + c
        y <- a[i] * log(x) ^ (b[i]) + c[i] # calculates log(y)
        final_y <- 10 ^ y 
        costs <- rbind(costs, final_y) # binds the output for each iteration to a df
        
      } else {
        
        if (year[i]== 2014) {
          y_value <- cost_function(a[i], b[i], x)
          y_conversion <- 1.25 * y_value # converts from 2014 dollars to current dollar (2022 October)
          costs <- rbind(costs, y_conversion)
        }
        
        else {
          x <- x * 3785.4 # convert MGD to m3/d
          y_value <- cost_function(a[i], b[i], x)
          y_dollar <- 1.1723 * y_value # 2020 Euro to current dollar (2022 October)
          costs <- rbind(costs, y_dollar)
        }
      
    }}
  
  costs_sum <- sum(costs) # sums the dataframe to get the total cost of the system
  return(costs_sum)
  
}


# From Hilbig et. al.
# Create a df of necessary economic information (Guo et. al.)
# CAPITAL COSTS

# coagulation and flocculation
coag <- data.frame('name' = 'coagulation & flocculation',
                   'a' = 0.569,
                   'b' = 1.135,
                   'c' = 4.605,
                   'oma' = 0.347,
                   'omb' = 1.448,
                   'omc' = 2.633,
                   'year' = NA,
                   'yearom' = NA)

# reverse osmosis
ro <- data.frame('name' = 'reverse osmosis',
                 'a' = 0.222,
                 'b' = 1.516,
                 'c' = 3.071,
                 'oma' = 0.534,
                 'omb' = 1.253,
                 'omc' = 2.786,
                 'year' = NA,
                 'yearom' = NA)

# ultrafiltration
uf <- data.frame('name' = 'ultrafiltration',
                 'a' = 0.966,
                 'b' = 0.929,
                 'c' = 3.082,
                 'oma' = 1.828,
                 'omb' = 0.598,
                 'omc' = 1.876,
                 'year' = NA,
                 'yearom' = NA)

# granular activated carbon
gac <- data.frame('name'= 'granular activated carbon',
                  'a' = 0.722,
                  'b' = 1.023,
                  'c' = 3.443,
                  'oma' = 1.669,
                  'omb' = 0.559,
                  'omc' = 2.371,
                  'year' = NA,
                  'yearom' = NA)

# chlorination (Capital Cost - Hilbig et. al. (2020))
cl <- data.frame('name' = 'chlorination',
                 'a' = 3.416,
                 'b' = -0.422,
                 'c' = 0,
                 'oma' = 0,
                 'omb' = 0,
                 'omc' = 0,
                 'year' = 2020,
                 'yearom' = 2014)

# ozonation
o3 <- data.frame('name' = 'ozonation',
                 'a' = 348.02,
                 'b' = -0.069,
                 'c' = 0,
                 'oma' = 0.0068,
                 'omb' = -0.051,
                 'omc' = 0,
                 'year' = 2020,
                 'yearom' = 2014)

# uv
uv <- data.frame('name' = 'uv disinfection + ozone',
                 'a' = 2.26,
                 'b' = -0.54,
                 'c' = 0,
                 'oma' = 0.016,
                 'omb' = -0.02,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)

uvh2o2 <- data.frame('name' = 'uv disinfection + h2o2',
                 'a' = 0.474,
                 'b' = -0.056,
                 'c' = 0,
                 'oma' = 0.038,
                 'omb' = -0.052,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)

mf <- data.frame('name' = 'microfiltration',
                 'a' = 3.57,
                 'b' = -0.22,
                 'c' = 0,
                 'oma' = 0.3,
                 'omb' = -0.22,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)


# combines the dataframes together
total <- rbind(coag, ro, uf, gac, cl, o3, uv, uvh2o2, mf)

# tests the function using a flow rate of 10 MGD
cctest <- calculate_costs(total$a, total$b, total$c, 10, total$year)
omtest <- calculate_costs(total$oma, total$omb, total$omc, 10, total$yearom)


