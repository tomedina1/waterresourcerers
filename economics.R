 
# ECONOMICS 
# Taylor Medina

library(tidyverse)

# SECTION 1: ECONOMIC EQUATIONS AND FUNCTIONS
# ------------------------------------------------------------------------------------------------------------------------

# Function 1: integrate(y = a*x^b) from 0 to x
# This is a general function format where a and b are constants and x is the flow rate
# y: $/flow rate (depending on the paper could be in units of m3/d or MGD)
# The function is integrated to get the total cost

cost_function <- function(a, b, x){
  
  int <- integrate(function(y)
    {a * y ^ b}, lower = 0, upper = x)
  return(int$value)
  
}

# Function 2: this function calculates the capital costs and the o&m costs depending on the inputs
# Do not be intimidated by the for loops - I hope the code is commented clearly enough
# This function requires vector inputs for a,b, c, and year. 
# a, b, and c are fitted constants unique to each unit process
# year is needed to account for inflation 

calculate_costs <- function(a, b, c, x, year){
  
  # generate a blank data frame
  costs <- data.frame()
  
  for (i in 1:length(a)) { # iterates through every input in the vector (a, b, c, and year should be the same length***)

      if (c[i] != 0){ # c is only a constant in the Williams Power Logarithmic Rule Equation (Guo et. al. 2014)
        
        x <- x * 3785.4 # convert MGD to m3/d (the equation only works in m3/d)
        
        # William's Power Logarithmic Rule
        # log(y) = a * log(x) ^ b + c
        
        y <- a[i] * log(x) ^ (b[i]) + c[i] # calculates log(y)
        final_y <- 10 ^ y 
        costs <- rbind(costs, final_y) # binds the output for each iteration to the blank df
        
      } else { # the rest of the equations do not have a value for c
        
        # based off of the equation y = a * x ^ b (Plumlee et. al. 2014, Hilbig et. al. 2020)
        
        if (year[i]== 2014) { # for all of the equations from Plumlee et. al. 2014
          y_value <- cost_function(a[i], b[i], x) # calls the integral function
          y_conversion <- 1.25 * y_value # converts from 2014 dollars to current dollar (2022 October)
          costs <- rbind(costs, y_conversion) # binds the output for each iteration to the blank df
        }
        
        else { # for all of the equations from Hilbig et. al. 2020
          x <- x * 3785.4 # convert MGD to m3/d
          y_value <- cost_function(a[i], b[i], x)  # calls the integral function
          y_dollar <- 1.1723 * y_value # 2020 Euro to current dollar (2022 October)
          costs <- rbind(costs, y_dollar) # binds the output for each iteration to the blank df
        }
      
    }}
  
  costs_sum <- sum(costs) # sums the dataframe to get the total cost of the system
  return(costs_sum)
  
}



# SECTION 2: DATA INPUT AND DATAFRAME GENERATION
# ------------------------------------------------------------------------------------------------------------------------

# Coagulation and Flocculation (Guo et al. 2014)
coag <- data.frame('name' = 'coagulation & flocculation',
                   'a' = 0.569,
                   'b' = 1.135,
                   'c' = 4.605,
                   'oma' = 0.347,
                   'omb' = 1.448,
                   'omc' = 2.633,
                   'year' = NA,
                   'yearom' = NA)

# Reverse Osmosis (Guo et. al. 2014)
ro <- data.frame('name' = 'reverse osmosis',
                 'a' = 0.222,
                 'b' = 1.516,
                 'c' = 3.071,
                 'oma' = 0.534,
                 'omb' = 1.253,
                 'omc' = 2.786,
                 'year' = NA,
                 'yearom' = NA)

# Membrane Ultrafiltration (Guo et. al. 2014)
uf <- data.frame('name' = 'ultrafiltration',
                 'a' = 0.966,
                 'b' = 0.929,
                 'c' = 3.082,
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

# Chlorination 
# Capital Cost (Hilbig et. al. 2020)
# O&M -- NO DATA YET
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
# Capital Cost (Hilbig et. al. 2020)
# O&M (Plumlee et. al. 2014)
o3 <- data.frame('name' = 'ozonation',
                 'a' = 348.02,
                 'b' = -0.069,
                 'c' = 0,
                 'oma' = 0.0068,
                 'omb' = -0.051,
                 'omc' = 0,
                 'year' = 2020,
                 'yearom' = 2014)

# UV Disinfection + Ozonation
# Capital Cost (Plumlee et. al. 2014)
# O&M (Plumlee et. al. 2014)
uv <- data.frame('name' = 'uv disinfection + ozone',
                 'a' = 2.26,
                 'b' = -0.54,
                 'c' = 0,
                 'oma' = 0.016,
                 'omb' = -0.02,
                 'omc' = 0,
                 'year' = 2014,
                 'yearom' = 2014)

# UV Disinfection + Hydrogen Peroxide
# Capital Cost (Plumlee et. al. 2014)
# O&M (Plumlee et. al. 2014)
uvh2o2 <- data.frame('name' = 'uv disinfection + h2o2',
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


# combines the dataframes together
total <- rbind(coag, ro, uf, gac, cl, o3, uv, uvh2o2, mf)

# tests the function using a flow rate of 10 MGD
cctest <- calculate_costs(total$a, total$b, total$c, 10, total$year)
omtest <- calculate_costs(total$oma, total$omb, total$omc, 10, total$yearom)


