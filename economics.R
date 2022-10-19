 
# ECONOMICS 
# Taylor Medina

library(tidyverse)


# William's Power Logarithmic Rule
# log(y) = a(log(x))^b + c

capitalcost <- function(a, b, c, x){
  
  costs <- data.frame()
  
  for (i in 1:length(a)) {
    # iterates through every input
      if (c[i] != 0){
        
        y <- a[i] * log(x) ^ (b[i]) + c[i] # calculates log(y)
        final_y <- 10 ^ y 
        costs <- rbind(costs, final_y) # binds the output for each iteration to a df
        
      } else {
        
        int <- integrate(function(y)
          {a[i] * y ^ b[i]}, lower = 0, upper = x)
        
        y_euro <- int$value
        y_dollar <- 1.1723 * y_euro # euro to dollar conversion from Sep 2020 
        costs <- rbind(costs, y_dollar)
      
    }}
  
  costs_sum <- sum(costs) # sums the dataframe to get the total cost of the system
  return(costs_sum)
  
}


omcost <- function(a, b, c, x, name){
  
  costs <- data.frame()
  
  for (i in 1:length(a)){
    
    if (!is.na(a[i])){
      
      y <- a[i] * log(x) ^ (b[i]) + c[i] # calculates log(y)
      final_y <- 10 ^ y 
      costs <- rbind(costs, final_y)
      
    } else {
      
      if (name[i] == 'uv disinfection'){
        
        avg <- mean(0.4, 5) / 3.785
        final <- avg * x ^ 2 * 2 # integrate and convert to 2022 dollar
        costs <- rbind(costs, final)
        
      } else {
        
        next
        
      }}}
  
  costs_sum <- sum(costs)
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
                   'omc' = 2.633)

# reverse osmosis
ro <- data.frame('name' = 'reverse osmosis',
                 'a' = 0.222,
                 'b' = 1.516,
                 'c' = 3.071,
                 'oma' = 0.534,
                 'omb' = 1.253,
                 'omc' = 2.786)

# ultrafiltration
uf <- data.frame('name' = 'ultrafiltration',
                 'a' = 0.966,
                 'b' = 0.929,
                 'c' = 3.082,
                 'oma' = 1.828,
                 'omb' = 0.598,
                 'omc' = 1.876)

# granular activated carbon
gac <- data.frame('name'= 'granular activated carbon',
                  'a' = 0.722,
                  'b' = 1.023,
                  'c' = 3.443,
                  'oma' = 1.669,
                  'omb' = 0.559,
                  'omc' = 2.371)

# chlorination
cl <- data.frame('name' = 'chlorination',
                 'a' = 3.416,
                 'b' = -0.422,
                 'c' = 0,
                 'oma' = NA,
                 'omb' = NA,
                 'omc' = NA)

# ozonation
o3 <- data.frame('name' = 'ozonation',
                 'a' = 348.02,
                 'b' = -0.069,
                 'c' = 0,
                 'oma' = NA,
                 'omb' = NA,
                 'omc' = NA)

# uv
uv <- data.frame('name' = 'uv disinfection',
                 'a' = 1209.2,
                 'b' = -0.328,
                 'c' = 0,
                 'oma' = NA,
                 'omb' = NA,
                 'omc' = NA)


# O&M costs for UV disinfection
uv_om <- data.frame(x = c(1022.06, 41639.53, 794936.48),
                    y = c(13.83, 133.69, 1774.91))
plot(uv_om$x, uv_om$y)

# combines the dataframes together
total <- rbind(coag, ro, uf, gac, cl, o3, uv)

# tests the function using a flow rate of 10 MGD
cctest <- capitalcost(total$a, total$b, total$c, 10)
omtest <- omcost(total$oma, total$omb, total$omc, 10, total$name)


