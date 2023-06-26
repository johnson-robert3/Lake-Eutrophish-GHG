#~~~
# Script to house all custom functions used for figures
# By: Robert Johnson
#~~~


# Commonly used values/vectors for figures

# Nutrient treatment

pulse_breaks = c("yes", "no")
# nut_breaks = c("yes", "no")

pulse_color = c("yes" = "#5D3891", "no" = "#F99417") # pulse = purple, ref = orange
# nut_color = c("yes" = "#5D3891", "no" = "#F99417") # pulse-purple, ref-orange

pulse_labs = c("yes" = "Pulsed", "no" = "Reference")
# nut_labs = c("yes" = "Pulsed", "no" = "Reference")

# nut_color = c("yes" = "seagreen3", "no" = "cornflowerblue")
# nut_color = c("yes" = "#e94560", "no" = "#0f3460") # pulse-red, ref-blue


# Food web treatment
fish_breaks = c("high", "medium", "low")
fish_alpha = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)
fish_labs = c("high" = "High", "medium" = "Intermediate", "low" = "Low")
fish_color = c("high" = "#a8dda8", "medium" = "#51c2d5", "low" = "#0f3460")



# # Buoyancy Frequency function for non-time-series data
# profile_bf = function(.dat) {
#    
#    bf = buoyancy.freq(wtr = .dat$temp,
#                       depths = .dat$depth_int)
#    
#    buoy_freq = as.vector(bf)
#    
#    depth = attr(bf, "depths")
#    
#    table = data.frame(depth, buoy_freq)
#    
#    return(table)
#    
# }

