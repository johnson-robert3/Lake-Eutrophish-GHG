#~~~
# Script to house all custom functions used for figures
# By: Robert Johnson
#~~~


# 3-panel figures (by food web treatment)
fig_aes_fw = function(.fig) {
   
   .fig +
      
      # Nutrient pulses
      geom_vline(xintercept = c(176, 211), linetype=2, color="gray40") +
      
      ## Data as points
      # geom_line(aes(group = trt_nutrients), size=0.5) +
      # geom_point(color="white", fill="white", shape=21, size=3) +
      # geom_point(aes(fill = trt_nutrients), shape=21, size=3, alpha=0.8) +
      # scale_fill_manual(name = NULL,
      #                   labels = c("no" = "Reference", "yes" = "Pulsed"),
      #                   values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
      
      ## Data as lines
      geom_line(aes(group = trt_nutrients, color = trt_nutrients), size=1.2, alpha=0.7) +
      scale_color_manual(name = NULL,
                         labels = c("no" = "Reference", "yes" = "Pulsed"),
                         values = c("no" = "cornflowerblue", "yes" = "seagreen3")) +
      
      # Aesthetics
      scale_x_continuous(name = "DOY", expand = expansion(mult=0.1)) +
      theme_classic()
   
}



# Buoyancy Frequency function for non-time-series data
profile_bf = function(.dat) {
   
   bf = buoyancy.freq(wtr = .dat$temp,
                      depths = .dat$depth_int)
   
   buoy_freq = as.vector(bf)
   
   depth = attr(bf, "depths")
   
   table = data.frame(depth, buoy_freq)
   
   return(table)
   
}


