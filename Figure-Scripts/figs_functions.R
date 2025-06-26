#~~~
# Script to house all custom functions used for figures
# By: Robert Johnson
#~~~


## Commonly used values/vectors for figures

# Events
p1 = 176.5
heatwave = 185:190
p2 = 211.5
derecho = 223.5


# Event labels
event_labs = c('P1', 'H', 'P2', 'D')
event_lab.x = c(p1, mean(heatwave), p2, derecho)  # x values for event labels above plots
# event_lab.xts = event_lab.x + c(0, 0, 0, 0)  # adjustment for flux time-series labels


# Add Events to a figure
fig_events = function(.fig) {
   
   .fig +
      # pulse events, DOY 176 and 211 (after all sampling had occurred)
      geom_vline(xintercept = c(p1, p2), linetype=1, color="gray40", linewidth=0.8) +
      # heat event, DOY 185-190 (July 3-8, 2020)
      annotate(geom = 'rect', xmin = min(heatwave), xmax = max(heatwave), ymin = -Inf, ymax = Inf, fill='gray75') +
      # derecho, DOY 223 (Aug. 10, 2020)
      geom_vline(xintercept = derecho, linetype=2, color='gray40', linewidth=0.8)
}


# Add analysis windows to a figure
fig_windows = function(.fig) {
   
   .fig +
      # pulse windows (1-5 days after event)
      annotate(geom = 'rect', xmin = p1+0.2, xmax = p1+4.2, ymin = -Inf, ymax = Inf, fill='gray90') +
      annotate(geom = 'rect', xmin = p2+0.2, xmax = p2+4.2, ymin = -Inf, ymax = Inf, fill='gray90') +
      # derecho window (1-5 days after event)
      annotate(geom = 'rect', xmin = derecho+0.2, xmax = derecho+4.2, ymin = -Inf, ymax = Inf, fill='gray90')
}


# Nutrient treatment
pulse_breaks = c("pulsed", "reference")
pulse_color = c("pulsed" = "#5D3891", "reference" = "#F99417") # pulse = purple, ref = orange
pulse_labs = c("pulsed" = "Pulsed", "reference" = "Reference")

# Food web treatment
fish_breaks = c("high", "medium", "low")
fish_alpha = c("high" = 0.9, "medium" = 0.6, "low" = 0.3)
fish_labs = c("high" = "High", "medium" = "Intermediate", "low" = "Low")
fish_color = c("high" = "#a8dda8", "medium" = "#51c2d5", "low" = "#0f3460")


# panel and axis aesthetics
fig_theme = function(.fig) {
   .fig +
   theme(panel.border = element_rect(fill=NA, color='black'),
         # axis.ticks.length = unit(0.3, 'line'),
         axis.ticks = element_line(color='black'), 
         axis.text = element_text(color='black', size=9),
         axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
         axis.title = element_text(color="black", size=9.5), 
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=1, 'line')))
}


