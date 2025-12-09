# -------------------------------------------------------
# Author: Ryan Hull
# Date: October 2025
# Purpose: make simple graph for visualization of project 
# -------------------------------------------------------

# 1. Libraries
library(ggplot2)
library(ggpubr)

# 2. Plotting
rm(list=ls())

df <- data.frame(
  
  Species = c("Ambystoma jeffersonianum", "Ambystoma gracile",
               "Ambystoma laterale", "Desmognathus ochrophaeus" ,"Ambystoma maculatum", "Ambystoma macrodactylum",
               "Ambystoma mavortium", "Ambystoma texanum", "Ambystoma tigrinum","Eurycea bislineata",
              "Hemidactylium scutatum"),
  Current_mean_latitude = c(37.9, 38.2, 39, 41, 42.5, 43, 43.9, 46.8, 48,45.5,41),
  Change_in_mean = c(0.5,3.5,1,9,5,4,7,10,7,2.7,4.5),
  stringsAsFactors = FALSE
)


plot <- ggplot(data=df, aes(x=Current_mean_latitude, y=Change_in_mean)) +
  geom_point(color="steelblue",size=2,alpha=0.7) +
  geom_smooth(method="lm",se=TRUE, color="darkred",linetype="dashed", linewidth=1) +
  labs(
    x = "Current mean latitude (°N)",
    y = "Projected range shift (°)" ) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal(base_size = 14)
      
plot

lm <- lm(Change_in_mean ~ Current_mean_latitude, data=df)
summary(lm)
