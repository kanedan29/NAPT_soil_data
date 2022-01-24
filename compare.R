library(tidyverse)
library(here)

d <- read_csv(here("NAPT_data.csv"))

ggplot() + 
  geom_density(data = d, aes(x = mad_som_walkley_black*.58, fill = "#1b9e77", color = "#1b9e77"), size = 1, alpha = 0.3) +
  geom_density(data = d, aes(x = mad_soil_total_c_combustion, fill = "#d95f02", color = "#d95f02"), size = 1, alpha = 0.3) +
  geom_density(data = d, aes(x = mad_som_loi_percent_wt_loss*.58, fill = "#7570b3", color = "#7570b3"), size = 1, alpha = 0.3) +
  scale_fill_identity(name = "Method",
                      breaks = c("#1b9e77","#d95f02","#7570b3"),
                      labels = c("WB", "Combustion", "LOI"),
                      guide = "legend")+
  scale_color_identity(name = "Method",
                       breaks = c("#1b9e77","#d95f02","#7570b3"), 
                       labels = c("WB", "Combustion", "LOI"),
                       guide = "legend")+
  xlab("")
