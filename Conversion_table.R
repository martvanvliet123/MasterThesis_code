# Conversion rates table
# How would we get all the crosses??
#16-10
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(xtable)
library(tidyr)
library(tibble)
library(readxl)
library(grid)
library(purrr)

## WITH WHAT PERCENTAGE OF THE DATA ARE WE WORKING WITH WHEN MINIMUM > 2, AND OTHER NUMBERS?? ELBOW PLOT??
# Loading data ------------------------------------------------------------
conv_data <- read_excel("C:/Users/m.vanvliet/OneDrive - Performation Healthcare Intelligence BV/Documenten/Data Dijklander/PC_export_.xlsx")
conv_data <- data.frame(Specialism = conv_data$Capaciteitstype, Consultcode = conv_data$`Consultcode / Opname type`, Department = conv_data$Entiteit, Appointments = conv_data$Aantallen)
# Appointments is aantal afspraken, operaties of opnames
# Only keep interesting data (with at least one week in which they have more than two appointments)
minimum <- 2
conv_data <- conv_data[conv_data$Appointments >= minimum, ]
# First, filter every unique possible combination. 
all_spec <- unique(conv_data[,c('Specialism', 'Consultcode', 'Department')])
conv_data

# No piles ----------------------------------------------------------------
all_spec_plus_code <- data.frame(Specialism = paste(all_spec[,1], all_spec[,3], all_spec[,2], sep = ", "))

# Calculate conversion rates ----------------------------------------------
conv_rates <- as.data.frame(matrix(runif(length(all_spec_plus_code$Specialism)^2, min = 0, max = 2000/length(all_spec_plus_code$Specialism)), nrow = length(all_spec_plus_code$Specialism), ncol = length(all_spec_plus_code$Specialism),
                                              dimnames = list(all_spec_plus_code$Specialism, all_spec_plus_code$Specialism)))
conv_rates <- as.data.frame(lapply(conv_rates, \(x) replace(x, sample(length(x), .999*length(x)), 0))) # Change 90% to NA
rownames(conv_rates) <- all_spec_plus_code$Specialism
colnames(conv_rates) <- all_spec_plus_code$Specialism

rm(all_spec, all_spec_plus_code, conv_data)
# Het gaat van kolom naar rij, dus (rij 7, kolom 4) is ván INT-arts-H, Poli, * naar GYN-arts-H, Poli, *.  
# # Now go back to the R script to import these data