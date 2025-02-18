# 22-10
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
library(plotly)

# Loading data ------------------------------------------------------------
# Import the PC input from Dijklander Ziekenhuizen
data <- read_excel("C:/.../PC_export_.xlsx")
# We will be doing all calculations on weekly level, so we will only need the Week variable.
# Furthermore, we concatenate the Capacitytype, Entity and ConsultCode into one label, called Specialism.
data <- data %>%
  mutate(Week = week(dmy(data$Startdatum))) %>%
  mutate(Specialism = paste(Capaciteitstype, Entiteit, `Consultcode / Opname type`, sep = ", "))

# Pile -------------------------------------------------------------------
# Choose the Specialism(s) (from here on out SPECs) that should be calculated
# Plug in ".*" if you want to combine all departments and/or all codes, or use c("..", "..") if you want multiple
spec <- "CAR - arts"
department <- "Poli"
code <- "H"

# Conversion rates --------------------------------------------------------
# Check if specialism is in data
# Reasons if it's not: Typo in the three variables above, or not significant (no week with more than two appointments planned)
# Read out the Rscript that contains the conversion table
source("C:/.../Conversion_table.R")
# Find conversion rows for the SPECs
specs_found <- grepl(paste(spec, ".*", department, ".*", code, sep = ""), rownames(conv_rates))
# Find the names that are part of the SPECs (so for INT - arts, this would be INT - arts - P and INT - arts - H)
second_in_line_names <- rownames(conv_rates)[specs_found]
# If found none, stop the script and return error message. Otherwise state how many SPECs
ifelse(any(specs_found), paste0("Found ", length(second_in_line_names), " specialisms."), stop("No conversion rates found, try another specialism"))
# Print SPECs
for (second_in_line_name in 1:length(second_in_line_names)) {
  print(second_in_line_names[second_in_line_name])
}
conversion_rates <- conv_rates[specs_found, ]
# For our first in line, we will first need to know which first in lines we need (i.e. that have at least one conv.rate to one of the SPECs that is not 0)
first_in_line_names <- colnames(conversion_rates[apply(conversion_rates, 2, function(col) sum(col) > 0)])
conversion_rates <- conversion_rates[, first_in_line_names]
rm(conv_rates)

# If conversion table contains NA's, set those to 0.
conversion_rates[is.na(conversion_rates)] <- 0

# Only keep the PC input data and part of the conversion table of those specialism that are first in line
first_in_line <- data[data$Specialism %in% first_in_line_names, c("Week", "Specialism", "Totale tijd", "Aantallen")]
first_in_line <- first_in_line %>%
  rename(Planned_Capacity = Aantallen) %>%
  select(-`Totale tijd`)

# For later, the second in lines:
second_in_line <- data[data$Specialism %in% second_in_line_names, c("Week", "Specialism", "Totale tijd", "Aantallen")]
# Planned capacity will be in Aantallen for now
second_in_line <- second_in_line %>%
  rename(Planned_Capacity = Aantallen) %>%
  select(-`Totale tijd`)

# Plug in Realised Capacity, which is for now just a random variable
first_in_line$Realised_Capacity <- first_in_line$Planned_Capacity + rnorm(n = length(first_in_line$Planned_Capacity), mean = 0, sd = 1 / 3 * median(first_in_line$Planned_Capacity))
first_in_line$Realised_Capacity[first_in_line$Realised_Capacity < 0] <- 0

# Combine conv_rate with expected planned capacities for the first in line to get new patients for second in line
# Create empty columns, one for every specialism in SPECs, these will contain number of patients for that specialism coming from row-specialism
first_in_line[second_in_line_names] <- list(rep(0, nrow(first_in_line)))
# Calculate realised_capacity times conversion rate
first_in_line <- first_in_line %>%
  rowwise() %>%
  mutate(
    across(all_of(second_in_line_names),
      ~ Planned_Capacity * conversion_rates[cur_column(), Specialism],
      .names = "{.col}"
    )
  ) %>%
  ungroup()

# Sum per week the columns, so you get the total of NewPatients for that week
first_in_line_totals_for_every_week <- first_in_line %>%
  group_by(Week) %>%
  summarise(across((4:(ncol(first_in_line) - 1)), sum))

# Total number of NewPatients for the SPECs combined is combination of all columns
Planned_NewPatients <- rowSums(first_in_line_totals_for_every_week[,(2:ncol(first_in_line_totals_for_every_week))])


# Realised_NewPatients ----------------------------------------------------
# Since we have no data on Realised NewPatients up until NOW, we will just calculate them using conversion rates
first_in_line <- first_in_line %>%
  rowwise() %>%
  mutate(
    across(all_of(second_in_line_names),
           ~ Realised_Capacity * conversion_rates[cur_column(), Specialism],
           .names = "{.col}"
    )
  ) %>%
  ungroup()

# Sum per week the columns, so you get the total of NewPatients for that week
first_in_line_totals_for_every_week <- first_in_line %>%
  group_by(Week) %>%
  summarise(across((4:(ncol(first_in_line) - 1)), sum))

# Total number of NewPatients for the SPECs combined is combination of all columns
Realised_NewPatients <- rowSums(first_in_line_totals_for_every_week[,(2:ncol(first_in_line_totals_for_every_week))])
rm(first_in_line_totals_for_every_week)
first_in_line <- first_in_line %>%
  select(-c(5:ncol(first_in_line)))

# First Spec ---------------------------------------------------------
# STARTING POINT
weeks_of_supply <- 7
NOW <- 15
## USE SMALL MULTIPLES TO SHOW PLOTS FOR ALL THE FIRST LINES SPECS
## USE TREEMAP TO SHOW IMPORTANCE OF EACH FIRST LINE SPEC TO THE SECOND LINE SPECS (OR HORIZONTAL ORDERED BARPLOT / LOLLIPOPS, MAKE A COMBINED AND HIGHLIGHT IN EVERY FACET A DIFFERENT SPEC)
## AFTER CONVERSION RATES IMPORTED, USE CLUSTERING / DENDOGRAM TO SEE WHAT SPECS ARE CLOSE TO EACH OTHER (DONT FORGET TO NORMALIZE DATA)
## https://www.data-to-viz.com/caveat/annotation.html for annotating graph

# Sum planned and realised capacity by week, for all first in lines combined
first_in_line <- first_in_line %>%
  group_by(Week) %>%
  mutate(Total = sum(Planned_Capacity)) %>%
  rename(Planned_Capacity_sep = Planned_Capacity, Planned_Capacity = Total) %>%
  mutate(Total = sum(Realised_Capacity)) %>%
  rename(Realised_Capacity_sep = Realised_Capacity, Realised_Capacity = Total)

# Only keep the first sum per week, so we can create a new dataframe which has all NA's removed and keeps one total per week
first_in_line$Planned_Capacity[-seq(1, length(first_in_line$Planned_Capacity), by = length(first_in_line_names))] <- NA
first_in_line$Realised_Capacity[-seq(1, length(first_in_line$Planned_Capacity), by = length(first_in_line_names))] <- NA
first_spec <- data.frame(Week = unique(first_in_line$Week), First_In_Line = rep("Total planned appointments", length(unique(first_in_line$Week))), Planned_Capacity = first_in_line$Planned_Capacity[!is.na(first_in_line$Planned_Capacity)], Realised_Capacity = first_in_line$Realised_Capacity[!is.na(first_in_line$Realised_Capacity)])

# For now, the newpatients expected for first in lines is just the median of their combined planned capacity
first_spec$NewPatients <- median(first_spec$Planned_Capacity) # Later this has to be actual new patients gotten in the weeks up until week NOW
# Queue is created by starting with weeks of supply times median of Planned Capacity, and then accumulating negative realised capacity (patients worked through) and positive new patients (patients added to supply)
first_spec <- first_spec %>% # Later this should start with actual supply
  mutate(Queue = weeks_of_supply * median(Planned_Capacity) - cumsum(Realised_Capacity) + cumsum(NewPatients))

# Start of as if it is week NOW, so remove further realised, queue and newpatients
first_spec <- first_spec %>%
  mutate(NewPatients = ifelse(Week > NOW, NA, NewPatients)) %>%
  mutate(Queue = ifelse(Week > NOW, NA, Queue)) %>%
  mutate(Realised_Capacity = ifelse(Week > NOW, NA, Realised_Capacity))

# Plot planned, realised and queue up until week NOW
suppressWarnings(print(
  ggplot(first_spec, aes(x = Week)) +
    geom_line(aes(y = Queue), color = "black", alpha = 0.8, linewidth = 1, position = position_nudge(0)) +
    geom_bar(aes(y = Planned_Capacity), stat = "identity", fill = "lightblue", color = "black", position = position_nudge(x = -0.5), alpha = 0.3) +
    geom_bar(aes(y = Realised_Capacity), stat = "identity", fill = "orange", color = "black", alpha = 0.5, position = position_nudge((-0.5))) +
    labs(
      title = paste("Example Queue before ", spec, "", code),
      x = "Week number",
      y = "Number of expected surgeries"
    ) +
    theme_minimal() +
    geom_hline(yintercept = 6 * median(first_spec$Planned_Capacity), linetype = "dashed") +
    geom_hline(yintercept = 8 * median(first_spec$Planned_Capacity), linetype = "dashed")
))

# Predict new patients, for now just median, later this should be expected new patients using historical data
first_spec$NewPatients[(NOW+1):length(first_spec$NewPatients)] <- rep(median(first_spec$Planned_Capacity), (length(first_spec$NewPatients) - (NOW)))
# Set queue cells equal to predicted new patients minus planned capacity for that week, in the next code we will accumulate this with queue up until week NOW
first_spec$Queue[is.na(first_spec$Queue)] <- -first_spec$Planned_Capacity[is.na(first_spec$Queue)] + first_spec$NewPatients[is.na(first_spec$Queue)]
first_spec <- first_spec %>%
  mutate(Queue_fup = Queue) %>%
  mutate(Queue_upper = Queue) %>%
  mutate(Queue_lower = Queue) %>%
  mutate(Queue_2upper = Queue) %>%
  mutate(Queue_2lower = Queue)

# Calculate the queue followup (fup) using previous queue up until week NOW and already filled in values for queue cells
a <- median(first_spec$Planned_Capacity)
b <- sd(first_spec$Realised_Capacity[1:NOW] - first_spec$Planned_Capacity[1:NOW])
starting_point_fup <- min(first_spec$Queue[1:NOW])
first_spec <- first_spec %>%
  mutate(Queue_fup = accumulate(Queue_fup, ~ ifelse(.y < starting_point_fup, .y + .x, .y))) %>%
  mutate(Queue_2upper = accumulate(Queue_2upper, ~ ifelse(.y < starting_point_fup, .y + .x + 2 * b, .y))) %>%
  mutate(Queue_2lower = accumulate(Queue_2lower, ~ ifelse(.y < starting_point_fup, .y + .x - 2 * b, .y))) %>%
  mutate(Queue_upper = accumulate(Queue_upper, ~ ifelse(.y < starting_point_fup, .y + .x + b, .y))) %>%
  mutate(Queue_lower = accumulate(Queue_lower, ~ ifelse(.y < starting_point_fup, .y + .x - b, .y))) %>%
  # Set queue to NA for weeks later then NOW, so we can plot queue up to week NOW with bigger linewidth
  mutate(Queue = ifelse(Week > NOW, NA, Queue))

# Set all values before week NOW to NA, so we can plot queue up to week NOW and from week NOW on we can plot the rest
first_spec[1:(NOW - 1), c("Queue_fup", "Queue_2upper", "Queue_2lower", "Queue_upper", "Queue_lower")] <- NA

# Second Spec -------------------------------------------------------------
# Do exactly the same as for the first in lines, but now for the second in lines
second_in_line$Realised_Capacity <- second_in_line$Planned_Capacity + rnorm(n = length(second_in_line$Planned_Capacity), mean = 0, sd = 1 / 3 * median(second_in_line$Planned_Capacity))

second_in_line <- second_in_line %>%
  group_by(Week) %>%
  mutate(Total = sum(Planned_Capacity)) %>%
  rename(Planned_Capacity_sep = Planned_Capacity, Planned_Capacity = Total) %>%
  mutate(Total = sum(Realised_Capacity)) %>%
  rename(Realised_Capacity_sep = Realised_Capacity, Realised_Capacity = Total)

second_in_line$Planned_Capacity[-seq(1, length(second_in_line$Planned_Capacity), by = length(second_in_line_names))] <- NA
second_in_line$Realised_Capacity[-seq(1, length(second_in_line$Realised_Capacity), by = length(second_in_line_names))] <- NA
second_spec <- data.frame(Week = unique(second_in_line$Week), Second_In_Line = rep("Total planned appointments"), Planned_Capacity = second_in_line$Planned_Capacity[!is.na(second_in_line$Planned_Capacity)], Realised_Capacity = second_in_line$Realised_Capacity[!is.na(second_in_line$Realised_Capacity)])

# Here, new patients are the Realised_NewPatients (calculated through conversion rates, but later this is just actual patients had up until now)
second_spec$NewPatients <- Realised_NewPatients # Later this has to be actual patients input from first in lines
second_spec <- second_spec %>%
  mutate(Queue = weeks_of_supply * median(Planned_Capacity) - cumsum(Realised_Capacity) + cumsum(NewPatients))

second_spec <- second_spec %>%
  mutate(NewPatients = ifelse(Week > NOW, NA, NewPatients)) %>%
  mutate(Queue = ifelse(Week > NOW, NA, Queue)) %>%
  mutate(Realised_Capacity = ifelse(Week > NOW, NA, Realised_Capacity))

suppressWarnings(print(
  ggplot(second_spec, aes(x = Week)) +
    geom_line(aes(y = Queue), color = "black", alpha = 0.8, linewidth = 1, position = position_nudge(0)) +
    geom_bar(aes(y = Planned_Capacity), stat = "identity", fill = "lightblue", color = "black", position = position_nudge(x = -0.5), alpha = 0.3) +
    geom_bar(aes(y = Realised_Capacity), stat = "identity", fill = "orange", color = "black", alpha = 0.5, position = position_nudge((-0.5))) +
    labs(
      title = paste("Example Queue for", spec, ", ", code),
      x = "Week number",
      y = "Number of expected surgeries"
    ) +
    theme_minimal() +
    geom_hline(yintercept = 6 * median(second_spec$Planned_Capacity), linetype = "dashed") +
    geom_hline(yintercept = 8 * median(second_spec$Planned_Capacity), linetype = "dashed")
))

second_spec$NewPatients[(NOW+1):length(second_spec$NewPatients)] <- Planned_NewPatients[(NOW+1):length(Planned_NewPatients)]
second_spec$Queue[is.na(second_spec$Queue)] <- -second_spec$Planned_Capacity[is.na(second_spec$Queue)] + Planned_NewPatients[(NOW+1):length(Planned_NewPatients)]
second_spec$Queue_fup <- second_spec$Queue
second_spec$Queue_upper <- second_spec$Queue
second_spec$Queue_lower <- second_spec$Queue
second_spec$Queue_2upper <- second_spec$Queue
second_spec$Queue_2lower <- second_spec$Queue

a <- median(second_spec$Planned_Capacity)
b <- sqrt((sd(second_spec$Realised_Capacity[1:NOW] - second_spec$Planned_Capacity[1:NOW]))^2 + (sd(Realised_NewPatients[1:NOW] - Planned_NewPatients[1:NOW]))^2)
starting_point_fup <- min(second_spec$Queue[1:NOW])
second_spec <- second_spec %>%
  mutate(Queue_fup = accumulate(Queue_fup, ~ ifelse(.y < starting_point_fup, .y + .x, .y))) %>%
  mutate(Queue_2upper = accumulate(Queue_2upper, ~ ifelse(.y < starting_point_fup, .y + .x + 2 * b, .y))) %>%
  mutate(Queue_2lower = accumulate(Queue_2lower, ~ ifelse(.y < starting_point_fup, .y + .x - 2 * b, .y))) %>%
  mutate(Queue_upper = accumulate(Queue_upper, ~ ifelse(.y < starting_point_fup, .y + .x + b, .y))) %>%
  mutate(Queue_lower = accumulate(Queue_lower, ~ ifelse(.y < starting_point_fup, .y + .x - b, .y))) %>%
  mutate(Queue = ifelse(Week > NOW, NA, Queue))

second_spec[1:(NOW - 1), c("Queue_fup", "Queue_2upper", "Queue_2lower", "Queue_upper", "Queue_lower")] <- NA

# Figures -----------------------------------------------------------------
xlims <- c(0, NOW + 8) # From where to where do we want to see the patient flow

newp_fs <- ggplot(first_spec, aes(x = Week)) +
  geom_bar(aes(y = NewPatients, fill = NewPatients), stat = "identity", position = position_nudge(-0.5)) +
  theme_void() +
  scale_fill_gradient(name = "New Patients 1", low = "#F8EEEC", high = "#DB0F27") +
  theme(
    axis.line.x.bottom = element_line(linewidth = 0.5),
    legend.position = c(1.14, 0),
    legend.key.size = unit(0.4, "cm")
  ) +
  coord_cartesian(xlim = xlims, ylim = c(0, max(first_spec$NewPatients) * 1.2))

first_queue <- ggplot(first_spec, aes(x = Week)) +
  geom_line(aes(y = Queue_fup), color = "black", alpha = 0.8, linewidth = 0.6, position = position_nudge(0)) +
  geom_line(aes(y = Queue), color = "black", alpha = 0.8, linewidth = 1, position = position_nudge(0)) +
  geom_line(aes(y = c(weeks_of_supply * median(Planned_Capacity), Queue[1], rep(NA, length(Planned_Capacity) - 2))), color = "black", alpha = 0.8, linewidth = 1, position = position_nudge((-1))) +
  geom_bar(aes(y = Planned_Capacity, fill = "Planned Capacity"), stat = "identity", color = "black", position = position_nudge(x = -0.5), alpha = 0.3) +
  geom_bar(aes(y = Realised_Capacity, fill = "Realised Capacity"), stat = "identity", color = "black", alpha = 0.5, position = position_nudge((-0.5))) +
  labs(
    title = paste("Predicted Queue before", spec, "", code),
    x = "Week number",
    y = "Total duration of appoointments (in hours)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    title = element_blank()
  ) +
  geom_hline(yintercept = 6 * median(first_spec$Planned_Capacity), linetype = "dashed") +
  geom_hline(yintercept = 8 * median(first_spec$Planned_Capacity), linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  coord_cartesian(xlim = xlims, ylim = c(0, 1.5 * max(first_spec$Queue[1:NOW]))) +
  geom_ribbon(aes(xmin = NOW, ymin = Queue_lower, ymax = Queue_upper, fill = "First CI"), color = "lightgreen", alpha = 0.4, position = position_nudge(0), linetype = "dashed", linewidth = 0.6) +
  geom_ribbon(aes(xmin = NOW, ymin = Queue_2lower, ymax = Queue_2upper, fill = "Second CI"), color = "red", alpha = 0.2, position = position_nudge(0), linetype = "dashed", linewidth = 0.6) +
  scale_y_continuous(sec.axis = sec_axis(~ . / median(first_spec$Planned_Capacity[1:NOW]), breaks = seq(0, 12, by = 2), name = "Weeks of supply")) +
  scale_fill_manual(name = "Legend", values = c("Planned Capacity" = "lightblue", "Realised Capacity" = "orange", "First CI" = "yellow", "Second CI" = "red"), limits = c("Planned Capacity", "Realised Capacity", "First CI", "Second CI"))

newp_ss <- ggplot(second_spec, aes(x = Week)) +
  geom_bar(aes(y = NewPatients, fill = NewPatients), stat = "identity", position = position_nudge((-0.5))) +
  theme_void() +
  scale_fill_gradient(name = "New Patients 2", low = "#F8EEEC", high = "#DB0F27") +
  theme(
    axis.line.x.bottom = element_line(linewidth = 0.5),
    legend.position = c(1.14, 0.5),
    legend.key.size = unit(0.4, "cm")
  ) +
  coord_cartesian(xlim = xlims, ylim = c(0, max(second_spec$NewPatients) * 1.2))


second_queue <- ggplot(second_spec, aes(x = Week)) +
  geom_line(aes(y = Queue_2lower), color = "red", linetype = "dashed", alpha = 0.8, linewidth = 0.6, position = position_nudge(0)) +
  geom_line(aes(y = Queue_2upper), color = "red", linetype = "dashed", alpha = 0.8, linewidth = 0.6, position = position_nudge(0)) +
  geom_line(aes(y = Queue_lower), color = "lightgreen", linetype = "dashed", alpha = 0.8, linewidth = 0.6, position = position_nudge(0)) +
  geom_line(aes(y = Queue_upper), color = "lightgreen", linetype = "dashed", alpha = 0.8, linewidth = 0.6, position = position_nudge(0)) +
  geom_line(aes(y = Queue_fup, color = "Expected supply"), alpha = 0.8, linewidth = 0.6, position = position_nudge(0)) +
  geom_line(aes(y = Queue, color = "Supply"), alpha = 0.8, linewidth = 1, position = position_nudge(0)) +
  geom_line(aes(y = c(weeks_of_supply * median(Planned_Capacity), Queue[1], rep(NA, length(Planned_Capacity) - 2))), color = "black", alpha = 0.8, linewidth = 1, position = position_nudge((-1))) +
  geom_bar(aes(y = Planned_Capacity), fill = "lightblue", stat = "identity", color = "black", position = position_nudge(x = -0.5), alpha = 0.3) +
  geom_bar(aes(y = Realised_Capacity), fill = "orange", stat = "identity", color = "black", alpha = 0.5, position = position_nudge((-0.5))) +
  labs(
    title = paste("Predicted Queue for", spec, ", ", code),
    x = "Week number",
    y = "Number of surgeries"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 6 * median(second_spec$Planned_Capacity), linetype = "dashed") +
  geom_hline(yintercept = 8 * median(second_spec$Planned_Capacity), linetype = "dashed") +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  coord_cartesian(xlim = xlims, ylim = c(0, 1.5 * max(second_spec$Queue[1:NOW]))) +
  geom_ribbon(aes(ymin = Queue_lower, ymax = Queue_upper), fill = "yellow", alpha = 0.4, position = position_nudge(0)) +
  geom_ribbon(aes(ymin = Queue_2lower, ymax = Queue_2upper), fill = "red", alpha = 0.2, position = position_nudge(0)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / median(second_spec$Planned_Capacity[1:NOW]), breaks = seq(0, 12, by = 2), name = "Weeks of supply")) +
  scale_color_manual(breaks = c("Supply", "Expected supply"), values = c("black", "black")) +
  theme(
    axis.title.y = element_text(size = 11),
    title = element_blank()
  )


suppressWarnings(print(
  ggarrange(
    newp_fs +
      theme(plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), "cm")),
    first_queue,
    newp_ss +
      theme(plot.margin = unit(c(0.3, 0.1, 0.1, 0.1), "cm")),
    second_queue,
    labels = c("", paste("Before ", spec, "", code), "", paste(spec, "", code)),
    label.x = 0.05,
    label.y = 1,
    ncol = 1,
    nrow = 4,
    align = "v",
    heights = c(1, 4, 1, 4)
  )
))

