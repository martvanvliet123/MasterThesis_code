library(ggplot2)
library(ggpubr)

# Each week, 10 patients arrive, all for ORT.
# They must go from Appointments (App) directly to Surgery (Surg) and from Surgery directly to Hospitalization (Hosp).

# THE MODEL -----------------------------------------------------------------

# Model duration
model_duration <- 12 # weeks

# Initial queue sizes:
initial_app_queue <- 50
initial_surg_queue <- 10
initial_hosp_patients <- 12

# Average length of hospital stay
length_of_stay <- 2 # weeks

# Load conversion rates
source("C:/.../conversierates.R")

# Number of patients arriving each week
new_patients_per_week <- data.frame(week = seq(1, model_duration, by = 1), new = rep(6, times = model_duration))

# Maximum number of appointments per week
max_app_per_week <- data.frame(week = seq(1, model_duration, by = 1), nr_app = c(8,8,8,8,8,8,8,8,4,4,8,8))

# Maximum number of surgeries per week
max_surg_per_week <- data.frame(week = seq(1, model_duration, by = 1), nr_surg = c(6,6,8,8,8,4,4,8,8,6,6,6))

# APPOINTMENTS
# Week 0: 10 new patients enter the ORT queue for appointments, no appointments have taken place yet
app_queue_per_week <- data.frame(week = c(0), nr_pat = initial_app_queue, wait_time = c(NA))
app_week <- data.frame(week = c(0), nr_app = c(0))

# SURGERY
# Assuming we can perform 10 surgeries per week
# Week 0: No surgeries have taken place yet, but 10 patients are waiting for surgery
surg_queue_per_week <- data.frame(week = c(0), nr_pat = initial_surg_queue, wait_time = c(NA))
surg_week <- data.frame(week = c(0), nr_surg = c(0))

# HOSPITALIZATION
# Patients are discharged once per week before new surgeries take place.
# Assume each patient stays for 1 week.
num_hosp = rep(0, times = model_duration)
discharge_per_week <- data.frame(week = seq(1, model_duration, by = 1), nr_hosp = num_hosp)
hosp_occupancy_per_week <- data.frame(week = c(0), nr_pat_morning = c(initial_hosp_patients), nr_pat_evening = c(initial_hosp_patients))

# MAIN SIMULATION LOOP
for (i in 1:model_duration) {
  # HOSPITAL DISCHARGE
  discharged_this_week <- discharge_per_week[discharge_per_week$week == i, "nr_hosp"]
  hosp_occupancy_per_week <- rbind(hosp_occupancy_per_week, c(i, hosp_occupancy_per_week[hosp_occupancy_per_week$week == (i-1), "nr_pat_evening"] - discharged_this_week, NA))
  
  # APPOINTMENTS
  max_app_this_week <- max_app_per_week[max_app_per_week$week == i, "nr_app"]
  app_this_week <- min(max_app_this_week, app_queue_per_week[nrow(app_queue_per_week), "nr_pat"])
  app_week <- rbind(app_week, list(i, app_this_week))
  
  not_served <- max(app_queue_per_week[nrow(app_queue_per_week), "nr_pat"] - app_this_week)
  new_patients <- not_served + new_patients_per_week[new_patients_per_week$week == i, "new"]
  
  wait_time = 0 
  leftover = new_patients
  for (j in i:model_duration) {
    leftover <- leftover - max_app_per_week[j+1, "nr_app"]
    if (leftover <= 0 | is.na(leftover)) {
      break
    }
    wait_time <- wait_time + 1
  }
  app_queue_per_week <- rbind(app_queue_per_week, list(i, new_patients, wait_time))
  
  # SURGERY
  max_surg_this_week <- max_surg_per_week[max_surg_per_week$week == i, "nr_surg"]
  surg_this_week <- min(c(max_surg_this_week, surg_queue_per_week$nr_pat[surg_queue_per_week$week == (i-1)]))
  surg_week <- rbind(surg_week, list(i, surg_this_week))
  
  not_served <- max(c(surg_queue_per_week[nrow(surg_queue_per_week), "nr_pat"] - surg_this_week, 0))
  new_patients <- not_served + ceiling((1 - dropout - conversion_rate_poli) * app_week[nrow(app_week), "nr_app"])
  
  wait_time = 0 
  leftover = new_patients
  for (j in i:model_duration) {
    leftover <- leftover - max_surg_per_week[j+1, "nr_surg"]
    if (leftover <= 0 | is.na(leftover)) {
      break
    }
    wait_time <- wait_time + 1
  }
  surg_queue_per_week <- rbind(surg_queue_per_week, list(i, new_patients, wait_time))
  
  # HOSPITALIZATION UPDATE
  discharge_per_week[discharge_per_week$week == (i + length_of_stay), "nr_hosp"] <- discharge_per_week[discharge_per_week$week == (i + length_of_stay), "nr_hosp"] + surg_this_week
  new_hosp <- hosp_occupancy_per_week[nrow(hosp_occupancy_per_week), "nr_pat_morning"] + ceiling(conversion_rate_poli * app_this_week) + surg_this_week
  hosp_occupancy_per_week[nrow(hosp_occupancy_per_week), "nr_pat_evening"] <- new_hosp
} 

# PLOT RESULTS
par(mfrow = c(1,2))
p1 <- ggplot(app_week, aes(x = week, y = nr_app)) + geom_bar(stat = "identity") + ggtitle("Appointments per week")
p2 <- ggplot(app_queue_per_week, aes(x = week, y = nr_pat)) + geom_bar(stat = "identity") + ggtitle("Appointment Queue")
p3 <- ggplot(surg_week, aes(x = week, y = nr_surg)) + geom_bar(stat = "identity") + ggtitle("Surgeries per week")
p4 <- ggplot(surg_queue_per_week, aes(x = week, y = nr_pat)) + geom_bar(stat = "identity") + ggtitle("Surgery Queue")
p5 <- ggplot(discharge_per_week, aes(x = week, y = nr_hosp)) + geom_bar(stat = "identity") + ggtitle("Discharges per week")
p6 <- ggplot(hosp_occupancy_per_week, aes(x = week, y = nr_pat_morning)) + geom_bar(stat = "identity") + ggtitle("Hospitalized patients per week")
ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)
