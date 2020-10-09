#import and cleanup traffic stop data

#install.packages("readr")
#install.packages("dplyr")
#install.packages("lubridate")

# import data
library (readr)
tstops <- tstops_jan2014

# review data
library(dplyr)
glimpse(tstops)
names(tstops)

# clean up data
library(lubridate)
tstops$intervention_date <- mdy(tstops$intervention_date)
tstops$subject_age <- as.numeric(tstops$subject_age)

#create factors
tstops$subject_race <- factor(tstops$subject_race,
                              levels = c("W", "B", "A", "I"),
                              labels = c("White", "Black", "Asian", "American Indian"))

tstops$subject_ethnicity <- factor(tstops$subject_ethnicity,
                                   levels = c("H", "N", "M"),
                                   labels = c("Hispanic", "Non-Hispanic", "Middle Eastern"))

tstops$subject_sex <- factor(tstops$subject_sex,
                             levels= c("M", "F"),
                             labels = c("Male", "Female"))
  
tstops$intervention_techinque <- factor(tstops$intervention_technique,
                                        levels = c("B", "G", "S"),
                                        labels = c("Blind", "General", "Spot"))

tstops$intervention_reason <-  factor(tstops$intervention_reason,
                                      levels = c("E", "I", "V"),
                                      labels = c("Equipment Violation", "Investigative Stop", 
                                                 "Motor Vehicle Violation"))

tstops$intervention_duration <- factor(tstops$intervention_duration,
                                       levels = c(1,2,3),
                                       labels = c("0-15 min","16-30 mins", "over 30 mins"))

tstops$intervention_disposition <- factor(tstops$intervention_disposition,
                                       levels = c("I", "M","N", "U", "V", "W"),
                                       labels = c("Infraction", "Misdemeanor Summons",
                                       "No Disposition", "Uniform Arrest Report",
                                       "Verbal Warning", "Written Warning"))

tstops$vehicle_searched<- factor(tstops$intervention_disposition,
                                 levels = c("I","M","N","U", "V", "W"),
                                 labels = c("Infraction", "Misdemenor Summons",
                                            "No Disoposition", "Uniform Arrest Report",
                                            "Verbal Warning", "Written Warning"))

tstops$contraband <- factor(tstops$vehicle_searched,
                           levels = c(0, 1),
                           labels = c("no", "yes"))

tstops$custodial_arrest <- factor(tstops$contraband,
                                 levels = c(0,1),
                                 labels = c("no","yes"))

tstops$days_of_week <- factor(tstops$day_of_week,
                              levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday"))

#review data
summary(tstops)

#fix age
hist(tstops$subject_age)
table(tstops$subject_age)
tstops$subject_age <- ifelse(tstops$subject_age < 16 |
                               tstops$subject_age > 99,
                             NA, tstops$subject_age)

#clean
tstops$intervention_month <- NULL
tstops$intervention_year <- NULL

#save cleaned data
saveRDS(tstops, file="traffic-cleaned.rds")




