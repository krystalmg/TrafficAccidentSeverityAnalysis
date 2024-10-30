library(dplyr)

# read datasets
accident <- read.csv("FARS2022NationalCSV/accident.csv", encoding = "latin1")
person <- read.csv("FARS2022NationalCSV/person.csv", encoding = "latin1")
vehicle <- read.csv("FARS2022NationalCSV/vehicle.csv", encoding = "latin1")

# midwest states
midwest_states <- c('Indiana', 'Illinois', 'Michigan', 'Ohio', 'Wisconsin', 
                    'Iowa', 'Kansas', 'Minnesota', 'Missouri', 
                    'Nebraska', 'North Dakota', 'South Dakota')


midwest_accident <- accident %>% filter(STATENAME %in% midwest_states)

midwest_person <- person %>% filter(ST_CASE %in% midwest_accident$ST_CASE)
midwest_vehicle <- vehicle %>% filter(ST_CASE %in% midwest_accident$ST_CASE)

# combine all three by ST_CASE
combined_midwest_data <- midwest_accident %>%
  full_join(midwest_person, by = "ST_CASE") %>%
  full_join(midwest_vehicle, by = "ST_CASE")

# remove duplicate ST_CASE entries, keeping the first occurrence
combined_midwest_data_unique <- combined_midwest_data %>% distinct(ST_CASE, .keep_all = TRUE)

# saves the deduplicated dataset to csv
write.csv(combined_midwest_data_unique, "combined_midwest_data.csv", row.names = FALSE)
