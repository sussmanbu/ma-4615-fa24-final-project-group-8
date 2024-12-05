library(tidyverse)
library(here)
library(dplyr)

# Read the CSV file, including the first row as data (col_names = FALSE)
our_data <- read_csv(here::here("dataset", "ELSI_aid.csv"), skip = 5, col_names = TRUE)

# Assign the first row as column names
colnames(our_data) <- our_data[1, ]

# Remove the first row now that it has been set as column names
our_data <- our_data[-1, ]

# Clean aid data
# Remove columns from "State Name [Public School] 2022-23" to "State Name [Public School] 2018-19"
# Remove rows with "†", "-", "‡", or negative values
cleaned_data <- our_data |>
  select(-c(`State Name [Public School] 2022-23`:`School Name [Public School] 2018-19`)) |>
  filter_all(all_vars(!str_detect(., "†|\\–|‡"))) |>
  filter_all(all_vars(as.numeric(.) >= 0 | is.na(as.numeric(.)))) |>
  mutate(`State Name [Public School] Latest available year` = str_to_title(`State Name [Public School] Latest available year`))

# Save the cleaned data as an .rds file
write_rds(cleaned_data, file = here::here("dataset", "clean_aid_data.rds"))
