library(tidyverse)
library(here)

our_data <- read_csv(here::here("dataset", "ELSIPreliminaryDataSet.csv"), skip = 5, col_names = TRUE)

# Clean the data set
# Remove rows with "†", "-", "‡", or negative values
our_data_clean <- our_data |>
  filter_all(all_vars(!str_detect(., "†|\\–|‡"))) |>
  filter_all(all_vars(as.numeric(.) >= 0 | is.na(as.numeric(.)))) |>
  mutate(`State Name [Public School] Latest available year` = str_to_title(`State Name [Public School] Latest available year`))

# Save the cleaned data as an .rds file
write_rds(our_data_clean, file = here::here("dataset", "our_data_clean.rds"))

