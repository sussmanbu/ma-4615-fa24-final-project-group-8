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

our_data <- read_csv(here::here("dataset", "ELSIPreliminaryDataSet.csv"), skip = 5, col_names = TRUE)

# Clean the new data set - for Jakob's use
new_data <- read_csv(here::here("dataset", "ELSIDataSet2.csv"), skip = 5, col_names = TRUE)

# Remove the last 5 columns
new_data <- new_data[, 1:62]

# Remove rows with "†", "-", "‡", or negative values
new_data_clean <- new_data |>
  filter_all(all_vars(!str_detect(., "†|\\–|‡"))) |>
  filter_all(all_vars(as.numeric(.) >= 0 | is.na(as.numeric(.)))) |>
  mutate(`State Name [Public School] Latest available year` = str_to_title(`State Name [Public School] Latest available year`))

# Save the cleaned data as an .rds file
write_rds(new_data_clean, file = here::here("dataset", "new_data_clean.rds"))

df_ndc <- data.frame(new_data_clean)

new_data_cleann <- new_data_clean |> 
  mutate(across(c(`Grades 1-8 Students [Public School] 2022-23`:`Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2020-21`), as.numeric)) |> 
  select(matches("19-20"), `School Name`, `State Name [Public School] Latest available year`) |> 
  mutate(total = sum(`Male Students [Public School] 2019-20`, `Female Students [Public School] 2019-20`), .by = `School Name`) |> 
  mutate(prop_white = `White Students [Public School] 2019-20`/total, .by = `School Name`) |> 
  mutate(prop_Black = `Black or African American Students [Public School] 2019-20`/total, .by = `School Name`) |> 
  mutate(prop_Asian = `Asian or Asian/Pacific Islander Students [Public School] 2019-20`/total, .by = `School Name`) |> 
  mutate(prop_hisp = `Hispanic Students [Public School] 2019-20`/total, .by = `School Name`)
