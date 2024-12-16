# On this page, I am:
# Computing the minority population proportion for each school across each state and finding the schools with the highest minority population.
# Finding the top 10 schools with the highest enrollment rates in Free and Reduced Lunch Programs
# Breaking down the minority population for these schools

library(dplyr)
library(ggplot2)

# Convert race columns to numeric
clean_aid_data <- clean_aid_data %>%
  mutate(
    `American Indian/Alaska Native Students [Public School] 2022-23` = as.numeric(`American Indian/Alaska Native Students [Public School] 2022-23`),
    `Asian or Asian/Pacific Islander Students [Public School] 2022-23` = as.numeric(`Asian or Asian/Pacific Islander Students [Public School] 2022-23`),
    `Hispanic Students [Public School] 2022-23` = as.numeric(`Hispanic Students [Public School] 2022-23`),
    `Black or African American Students [Public School] 2022-23` = as.numeric(`Black or African American Students [Public School] 2022-23`),
    `White Students [Public School] 2022-23` = as.numeric(`White Students [Public School] 2022-23`),
    `Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2022-23` = as.numeric(`Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2022-23`),
    `Two or More Races Students [Public School] 2022-23` = as.numeric(`Two or More Races Students [Public School] 2022-23`),
    `Total Race/Ethnicity [Public School] 2022-23` = as.numeric(`Total Race/Ethnicity [Public School] 2022-23`)
  )

# Calculate minority population and filter top 10 schools
top_minority_schools <- clean_aid_data %>%
  mutate(
    Minority_Population = 
      `American Indian/Alaska Native Students [Public School] 2022-23` +
      `Asian or Asian/Pacific Islander Students [Public School] 2022-23` +
      `Hispanic Students [Public School] 2022-23` +
      `Black or African American Students [Public School] 2022-23` +
      `Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2022-23` +
      `Two or More Races Students [Public School] 2022-23`
  ) %>%
  arrange(desc(Minority_Population)) %>%  # Sort by Minority Population descending
  head(10) %>%                            # Keep top 10 schools
  select(`School Name`, Minority_Population, `Total Race/Ethnicity [Public School] 2022-23`) %>%
  mutate(Minority_Percentage = (Minority_Population / `Total Race/Ethnicity [Public School] 2022-23`) * 100)

# Display the results
print(top_minority_schools)

# Issue I am running into - Some of the schools are so small and they only have a minority population

# Calculating enrollment rates for the top 10 schools with most students in Free and Reduced Lunch Programs

schools <- c(
  "HIGHLANDS COMMUNITY CHARTER",
  "NORTH STAR ACADEMY CHARTER SCHOOL",
  "ALABAMA CONNECTIONS ACADEMY",
  "READING SHS",
  "CALIFORNIA VIRTUAL ACADEMY @ LOS ANGELES",
  "UPPER DARBY SHS",
  "RIVER SPRINGS CHARTER",
  "NORTH SHORE SENIOR HIGH",
  "VISIONS IN EDUCATION",
  "PARAMOUNT HIGH"
)

clean_aid_data <- clean_aid_data %>%
  mutate(
    `Free and Reduced Lunch Students [Public School] 2022-23` = as.numeric(`Free and Reduced Lunch Students [Public School] 2022-23`),
    `Total Race/Ethnicity [Public School] 2022-23` = as.numeric(`Total Race/Ethnicity [Public School] 2022-23`)
  )

# Filter the data to include only the specified schools
filtered_schools <- clean_aid_data %>%
  filter(`School Name` %in% schools, `Total Race/Ethnicity [Public School] 2022-23` > 0) %>%
  mutate(
    Lunch_Enrollment_Rate = `Free and Reduced Lunch Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23`
  ) %>%
  select(`School Name`, 
         `Free and Reduced Lunch Students [Public School] 2022-23`, 
         `Total Race/Ethnicity [Public School] 2022-23`, 
         Lunch_Enrollment_Rate)

# Display the results
print(filtered_schools)

# Breakdown minority populations 

clean_aid_data <- clean_aid_data %>%
  mutate(
    `American Indian/Alaska Native Students [Public School] 2022-23` = as.numeric(`American Indian/Alaska Native Students [Public School] 2022-23`),
    `Asian or Asian/Pacific Islander Students [Public School] 2022-23` = as.numeric(`Asian or Asian/Pacific Islander Students [Public School] 2022-23`),
    `Hispanic Students [Public School] 2022-23` = as.numeric(`Hispanic Students [Public School] 2022-23`),
    `Black or African American Students [Public School] 2022-23` = as.numeric(`Black or African American Students [Public School] 2022-23`),
    `White Students [Public School] 2022-23` = as.numeric(`White Students [Public School] 2022-23`),
    `Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2022-23` = as.numeric(`Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2022-23`),
    `Two or More Races Students [Public School] 2022-23` = as.numeric(`Two or More Races Students [Public School] 2022-23`),
    `Total Race/Ethnicity [Public School] 2022-23` = as.numeric(`Total Race/Ethnicity [Public School] 2022-23`)
  )

# Filter for the Top 10 Schools and calculate race percentages
race_breakdown <- clean_aid_data %>%
  filter(`School Name` %in% schools, `Total Race/Ethnicity [Public School] 2022-23` > 0) %>%
  mutate(
    American_Indian_Percent = `American Indian/Alaska Native Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100,
    Asian_Percent = `Asian or Asian/Pacific Islander Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100,
    Hispanic_Percent = `Hispanic Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100,
    Black_Percent = `Black or African American Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100,
    White_Percent = `White Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100,
    Hawaiian_Percent = `Nat. Hawaiian or Other Pacific Isl. Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100,
    Two_Or_More_Races_Percent = `Two or More Races Students [Public School] 2022-23` / `Total Race/Ethnicity [Public School] 2022-23` * 100
  ) %>%
  select(`School Name`,
         American_Indian_Percent,
         Asian_Percent,
         Hispanic_Percent,
         Black_Percent,
         White_Percent,
         Hawaiian_Percent,
         Two_Or_More_Races_Percent)

# Display the results
print(race_breakdown)
