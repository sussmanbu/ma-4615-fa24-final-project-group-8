# On this page, I am computing the minority population proportion for each school across each state.
# This will be used to cross-compare with our second data set. 
# That is, are these numbers representative of the population in those surrounding areas?
# Then, I want to find the schools with the highest number of students enrolled in a government program. 
# Do these match with the schools with the highest minority population?

library(dplyr)
library(ggplot2)

# Create a list of terms that identify the minority group columns 
minority_patterns <- c(
  "American Indian/Alaska Native Students",
  "Asian or Asian/Pacific Islander Students",
  "Hispanic Students",
  "Black or African American Students",
  "Nat. Hawaiian or Other Pacific Isl. Students",
  "Two or More Races Students"
)

# Match columns with the titles
minority_columns <- grep(paste(minority_patterns, collapse = "|"), colnames(new_data_clean), value = TRUE)
non_minority_columns <- grep("White Students", colnames(new_data_clean), value = TRUE)
total_students_columns <- grep("Total Race/Ethnicity", colnames(new_data_clean), value = TRUE)

# Convert matched columns to numeric for calculations
new_data_clean <- new_data_clean %>%
  mutate(across(all_of(minority_columns), as.numeric, .names = "numeric_{col}")) %>%
  mutate(across(all_of(non_minority_columns), as.numeric, .names = "numeric_{col}")) %>%
  mutate(across(all_of(total_students_columns), as.numeric, .names = "numeric_{col}"))

# Update columns to use the numeric versions
minority_numeric_columns <- grep("^numeric_", names(new_data_clean), value = TRUE, perl = TRUE)
minority_numeric_columns <- minority_numeric_columns[grepl(paste(minority_patterns, collapse = "|"), minority_numeric_columns)]

non_minority_numeric_columns <- grep("^numeric_", names(new_data_clean), value = TRUE, perl = TRUE)
non_minority_numeric_columns <- non_minority_numeric_columns[grepl("White Students", non_minority_numeric_columns)]

total_students_numeric_columns <- grep("^numeric_", names(new_data_clean), value = TRUE, perl = TRUE)
total_students_numeric_columns <- total_students_numeric_columns[grepl("Total Race/Ethnicity", total_students_numeric_columns)]

# Perform row-wise calculations for each school 
new_data_clean <- new_data_clean %>%
  mutate(
    Minority_Total = rowSums(select(., all_of(minority_numeric_columns)), na.rm = TRUE),
    Non_Minority_Total = rowSums(select(., all_of(non_minority_numeric_columns)), na.rm = TRUE),
    Total_Students = rowSums(select(., all_of(total_students_numeric_columns)), na.rm = TRUE),
    Minority_Proportion = Minority_Total / Total_Students,
    Non_Minority_Proportion = Non_Minority_Total / Total_Students
  )

# Sums up the total minority, non-minority, and total student counts for each state 
# and calculates proportions of minorities against non-minorities
statewise_data <- new_data_clean %>%
  group_by(`State Name [Public School] Latest available year`) %>%
  summarise(
    Total_Minority = sum(Minority_Total, na.rm = TRUE),
    Total_Non_Minority = sum(Non_Minority_Total, na.rm = TRUE),
    Total_Students = sum(Total_Students, na.rm = TRUE),
    Minority_Proportion = Total_Minority / Total_Students,
    Non_Minority_Proportion = Total_Non_Minority / Total_Students
  )

# Print school-level proportions
print(new_data_clean %>% 
        select(`School Name`, `State Name [Public School] Latest available year`, Minority_Proportion, Non_Minority_Proportion))

# Print state-level aggregates
print(statewise_data)

# Graph Top 10 Schools with highest minority population proportion
top_schools <- new_data_clean %>%
  arrange(desc(Minority_Proportion)) %>%
  head(10)

# Issue I am running into - Some of the schools are so small and they only have a minority population

# Bar graph
ggplot(top_schools, aes(x = reorder(`School Name`, Minority_Proportion), y = Minority_Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Schools with Highest Minority Population Proportion",
       x = "School Name",
       y = "Minority Population Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Graph Top 10 States with highest minority population proportion
top_states <- statewise_data %>%
  arrange(desc(Minority_Proportion)) %>%
  head(10)

# Bar graph
ggplot(top_states, aes(x = reorder(`State Name [Public School] Latest available year`, Minority_Proportion), y = Minority_Proportion)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Top 10 States with Highest Minority Population Proportion",
       x = "State Name",
       y = "Minority Population Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
