library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

suicidal_ideation <- read_csv("./data/suicidal_ideation.csv")



suicidal_ideation <- suicidal_ideation %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

suicidal_ideation <- suicidal_ideation %>%
  select(id, created_at, everything())
suicidal_ideation

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
suicidal_ideation <- suicidal_ideation %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count suicidal_ideation per month
monthly_counts <- suicidal_ideation %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of suicidal_ideation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

skim(suicidal_ideation)



# add column symptom and fill "suicidal_ideation"

suicidal_ideation <- suicidal_ideation %>%
  mutate(
    symptom = "suicidal_ideation"
  )


# remove unecessary columns
suicidal_ideation <- suicidal_ideation %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)

# remove NAs from selftext

suicidal_ideation <- suicidal_ideation %>%
  filter(!is.na(selftext))

# Save the modified suicidal_ideation data
write_csv(suicidal_ideation, "./data/suicidal_ideation_final.csv")

# Display the suicidal_ideation data
suicidal_ideation


# Recent 10K posts 

suicidal_ideation_10K <- suicidal_ideation %>%
  arrange(desc(created_at)) %>%
  head(10000)

# save the recent 10K posts

write_csv(suicidal_ideation_10K, "./data/suicidal_ideation_10K.csv")

suicidal_ideation_10K
