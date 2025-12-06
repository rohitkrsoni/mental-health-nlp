library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

difficulty_concentrating <- read_csv("./data/difficulty_concentrating.csv")



difficulty_concentrating <- difficulty_concentrating %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

difficulty_concentrating <- difficulty_concentrating %>%
  select(id, created_at, everything())
difficulty_concentrating

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
difficulty_concentrating <- difficulty_concentrating %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count difficulty_concentrating per month
monthly_counts <- difficulty_concentrating %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of difficulty_concentrating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

skim(difficulty_concentrating)



# add column symptom and fill "difficulty_concentrating"

difficulty_concentrating <- difficulty_concentrating %>%
  mutate(
    symptom = "difficulty_concentrating"
  )


# remove unecessary columns
difficulty_concentrating <- difficulty_concentrating %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)

# remove NAs from selftext

difficulty_concentrating <- difficulty_concentrating %>%
  filter(!is.na(selftext))

# Save the modified difficulty_concentrating data
write_csv(difficulty_concentrating, "./data/difficulty_concentrating_final.csv")

# Display the difficulty_concentrating data
difficulty_concentrating


# Recent 10K posts 

difficulty_concentrating_10K <- difficulty_concentrating %>%
  arrange(desc(created_at)) %>%
  head(10000)

# save the recent 10K posts

write_csv(difficulty_concentrating_10K, "./data/difficulty_concentrating_10K.csv")

difficulty_concentrating_10K
