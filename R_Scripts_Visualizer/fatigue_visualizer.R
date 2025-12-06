library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

fatigue <- read_csv("./data/fatigue.csv")



fatigue <- fatigue %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

fatigue <- fatigue %>%
  select(id, created_at, everything())
fatigue

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
fatigue <- fatigue %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count fatigue per month
monthly_counts <- fatigue %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of fatigue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

skim(fatigue)



# add column symptom and fill "fatigue"

fatigue <- fatigue %>%
  mutate(
    symptom = "fatigue"
  )


# remove unecessary columns
fatigue <- fatigue %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)

# remove NAs from selftext

fatigue <- fatigue %>%
  filter(!is.na(selftext))

# Save the modified fatigue data
write_csv(fatigue, "./data/fatigue_final.csv")

# Display the fatigue data
fatigue


# Recent 10K posts 

fatigue_10K <- fatigue %>%
  arrange(desc(created_at)) %>%
  head(10000)

# save the recent 10K posts

write_csv(fatigue_10K, "./data/fatigue_10K.csv")

fatigue_10K
