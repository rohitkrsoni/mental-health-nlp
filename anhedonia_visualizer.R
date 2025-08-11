library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)

anhedonia <- read_csv("./data/anhedonia.csv")



anhedonia <- anhedonia %>%
  mutate(
    created_at = as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), 
    year = year(created_at),
    month = month(created_at, label = TRUE),
    day = day(created_at)
  )

anhedonia <- anhedonia %>%
  select(id, created_at, everything())
anhedonia

# Step 1: Create a proper Date for ordering (e.g., 2023-01-01), and label (e.g., "Jan 2023")
anhedonia <- anhedonia %>%
  mutate(
    month_start = floor_date(created_at, "month"),
    month_year = format(month_start, "%b %Y")
  )

# Step 2: Count anhedonia per month
monthly_counts <- anhedonia %>%
  count(month_start, month_year) %>%
  arrange(month_start) %>%
  mutate(month_year = factor(month_year, levels = unique(month_year)))


# Step 3: Plot
ggplot(monthly_counts, aes(x = month_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Monthly Post Counts",
    x = "Month",
    y = "Number of anhedonia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

skim(anhedonia)



# add column symptom and fill "anhedonia"

anhedonia <- anhedonia %>%
  mutate(
    symptom = "anhedonia"
  )


# remove unecessary columns
anhedonia <- anhedonia %>%
  select(-subreddit, -month_year, -month, -created_utc, -day, -url, -month_start, -year)

# remove NAs from selftext

anhedonia <- anhedonia %>%
  filter(!is.na(selftext))

# Save the modified anhedonia data
write_csv(anhedonia, "./data/anhedonia_final.csv")

# Display the anhedonia data
anhedonia


# Recent 10K posts 

anhedonia_10K <- anhedonia %>%
  arrange(desc(created_at)) %>%
  head(10000)

# save the recent 10K posts

write_csv(anhedonia_10K, "./data/anhedonia_10K.csv")

anhedonia_10K
