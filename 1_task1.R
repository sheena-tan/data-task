#### Task 1 ----
#### feelings with/without apology, variation by initiator type, forgiveness

library(tidyverse)
library(here)
library(lsr)

# load data
apology_clean <- read_rds(here("data/apology_clean.rds"))


# QUESTION 1 ----

# transform data for pairwise t-test
apology_long <- apology_clean |>
  select(ResponseId, feelings_youalone, feelings_bothyoufirst) |>
  rename(
    no = feelings_youalone,
    yes = feelings_bothyoufirst
  ) |>
  pivot_longer(
    cols = c("no", "yes"),
    names_to = "apology_returned"
  ) |>
  arrange(apology_returned, ResponseId)

# summary statistics
apology_long |> group_by(apology_returned) |>
  summarize(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

# pairwise t-test
t.test(value ~ apology_returned, apology_long, paired = TRUE)

