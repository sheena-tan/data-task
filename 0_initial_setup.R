#### Initial Setup ----
#### data cleaning, eda

# Notes: There definitely seems to be a significant difference between the feelings
# of those who do and don't get return apologies (Task1). There also seems to be
# significant differences between the different types of people (Task2). I wonder if this
# maps onto agreeableness personality-wise. Could also be more related to people's
# lived experiences and morals?


library(tidyverse)
library(here)
library(patchwork)

# load data
apology_data <- read_csv(here("data/apology_data.csv"))

skimr::skim(apology_data)
# consent: n_unique = 1; all responses agreed
# passedattn: n_unique = 2; need to clean responses that did not pass
# Progress: mean = 84.1; need to clean responses that did not answer about feelings


## data cleaning ----
apology_clean <- apology_data |>
  filter(IPAddress != "72.83.222.239") |> #see comments field
  filter(Progress > 30) |>
  filter(passedattn == "yes") |>
  select(initiator_type, starts_with("feelings_"), starts_with("outcome_"),
         blame_1, real_imaginary, describe, comments)


## eda ----
plot_density_boxplot <- function(data, x_variable) {
  p1 <- ggplot(data, aes({{x_variable}})) +
    geom_boxplot() +
    theme_void()

  p2 <- ggplot(data, aes({{x_variable}})) +
    geom_density() +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  p1/p2 + plot_layout(heights = unit(c(1, 5), c("cm", "null")))
}

plot_density_boxplot(apology_clean, feelings_youalone)
# overall, people feel very bad (-30,-10) when they do not get a return apology
plot_density_boxplot(apology_clean, feelings_bothyoufirst)
# overall, people feel much better (0,20) when they do get a return apology


apology_clean |> filter(initiator_type == "always") |>
  plot_density_boxplot(feelings_youalone)
# always-type do not feel good (-20,-10) with no return apology, but better than the population average
apology_clean |> filter(initiator_type == "always") |>
  plot_density_boxplot(feelings_bothyoufirst)
# always-type feel much better (10,20) when both people apologize


apology_clean |> filter(initiator_type == "conditional") |>
  plot_density_boxplot(feelings_youalone)
# dramatic negative shift strong right skew, feel horrible (-30,-23) with no return apology
apology_clean |> filter(initiator_type == "conditional") |>
  plot_density_boxplot(feelings_bothyoufirst)
# significantly more positive (-8,20) with apology, though not as positive as always-type


apology_clean |> filter(initiator_type == "never") |>
  plot_density_boxplot(feelings_youalone)
# wide but negative spread (-30,-12) with no apology
apology_clean |> filter(initiator_type == "never") |>
  plot_density_boxplot(feelings_bothyoufirst)
# interestingly left skew (-8,10) but least positive out of all types









