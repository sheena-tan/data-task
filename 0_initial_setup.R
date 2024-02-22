#### Initial Setup ----
#### data cleaning, eda


library(tidyverse)
library(here)
library(patchwork)
library(ggdist)

# load data ----
apology_data <- read_csv(here("data/apology_data.csv"))

skimr::skim(apology_data)
# consent: n_unique = 1; all responses agreed
# passedattn: n_unique = 2; need to clean responses that did not pass
# Progress: mean = 84.1; need to clean responses that did not answer about feelings



## data cleaning ----
apology_clean <- apology_data |>
  filter(ResponseId != "R_bBJ75JWs3W8KSgF") |> #see comments field
  filter(Progress > 30) |>
  filter(passedattn == "yes") |>
  select(ResponseId, initiator_type, starts_with("feelings_"), starts_with("outcome_"),
         blame_1, real_imaginary, describe, comments)

# transform data for pairwise t-test
apology_long_q1 <- apology_clean |>
  select(ResponseId, initiator_type, feelings_youalone, feelings_bothyoufirst) |>
  rename(no = feelings_youalone, yes = feelings_bothyoufirst) |>
  pivot_longer(cols = c("no", "yes"), names_to = "apology_returned") |>
  arrange(apology_returned, ResponseId)

apology_long_q3 <- apology_clean |>
  select(ResponseId, feelings_bothyoufirst, feelings_youaloneforgiven) |>
  rename(no = feelings_youaloneforgiven, yes = feelings_bothyoufirst) |>
  pivot_longer(cols = c("no", "yes"), names_to = "apology_returned") |>
  arrange(apology_returned, ResponseId)

# transform data for one-way ANOVA
apology_wide <- apology_clean |>
  select(ResponseId, initiator_type, feelings_youalone, feelings_bothyoufirst) |>
  mutate(
    .res = feelings_bothyoufirst - feelings_youalone,
    initiator_type = as.factor(initiator_type)
  )

# write out data
write_rds(apology_clean, here("data/apology_clean.rds"))
write_rds(apology_long_q1, here("data/apology_long_q1.rds"))
write_rds(apology_long_q3, here("data/apology_long_q3.rds"))
write_rds(apology_wide, here("data/apology_wide.rds"))



## initial eda ----
plot_density_boxplot <- function(data, x_variable) {
  p1 <- ggplot(data, aes({{x_variable}})) +
    geom_boxplot() +
    xlim(-30, 30) +
    theme_void()

  p2 <- ggplot(data, aes({{x_variable}})) +
    geom_density() +
    xlim(-30, 30) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  p1/p2 + plot_layout(heights = unit(c(1, 5), c("cm", "null")))
}

alone_total <- plot_density_boxplot(apology_clean, feelings_youalone)
# overall, people feel very bad (-30,-10) when they do not get a return apology
both_total <- plot_density_boxplot(apology_clean, feelings_bothyoufirst)
# overall, people feel much better (0,20) when they do get a return apology

alone_always <- apology_clean |> filter(initiator_type == "always") |>
  plot_density_boxplot(feelings_youalone)
# always-type do not feel good (-20,-10) with no return apology, but better than the population average
both_always <- apology_clean |> filter(initiator_type == "always") |>
  plot_density_boxplot(feelings_bothyoufirst)
# always-type feel much better (10,20) when both people apologize

alone_cond <- apology_clean |> filter(initiator_type == "conditional") |>
  plot_density_boxplot(feelings_youalone)
# dramatic negative shift strong right skew, feel horrible (-30,-23) with no return apology
both_cond <- apology_clean |> filter(initiator_type == "conditional") |>
  plot_density_boxplot(feelings_bothyoufirst)
# significantly more positive (-8,20) with apology, though not as positive as always-type

alone_never <- apology_clean |> filter(initiator_type == "never") |>
  plot_density_boxplot(feelings_youalone)
# wide but negative spread (-30,-12) with no apology
both_never <- apology_clean |> filter(initiator_type == "never") |>
  plot_density_boxplot(feelings_bothyoufirst)
# interestingly left skew (-8,10) but least positive out of all types


## task 1 question 2 eda ----
residual_boxplot <- apology_wide |>
  ggplot(aes(initiator_type, .res)) +
  geom_boxplot() +
  labs(
    x = "initiator type",
    y = "residual"
  ) +
  theme_ggdist()


# write plots ----
ggsave(here("results/alone_total.png"), alone_total)
ggsave(here("results/both_total.png"), both_total)
ggsave(here("results/alone_always.png"), alone_always)
ggsave(here("results/both_always.png"), both_always)
ggsave(here("results/alone_cond.png"), alone_cond)
ggsave(here("results/both_cond.png"), both_cond)
ggsave(here("results/alone_never.png"), alone_never)
ggsave(here("results/both_never.png"), both_never)
ggsave(here("results/residual_boxplot.png"), residual_boxplot)








