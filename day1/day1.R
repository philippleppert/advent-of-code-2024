library(tidyverse)

day1 <- read_delim("day1/day1.txt", col_names = F)

# Part 1
day1_mod <-
  tibble(
    left = sort(day1$X1),
    right = sort(day1$X4)
    ) %>%
  mutate(dist = right - left)

sum(abs(day1_mod$dist))

# Part 2
day1_mod2 <- 
  day1 %>%
  mutate(
    appearance_times = map_int(X1, ~sum(.x ==  X4)),
    sim_score = X1 * appearance_times
    )

sum(day1_mod2$sim_score)

