library(nycflights13)
library(tidyverse)

jan1 <- filter(flights, month == 1, day == 1)

dec25 <- filter(flights, month == 12, day == 25)

LGA <- filter(flights, origin == "LGA")

dec25lga <- filter(flights, month == 12, day == 25, origin == "LGA")
dec25not_lga_not_Dec25 <- filter(flights, month != 12 & day != 25, origin != "LGA")

flights %>% select(-(year:day))
select(flights, starts_with("time"))

transmute(flights, speed = distance/air_time*60, gain = dep_delay - arr_delay)

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocks %>% gather( "stock", "price", -time)


flights %>% transmute(dep_time, hours = dep_time %/% 100, minutes = dep_time %% 100)

View(flights %>% mutate(hours = air_time / 60, gain = dep_delay - arr_delay, speed = distance / air_time * 60))

flights %>% select(contains("dep"))

library("forcats")
f <- factor(c("a", "c", "b", "a"))