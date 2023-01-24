# ISLR Trees

library(tree)
library(ISLR2)
library(tidyverse)

attach(Carseats)
High <- factor(ifelse(Sales<=8, "No", "Yes"))

summary(Carseats)
lm.fit=lm(Sales~Advertising+Price,data=Carseats)

Carseats <- data.frame(Carseats)

Carseats %>% 
  mutate(High = High)

tree.carseats <- tree(High ~ .-Sales, Carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats
