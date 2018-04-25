# Exercise 1

# Load data
library(tidyverse)

# Question: Does feed type have an effect on the mass of pigs at the end of the experiment?
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# H0: Feed type has no effect on the mass of pigs at the end of the experiment.
# H1: Feed type does have an effect on the mass of pigs at the end of the experiment.

# Make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(rep("Feed 1", length(feed_1)), rep("Feed 2", length(feed_2)), rep("Feed 3", length(feed_3)), rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))
bacon1 <- as_tibble(bacon)
bacon_1 <- aov(mass ~ feed, data =bacon1)
summary(bacon_1)

ggplot(data = bacon_1, aes(x = feed, y = mass)) +
  geom_boxplot(aes(fill = feed), notch = T)

# Exercise 2

tande <- datasets::ToothGrowth
tande.aov <- aov(len~supp, data = tande)
summary(tande.aov)

# H0: The use of supplements does not have an eefect on the quality of the teeth.
# H1: The use of supplements does have an eefect on the quality of the teeth.


# Exercise 3

iris <- iris
iris.aov <- aov(data= iris, Petal.Length~Species)
summary(iris.aov)

# H0:There is no difference between the species and petal lengths.
# H1:There is a difference between the species and petal lengths.