# Day_4
# Kim Scholtz
# 19 April 2018
# ANOVA

# Load data ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Importance of ANOVA (differences in means between more than two samples)
# ANOVAs require that some assumptions are met:
# Normally distributed data
# Homogeneity of variances
# Independence of data
# In our case, we will encourage also that the data are balanced


# H0: No difference in the mass of chicks
# H1: Difference in mass of chicks

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

# t-test
t.test(weight ~ Diet, data = chicks_sub)

#Welch Two Sample t-test
#data:  weight by Diet
# t = -1.2857, df = 15.325, p-value = 0.2176
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -98.09263  24.19263
# sample estimates:
# mean in group 1 mean in group 2 
# 177.75          214.70 

# Don not reject H0
# There is no significant difference


# 1-Way ANOVA -------------------------------------------------------------
# Research Question: Is there a differene in chicken mass attained after
# 21 days after the chickens have been fed four different diets?
# H0: There is no difference in chicken mass at 21 days after
# having been fed one of four diets.

chicks_21 <- chicks %>% 
  filter(Time == 21)
chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# Task: What does the outcome say about the chicken masses? Which ones are different from each other?
# Task: Devise a graphical display of this outcome.

# Notched boxplots ()
ggplot(data = chicks.aov1, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)
# Explanation: 
# Significant difference between Diet 1 and Diet 3 and Diet 1 and 4, as the notches do not overlap
# No significant difference between Diets 2, 3 and 4, as theree nothes overlap.


# Tukey HSD test ----------------------------------------------------------
TukeyHSD(aov(weight ~Diet, data = chicks_21))
# Comparing each of the Diets
# No Significant difference for Diets 2-1
# Significant difference for Diets 3-1

# Make use of the function geom segment

# Boxplots
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

# Figure out a way to illustrate the lower and upper values
# Segments showing
# Dataframe of srgments

chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))
# Or just plot confidence intervals the base R way...

plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))


# Multiple factor ANOVA ---------------------------------------------------
library(tidyverse)
# H0: There is no change in chicken mass (kg) from day 0 to day 21.
chicks_0_21 <- ChickWeight %>%  
  filter(Time %in% c(0,2, 21))

  
ggplot(data = chicks_0_21, aes(x = as.factor(Time), y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

# Run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Perform a Tukey post-hoc test
TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21)))
# Look at the confidence intervals
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))
# Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# Note the increase in the degree of freedom for the time factor
# But no increase for the d.f. for Diet

# How to look at interactions BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
# Lets look at the Tukey results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Create a line graph to help explain this concept
chicks_means <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = T))

ggplot(data = chicks_means, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)

# Non-parametric tests ----------------------------------------------------

# But what if...we dont have normal data?
# For a t-test we rather use Wilcox rank sum test
wilcox.test() # And then one fills this in the same as for t.test()

# And now for the Kruskall- Wallis
kruskal.test(weight ~ Diet, data = chicks_0_21)

library(pgirmess)  
kruskalmc(weight ~ Diet, data =chicks_0_21)


# Exercise 7.4.1 ----------------------------------------------------------

# Enter the mass at the end of the experiment
# Question: Does feed type have an effect on the mass of pigs at the end of the experiment?
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# Make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(rep("Feed 1", length(feed_1)), rep("Feed 2", length(feed_2)), rep("Feed 3", length(feed_3)), rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))
