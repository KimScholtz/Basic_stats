
# Day 3
# Kim Scholtz
# 17 April 2018
# Distributions



# Generate a Cullen and Frey graph


# Load libraries ----------------------------------------------------------

library(fitdistrplus)
library(logspline)

# Generate log-normal data

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm)
descdist(r_norm)
# running a fit distribution

hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# uniform data
y <- runif(100)
par(mflow = c(1, 1))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

# Chapter 6
# t-test: if you compare 2 things
# ANOVA: more than 2 things

# Load libraries ----------------------------------------------------------------
library(tidyverse)
library(plotly)

# Random normal data
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Check assumptions -------------------------------------------------------

# Normality
# For this we may use the Shapiro-Wilk test
shapiro.test(r_dat$dat)
shapiro.test(r_dat$dat)[1]
shapiro.test(r_dat$dat)[2]


# But that is testing all of the data together
# We must be abit more clever about how we make this test

r_dat %>% 
group_by(sample) %>%
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
# Remember the data are normal when p>= 0.05
# The data are non-normal when p <= 0.05


# Check homoscedasticity --------------------------------------------------

# There are many ways to check for homoscedasticity
# Which is the similarity of variance between sample sets
# for now we will simply say that this assumption is met
# the variance of the samples are not more than 2-4 times greater
# then one another

# check everything at once
# WRONG

# Check variance for entire dataset
var(r_dat$dat)

# or do it the tidy
r_dat %>% 
  group_by(sample) %>%
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))

# The observations in the groups being compared are independent of each other



# A one sample t-test -----------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# Visualisation showing the density plot
ggplot(data = r_one, aes(x = dat)) +
  geom_density(aes(fill = sample)) +
  labs(x = "Data", y = "Count")

# Run the test
t.test(r_one$dat, mu = 20)


# Run a test we know will produce a significant result --------------------
t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------
# Are these data SMALLER/LESS than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")
# or Greater
t.test(r_one$dat, mu = 20, alternative = "greater")

# But what about for the larger population mean?
# Are the samples less than the population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
# What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")



# Two sample t-tests ------------------------------------------------------

# Create a dataframe ------------------------------------------------------

r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# Run a default/basic test ------------------------------------------------

t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# Pick a side -------------------------------------------------------------
# Is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
# Is A greater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")


# Working on Ecklonia exercise

# Question
# H0:Epiphyte length at Batsata is not greater than at Boulders Beach.
# H1:Epiphyte length at Batsata is greater than at Boulders Beach.


ecklonia <- read_csv("ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()
  
# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "epiphyte_length")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "epiphyte_length (m)", x = "")
library(ggpubr)
library(ggplot)
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")


# Exercise 1

pH_new <- read_csv("pH.new.csv")
graph1 <- ggplot(data = pH_new, aes(x = pH, fill = Site)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = Site), colour = NA, alpha = 0.4) +
  labs(x = "value") 
graph1
t.test(pH ~ Site, data = pH_new, var.equal = TRUE)


#Two Sample t-test

#data:  pH by Site
#t = 0.74708, df = 18, p-value = 0.4647
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.4479361  0.9422951
#sample estimates:
#  mean in group Lower mean in group Middle 
#6.317179             6.070000 

# From the t-test we see that there is a significant difference