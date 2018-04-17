# Day_2
# 13 April 2018
# Data visualisation and distribution

# Loading libraries -------------------------------------------------------
library(tidyverse)

# How to calculate:
# mean
# median
# sd
# variation

# Manual Calculations -----------------------------------------------------
# Dataframe making

# Mean
#Random data: rnorm()
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), sample = "A")

# Visualisation
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()


# The mean
# Sum of all the values
# Divide by
# Number of all the points

r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))


r_dat$dat[(length(r_dat$dat)+1)/2]


r_dat %>% 
  summarise(r_median = median(dat))

r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

# Variance
# The sum of
# Each value minus the mean
# Squared

# Divide by the count of samples minus one
# mutate (create a new column)

r_dat %>%
  mutate(r_error = dat - mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/ (n()-1),
            r_var_func = var(dat))

# Standard deviation

r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

# Exercise


summary(ChickWeight$weight)
chick <- ChickWeight
chick %>%
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))

# Visualisation -----------------------------------------------------------
# Load libraries
# These few packages contain most functions necessary
# To make publication ready figures
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# Qualitative -------------------------------------------------------------

# Stacked bar graph
# Create the count of qualitative data
iris_count <- iris %>%
  count(Species) %>% 
  mutate(prop = n/sum(n))

# Load SA time data
# Use tab in the " after read.csv
sa_time <- read.csv("SA_time.csv")

# Edit time
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2)))

sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human, -geo)

sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))

ggplot(data = sa_count, aes(x = "", y = n, fill= time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumalitive sum",
       x = NULL, y = "Count") +
  theme_minimal()


# Making a pie chart

ggplot(data = sa_count, aes(x = "", y = n, fill= time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "salmon") #### Using a different palette

# Histogram
ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

# Get rid of the one value
sa_clean <- sa_long %>% 
  filter(minutes < 300)

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Realtive Proportion Histogram
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplot
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

# Box plot
# Notched boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)



# Calculate summary stats for plotting over the box plots

sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# Plot these
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats,  size = 6, shape = 18,
             aes(y = time_type_mean, colour = "goldenrod"))


# Relationships -----------------------------------------------------------
# Basic scatter plot

sa_time_clean <- sa_time %>% 
  filter(just_now < 300)

ggplot(data = sa_time_clean, aes(y = now_now, x = just_now)) +
  geom_point()

# Limit the axis
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0,60))

# Areas and trendline
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0,60))


