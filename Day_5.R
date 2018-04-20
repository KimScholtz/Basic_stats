# Day 5
# Continuing with ANOVA
#20 April 2018
# Kimmy

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(Rmisc)
library(ggpubr)
# Load data ---------------------------------------------------------------

snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))

snakes$day <- as.factor(snakes$day)
# Assumption that gets violated: not independent
# because the same snakes are used  

# Summarise the data -----------------------------------------------------
snakes_summary <- snakes %>% 
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))
# HO: There is no difference in the number of openings from day to day
# H1: There is a difference in the number of openings from day to day


# Test a hypothesis -------------------------------------------------------

# First calculate SE and CI
snakes.summary2 <- summarySE(data = snakes,
                               measurevar = "openings",
                               groupvars = c("day"))


# Then visualise the data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2,
               aes(x = day, xend = day, y = openings - ci,
                   yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# What are our null hypotheses?
# H0: There is no difference between snakes with respect to the number 
# of openings at which they habituate
# H0: There is no difference between days in terms of the number of openings 
# at which the snakes habituate

# Analysis of variance of opening and closing as a funvtion of day

snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)
# Reject the null hypothesis because ther is a significant difference between 
# the number of openings at which the snakes habituate

# Test both hypotheses
snake.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snake.all.aov)


# Testing assumptions afterwards ------------------------------------------

# First visualise normality of results
snakes.residuals <- residuals(snake.all.aov)
hist(snakes.residuals)

# Then visualise homoscedascity of results
plot(fitted(snake.all.aov), residuals(snake.all.aov))

# Check Tukey results
snakes.tukey <- TukeyHSD(snake.all.aov, which = "day")
plot(snakes.tukey)

# Visualise the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)



# Exercise using moth data ------------------------------------------------

# Load data 
library(tidyverse)
library(ggpubr)
library(corrplot)
library(ggplot2)
moths <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", - Location)

# Summarise the data ------------------------------------------------------

moth_loc_summary <- moths %>%
  group_by(Location) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

moth_trap_summary <- moths %>%
  group_by(trap) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

# Formulate the hypotheses --------------------------------------------------

# HO: There is no difference in the count depending of different locations
# HO: There is no difference in count depending on the different trap types

#  Calculate SE & CI ------------------------------------------------------

moth_loc_summary_2 <- summarySE(data = moths,
                                measurevar = "count",
                                groupvars = c("Location"))

moth_trap_summary_2 <- summarySE(data = moths,
                                 measurevar = "count",
                                 groupvars = c("trap"))

# Visualise the data ------------------------------------------------------

Location <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moth_loc_summary_2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Trap <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moth_trap_summary_2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Final <- ggarrange(Location, Trap,
                   ncol = 2, nrow = 1,
                   labels = c("Location", "Trap"),
                   common.legend = TRUE)

# Test the hypothesis -----------------------------------------------------

moth.loc.aov <- aov(count ~ Location, data = moths)
summary(moth.loc.aov)

moth.trap.aov <- aov(count ~ trap, data = moths)
summary(moth.trap.aov)

# Test both hypotheses

moths.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moths.all.aov)

# Testing assumptions 

# First visualise normality of data

moths.residuals <- residuals(moths.all.aov)
hist(moths.residuals)

# Visualise homoscedasticity

plot(fitted(moths.all.aov), residuals(moths.all.aov))

# Apply tukey test 

moths.loc.tukey <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.loc.tukey)

moths.trap.tukey <- TukeyHSD(moths.all.aov, which = "trap")
plot(moths.trap.tukey)

# Visualise the factor interaction



# Regressions -------------------------------------------------------------

# For the explanation of this statistical analysis
# we are going to use eruption data from 01' Faithful

head(faithful)
# plot a quick scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "red")


# Form a hypothesis -------------------------------------------------------
# H0: Waiting time does not influence the duration of an eruption
# H1: Waiting time does influence the duration of an eruption


# Test a hypothesis -------------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)
slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

# Load data
ecklonia <- read_csv("ecklonia.csv")


# Formulate a hypothesis --------------------------------------------------

# H0:There is no relationship between stipe length and stipe mass
# for Ecklonia maxima
# H0:There is a relationship between stipe length and stipe mass
# for Ecklonia maxima


# Test a hypothesis -------------------------------------------------------

cor.test(ecklonia$stipe_diameter, ecklonia$frond_length)
ggplot(data = ecklonia, aes(x = stipe_diameter, y = frond_length)) +
  geom_point()


# Run hecka tests at once -------------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub) 
  ecklonia_cor


# Spearman rank test (ordinal) ------------------------------------------------------

 ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3)) 

# Then run a Spearmen test
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")


# Kendall rank correlation (not ordinal) ------------------------------------------------
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")


# Visualise all the things ------------------------------------------------
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson
corrplot(ecklonia_pearson, method = "circle")

# End Day 5
