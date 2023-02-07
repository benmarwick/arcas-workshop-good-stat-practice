# How to make informative dot plots, box plots and violin plots in R
# Nico Riedel, Robert Schulz, Tracey Weissgerber
# OSF: https://osf.io/wx2dz/

library(tidyverse)

#dataset
set.seed(5)
data_1 = tibble(group = 'group a', data_col = fGarch::rsnorm(50, mean = 3.5, sd = 1, xi = 10))
data_2 = tibble(group = 'group b', data_col = rnorm(15, mean = 4.5, sd = 1.0))
data_3 = tibble(group = 'group c', data_col = rnorm(20, mean = 4.6, sd = 0.8))
data_4 = tibble(group = 'group d', data_col = rnorm(4, mean = 4, sd = 0.5))

data <- rbind(data_1, data_2, data_3, data_4)
data$group <- as_factor(data$group)

# use ggbeeswarm package for real dot plot
ggplot(data, aes(x = group, y = data_col)) +
  ggbeeswarm::geom_quasirandom()

# 2 adjust width of the dot plot
ggplot(data, aes(x = group, y = data_col)) +
  ggbeeswarm::geom_quasirandom(width = 0.3)

#  additionally make width proportional to sample size
#              to get similar density for all groups
ggplot(data, aes(x = group, y = data_col)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, varwidth = TRUE)

# add semi-transparency to points to make all points visible
ggplot(data, aes(x = group, y = data_col)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, varwidth = TRUE, alpha = 0.2)


# additional emphasis on median
ggplot(data, aes(x = group, y = data_col)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, varwidth = TRUE, alpha = 0.2, size = 1.8) +
  stat_summary(fun = median, 
               fun.min = median, 
               fun.max = median, 
               geom = "crossbar",
               width = 0.2, 
               size = 1.5, 
               fatten = 1, 
               color = "black")

# many other steps, Add ticks on y axis
ggplot(data, aes(x = group, y = data_col)) +
  ggbeeswarm::geom_quasirandom(width = 0.3, varwidth = TRUE, alpha = 0.2, size = 1.8) +
  stat_summary(fun = median, fun.min = median, fun.max = median, geom = "crossbar",
               width = 0.2, size = 1.5, fatten = 1, color = "black") +
  scale_x_discrete(name = NULL, labels = c("a", "b", "c", "d")) +
  scale_y_continuous(name = "Biomarker 1 [ng/ml]", limits = c(0, 7), breaks = seq(0, 7))

# box plot evolution --------------------------------------

#1. Box plot only
ggplot(data, aes(x = group, y = data_col)) +
  geom_boxplot() +
  theme_minimal(base_size = 12)

#2. Box plot with unjittered dot plot
ggplot(data, aes(x = group, y = data_col)) +
  geom_boxplot() +
  geom_point()  +
  theme_minimal(base_size = 12)

#2.2 add boxplot
ggplot(data, aes(x = group, y = data_col)) +
  geom_boxplot() +
  ggbeeswarm::geom_quasirandom() +
  theme_minimal(base_size = 12)

#2.6 remove duplicate outliers
ggplot(data, aes(x = group, y = data_col)) +
  geom_boxplot(varwidth = TRUE, 
               outlier.shape = NA) +
  ggbeeswarm::geom_quasirandom(alpha = 0.2, size = 3) +
  theme_minimal(base_size = 12)










