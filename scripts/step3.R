library(tidyverse)
library(scales)
library(sjPlot)
library(emmeans)

# NOTE: Load data (danish-1, english-1, rClauseData-3)

# NOTE: Part 1: Plotting individual variation when the data is continuous
# Here's one way to combine different dimensions of the data
# What are some potential problems here...?

# NOTE: How about categorical data?
# If the response is binary and we can create a 0/1
# column, things tend to be straightforward:

# QUESTION: How can you show by-item variation in a figure?
# What changes will be necessary given the structure of the data?

# NOTE: Part 2: Visualizing models
# An easy way to do this is to use sjPlot and/or emmeans (multicomp)
# Suppose we want to run a model on rc


# NOTE: How about multiple comparisons...?

# NOTE: Part 2.1: Model predictions

# NOTE: Part 3: Extras
# plotly for interactive plots
# There are numerous packages that complement ggplot2. Let's see three of them:

# NOTE: Let's take a plot from earlier (english data set):


# NOTE: Part 4: saving your plot
# Ensure that you have a hi-res figure; jpeg/png are favoured by
# most journals
