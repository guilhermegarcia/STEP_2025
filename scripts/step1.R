# NOTE: Load packages
library(tidyverse)
library(janitor)

# NOTE: Load data (english-1 and danish-1)

# NOTE: PART 0: Summaries before figures
# How can we quickly inspect key information from our data using dplyr?

# QUESTION:
# 0. How many unique words are there in this lexical decision task? Are they balanced across the age groups?
# 1. What are the mean, standard deviation and standard error of RTlexdec?
# 2. How do these values vary by the age of each participant?
# 3. What word has the fastest reaction time on average? How about the slowest?
# 4. What are the top three most familiar nouns and verbs?

# ANSWERS:


# NOTE: PART 1: Geom key geoms and the syntax of ggplot2
# Let's explore the english-1.csv with typical geoms

# EXAMPLE: X IS CATEGORICAL: geom_boxplot(), geom_violin(), stat_summary(), etc.
# we *normally* don't want geom_bar() or geom_col();

# EXAMPLE: X IS CONTINUOUS: geom_point(), stat_smooth()

# NOTE: Part 1.1: The notion of scope
# Compare the two figures: global vs local scope for color

# NOTE: Part 1.2: Going beyong two variables


# HACK: The way we present our data doesn't change the data,
# but it does change people's perception of our data.

# HACK: A lot of the aesthetic issues here will be discussed later


# NOTE: Part 1.3:
# You can also create figures from piped functions (procedural):

# QUESTION: Download danish.csv and explore the data visually
# 1. What do you think the research question is here?
# 2. Create some summaries using dplyr verb functions.
# 3. What layers could you combine in a figure here?
# 4. How do you order categories along the x-axis based on their y values?
# 5. Can you show both the standard error AND the confidence intervals for each category?

# ====================================
# ====================================
# ====================================
