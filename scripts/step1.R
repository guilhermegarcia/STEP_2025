# NOTE: Load packages
library(tidyverse)
library(janitor)

# NOTE: Load data
en <- read_csv("data/english-1.csv")
d <- read_csv("data/danish-1.csv")

# NOTE: PART 0: Summaries before figures
# How can we quickly inspect key information from our data using dplyr?

# QUESTION:
# 0. How many unique words are there in this lexical decision task? Are they balanced across the age groups?
# 1. What are the mean, standard deviation and standard error of RTlexdec?
# 2. How do these values vary by the age of each participant?
# 3. What word has the fastest reaction time on average? How about the slowest?
# 4. What are the top three most familiar nouns and verbs?

# ANSWERS:

en |>
  summarize(
    n = n_distinct(Word),
    .by = AgeSubject
  ) |>
  mutate(total = sum(n))

en |>
  summarize(
    Mean = mean(RTlexdec),
    SD = sd(RTlexdec),
    SE = SD / sqrt(n())
  )

en |>
  summarize(
    Mean = mean(RTlexdec),
    SD = sd(RTlexdec),
    SE = SD / sqrt(n()),
    .by = AgeSubject
  )

en |>
  summarize(
    Mean = mean(RTlexdec),
    .by = Word
  ) |>
  arrange(Mean)

en |>
  summarize(
    Mean = mean(Familiarity),
    .by = c(Word, WordCategory)
  ) |>
  arrange(desc(Mean)) |>
  slice(1:3, .by = WordCategory)


# NOTE: PART 1: Geom key geoms and the syntax of ggplot2
# Let's explore the english-1.csv with typical geoms

# EXAMPLE: X IS CATEGORICAL: geom_boxplot(), geom_violin(), stat_summary(), etc.
# we *normally* don't want geom_bar() or geom_col();

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot()

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  geom_violin(draw_quantiles = 0.5)

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  stat_summary() # NOTE: SE

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  stat_summary(fun.data = mean_sdl) # NOTE: SD

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  stat_summary(fun.data = mean_cl_normal) # NOTE: 95% CI

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  stat_summary(fun.data = mean_cl_boot) # NOTE: 95% CI (bootstrapped)

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  stat_summary(geom = "bar")

ggplot(data = en, aes(x = AgeSubject, y = RTlexdec)) +
  geom_boxplot() +
  stat_summary(color = "red")

# EXAMPLE: X IS CONTINUOUS: geom_point(), stat_smooth()

ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(alpha = 0.2)

ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  stat_smooth()

ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = "lm") # WARN: To ensure a straight line (lm)


# NOTE: Part 1.1: The notion of scope
# Compare the two figures: global vs local scope for color

ggplot(data = en, aes(x = Familiarity, y = RTlexdec, color = AgeSubject)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = "lm")

ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(alpha = 0.2) +
  stat_smooth(aes(color = AgeSubject), method = "lm")


# NOTE: Part 1.2: Going beyong two variables

ggplot(data = en, aes(x = Familiarity, y = RTlexdec, color = AgeSubject)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = "lm") +
  facet_grid(~WordCategory, labeller = "label_both")

# WARN: Notice how this is starting to become too cluttered
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(aes(colour = WrittenFrequency), alpha = 0.2) +
  stat_smooth(aes(linetype = AgeSubject), method = "lm", colour = "white") +
  facet_grid(~WordCategory, labeller = "label_both")

# NOTE: Let's combine some variables: compare OLD vs YOUNG
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(aes(colour = WrittenFrequency)) +
  stat_smooth(method = "lm", colour = "white") +
  facet_grid(AgeSubject ~ WordCategory, labeller = "label_both")

# NOTE: Let's combine some variables: compare OLD vs YOUNG
ggplot(data = en, aes(x = Familiarity, y = RTlexdec)) +
  geom_point(aes(colour = WrittenFrequency)) +
  stat_smooth(method = "lm", colour = "white") +
  facet_grid(WordCategory ~ AgeSubject, labeller = "label_both")

# HACK: The way we present our data doesn't change the data,
# but it does change people's perception of our data.

# HACK: A lot of the aesthetic issues here will be discussed later


# NOTE: Part 1.3:
# You can also create figures from piped functions (procedural):
en |>
  summarize(
    Mean = mean(RTlexdec),
    SD = sd(RTlexdec),
    .by = AgeSubject
  ) |>
  ggplot(data = _, aes(x = AgeSubject, y = Mean)) +
  geom_errorbar(aes(
    ymin = Mean - SD,
    ymax = Mean + SD
  ), width = 0.5) +
  geom_point()

# QUESTION: Download danish.csv and explore the data visually
# 1. What do you think the research question is here?
# 2. Create some summaries using dplyr verb functions.
# 3. What layers could you combine in a figure here?
# 4. How do you order categories along the x-axis based on their y values?
# 5. Can you show both the standard error AND the confidence intervals for each category?

# ====================================
# ====================================
# ====================================
