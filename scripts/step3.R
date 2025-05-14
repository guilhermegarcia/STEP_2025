library(tidyverse)
library(scales)
library(sjPlot)
library(emmeans)

d <- read_csv("data/danish-1.csv")
en <- read_csv("data/english-1.csv")
rc <- read_csv("data/rClauseData-3.csv") |>
  filter(Type != "Filler")

rc
d

# NOTE: Part 1: Plotting individual variation when the data is continuous
# Here's one way to combine different dimensions of the data
# What are some potential problems here...?
ggplot(data = d, aes(x = Affix |> fct_reorder(RT, .fun = mean), y = RT)) +
  # geom_boxplot(alpha = 0.3, color = "gray") +
  stat_summary(
    fun.data = mean_sdl,
    linewidth = 5,
    alpha = 0.2,
    color = "slateblue"
  ) +
  stat_summary() +
  stat_summary(aes(group = Participant),
    geom = "line",
    alpha = 0.1,
    linewidth = 1
  ) +
  theme_classic(base_family = "Futura") +
  labs(
    x = "Affix",
    y = "Reaction time (ms)"
  )

# WARN: Here are some issues:
# 1. the figure may be too cluttered
# 2. reviewers and readers may think there's too much variation now that they *see* it
# 3. more coding/data transformation depending on the data structure

# NOTE: How about categorical data?
# If the response is binary and we can create a 0/1
# column, things tend to be straightforward:

rc # Let's create a binary Low response col:

rc <- rc |>
  mutate(Low = if_else(Response == "Low", 1, 0))

ggplot(data = rc, aes(x = Condition, y = Low)) +
  stat_summary(aes(group = ID),
    geom = "line",
    alpha = 0.05,
    linewidth = 1
  ) +
  stat_summary() +
  theme_classic(base_family = "Futura") +
  # WARN: These are percentages *derived* from 0/1s
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(limits = c("NoBreak", "High", "Low")) +
  labs(
    x = "Condition",
    y = "% of Low responses"
  )

# QUESTION: How can you show by-item variation in a figure?
# What changes will be necessary given the structure of the data?

# ANSWER: We can no longer use Condition, of course; but we can use Proficiency, for example:
ggplot(data = rc, aes(x = Proficiency, y = Low)) +
  stat_summary(aes(group = Item),
    geom = "line",
    alpha = 0.05,
    linewidth = 1
  ) +
  stat_summary() +
  theme_classic(base_family = "Futura") +
  # WARN: These are percentages *derived* from 0/1s
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(limits = c("Int", "Adv", "Nat")) +
  labs(
    x = "Proficiency",
    y = "% of Low responses"
  )

# NOTE: Part 2: Visualizing models
# An easy way to do this is to use sjPlot and/or emmeans (multicomp)
# Suppose we want to run a model on rc

fit <- glm(Low ~ Condition + Proficiency, data = rc, family = binomial())

summary(fit)

# HACK: Easiest way to plot model's estimates: plot_model(), which uses ggplot2
plot_model(fit,
  transform = NULL,
  show.intercept = TRUE
) +
  # NOTE: You can then adjust the plot as usual:
  theme_classic(base_family = "Futura") +
  labs(title = NULL) +
  # NOTE: E.g., let's add a dashed line for zero effect (notice the flipped coord):
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


# NOTE: How about multiple comparisons...?
emmeans(fit, pairwise ~ Proficiency)$contrasts |>
  plot() # Again, this is just a typical ggplot2 object



# NOTE: Part 3: Extras
# plotly for interactive plots
# There are numerous packages that complement ggplot2. Let's see three of them:

library(plotly) # HACK: For interactive plots
library(patchwork) # HACK: For combining plots
library(ggdist) # HACK: To plot distributions

# NOTE: Let's take a plot from earlier (english data set):

ggplot(data = en, aes(
  x = Familiarity, y = RTlexdec,
  group = Word, color = AgeSubject
)) +
  geom_point(size = 5, alpha = 0.3) +
  theme_classic(base_family = "Futura") +
  # WARN: These are percentages *derived* from 0/1s
  labs(
    x = "Familiarity",
    y = "Reaction time (log)"
  ) +
  scale_color_manual(values = c("darkorange2", "slateblue"))

ggplotly()

# WARN: While it's super easy to plot using ggplotly(),
# you'd have more control over the figure plotting directly with plotly
# but that is less familiar if you're coming from ggplot2

# NOTE: Patchwork to combine figures
# Suppose we have two plots:

fig1 <- ggplot(data = rc, aes(x = Hours, y = RT, group = ID)) +
  geom_point() +
  theme_classic(base_family = "Futura") +
  # WARN: These are percentages *derived* from 0/1s
  labs(
    x = "Hours of study per week",
    y = "Reaction time (ms)"
  )

fig2 <- plot_model(fit,
  transform = NULL,
  show.intercept = TRUE
) +
  # NOTE: You can then adjust the plot as usual:
  theme_classic(base_family = "Futura") +
  labs(title = NULL) +
  # NOTE: E.g., let's add a dashed line for zero effect (notice the flipped coord):
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

# HACK: patchwork makes it very easy to combine figures:
fig1 / fig2
fig1 + fig2

# NOTE: ggdist: let's go back to our stat_summary() above
ggplot(data = d, aes(x = Affix |> fct_reorder(RT, .fun = mean), y = RT)) +
  # geom_boxplot(alpha = 0.3, color = "gray") +
  # stat_summary() +
  stat_halfeye(.width = c(0.5, 0.7)) +
  theme_classic(base_family = "Futura") +
  labs(
    x = "Affix",
    y = "Reaction time (ms)"
  )

# NOTE: Part 4: saving your plot
# Ensure that you have a hi-res figure; jpeg/png are favoured by
# most journals

ggsave(file = "~/Desktop/my_plot.jpeg", dpi = 500, width = 6, height = 3)
ggsave(file = "~/Desktop/my_plot.png", dpi = 500, width = 6, height = 3)
