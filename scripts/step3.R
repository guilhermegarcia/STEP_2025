library(tidyverse)
library(scales)
library(sjPlot)
library(emmeans)
library(plotly)
library(xtable)

# NOTE: Load data (danish-1, english-1, rClauseData-3)

d <- read_csv("data/danish-1.csv")
e <- read_csv("data/english-1.csv")
rc <- read_csv("data/rClauseData-3.csv")



# NOTE: Part 1: Plotting individual variation when the data is continuous
# Here's one way to combine different dimensions of the data
# What are some potential problems here...?

ggplot(data = d, aes(x = Affix |> fct_reorder(RT, .fun = mean), y = RT)) + 
  stat_summary(geom = "line", color = "black", alpha = 0.2,
               aes(group = Participant)
               # position = position_jitter(width = 0.2)
               ) +
  # stat_summary(geom = "point", color = "red", size = 4, alpha = 0.5,
  #              aes(group = Participant),
  #              position = position_jitter(width = 0.2)) +
  stat_summary() + 
  labs(title = "My plot")

# ggplotly()



# NOTE: How about categorical data?
# If the response is binary and we can create a 0/1
# column, things tend to be straightforward:

# QUESTION: How can you show by-item variation in a figure?
# What changes will be necessary given the structure of the data?

ggplot(data = d, aes(x = Affix |> fct_reorder(RT, .fun = mean), y = RT)) + 
  stat_summary(geom = "line", color = "black", alpha = 0.2,
               aes(group = Participant)
               # position = position_jitter(width = 0.2)
  ) +
  # stat_summary(geom = "point", color = "red", size = 4, alpha = 0.5,
  #              aes(group = Participant),
  #              position = position_jitter(width = 0.2)) +
  stat_summary() + 
  labs(title = "My plot")


# NOTE: Part 2: Visualizing models
# An easy way to do this is to use sjPlot and/or emmeans (multicomp)
# Suppose we want to run a model on rc

fit <- lm(RT ~ Affix, data = d)

fit |> xtable()
summary(fit)

fit |> tab_model()

fit |> 
  plot_model(show.intercept = FALSE, show.values = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  theme_classic() +
  labs(title = "", y = "My x axis")


# NOTE: How about multiple comparisons...?

# Back in the day:
aov(RT ~ Affix, data = d) |> 
  TukeyHSD() |> 
  plot()

# Now, we can use emmeans
emmeans(fit, pairwise ~ Affix)$contrasts |> plot()

mc <- emmeans(fit, pairwise ~ Affix)$contrasts |>
  as_tibble() |> 
  mutate(lower_CI = estimate - 1.96*SE,
         upper_CI = estimate + 1.96*SE)

ggplot(data = mc,
       aes(x = estimate, 
           y = contrast |> fct_reorder(estimate))) + 
  geom_errorbarh(aes(xmin = lower_CI, xmax = upper_CI),
                 height = 0, linewidth = 5, 
                 alpha = 0.3,
                 color = "blue") +
  geom_errorbarh(aes(xmin = estimate - SE, xmax = estimate + SE),
                 height = 0, linewidth = 0.5, 
                 alpha = 1,
                 color = "black") +
  geom_point(size = 3)

# NOTE: Part 2.1: Model predictions

rc <- rc |> 
  filter(Type != "Filler") |> 
  droplevels() |> 
  mutate(across(where(is_character), as_factor)) |> 
  mutate(High = if_else(Response == "High", 1, 0))

rc |> glimpse()
str(rc)

ggplot(data = rc, aes(x = Condition, y = High, color = Proficiency)) + 
  stat_summary() +
  stat_summary(geom = "line", aes(group = Proficiency), linetype = "dashed", alpha = 0.5)

fit2 <- glm(High ~ Condition * Proficiency, data = rc, family = "binomial")
summary(fit2)

plot_model(fit2, transform = NULL, type = "pred")

e



# Q: Plot individual variation for Ind and Adv only on top of existing fig.

my_lab <- rc |> 
  summarize(High = mean(High), 
            .by = c(Proficiency, Condition)) |> 
  filter(Condition == "Low")

my_lab

ggplot(data = rc, aes(x = Condition, y = High, color = Proficiency)) + 
  stat_summary(fun.data = mean_cl_boot) +
  # stat_summary(data = rc |> 
  #                filter(Proficiency != "Nat"),
  #              geom = "line", 
  #              aes(group = ID), 
  #              # linetype = "dashed", 
  #              alpha = 0.2) +
  stat_summary(geom = "line", 
               aes(group = Proficiency), 
               linetype = "solid", 
               alpha = 0.5,
               linewidth = 2) +
  geom_label(data = my_lab, 
             aes(label = Proficiency),
             fill = "black",
             color = "white",
             family = "Futura") + 
  theme_classic() + 
  theme(legend.position = "none")

rc

#

e <- e |> 
  mutate(Young = if_else(AgeSubject == "young", 1, 0))

e |> sample_n(size = 10)

ggplot(data = e, aes(x = AgeSubject, y = RTlexdec)) + 
  geom_boxplot()

ggplot(data = e, aes(x = RTlexdec, y = Young)) + 
  geom_point(alpha = 0.5, size = 3)

fit3 <- glm(Young ~ RTlexdec, data = e, family = "binomial")

summary(fit3)

plot_model(fit3, type = "pred")


preds <- predict(fit3, 
                 newdata = tibble(RTlexdec = seq(6, 7, by = 0.01)),
                 type = "response")

my_data <- tibble(
  RTlexdec = seq(6, 7, by = 0.01),
  Prob = preds
)

nrow(my_data)

ggplot(data = my_data, aes(x = RTlexdec, y = Prob)) + 
  geom_line() + 
  geom_point(shape = 21, fill = "white")

#






# NOTE: Part 3: Extras
# plotly for interactive plots
# There are numerous packages that complement ggplot2. Let's see three of them:

# NOTE: Let's take a plot from earlier (english data set):


# NOTE: Part 4: saving your plot
# Ensure that you have a hi-res figure; jpeg/png are favoured by
# most journals
