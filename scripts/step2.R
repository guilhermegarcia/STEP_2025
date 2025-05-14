library(tidyverse)
library(scales)
library(janitor)

d <- read_csv("data/danish-1.csv")
q <- read_csv2("data/q-1.csv")
#
# NOTE: Part 1: Data cleaning + key tidyverse functions (review)
# mutate, filter, select, if_else, summarize, rename, arrange, etc.
# Real scenario: you have some chaotic data to analyse
# First, you need to "clean" it; only then can you truly work with it
# So before discussing figures, we need to make sure we know how
# to prepare the data, i.e., how to clean it.

q
glimpse(q)

# QUESTION: Does the file follow the tidy format? Is it long or wide?
names(q)
# NOTE: First step: print names of cols

# PROBLEM: Spaces in col names; upper- and lower-case cols
# That's why we use janitor, for example
q <- clean_names(q)
names(q)

# PROBLEM: We want to get rid of useless cols
q <- q |>
  select(-matches("^(heure|adresse|nom|grade|feedback|points|total|quiz)"))

glimpse(q)

# PROBLEM: Table is not tidy; wide-to-long transformation to create tidy table:
long <- q |>
  pivot_longer(
    names_to = "question",
    values_to = "response",
    cols = 7:16
  )

names(long)

# NOTE: Good idea: simplify/translate col names?
long <- long |>
  rename(
    "name" = 2,
    "data_interest" = 3,
    "coding_interest" = 4,
    "exp_data" = 5,
    "exp_r" = 6,
    "difficulty" = 7
  )


# WARN: Is the col id correctly classified by R?
# How about the _interest cols? They're ordered factors!
long <- long |>
  mutate(
    id = as_factor(id),
    across(4:7, ~ factor(.x, ordered = TRUE))
  )

long

# NOTE: We have some ordered factors. Let's create some
# dichotomous versions for some of these variables.
# This will be easier to plot. For all variables in question, if we get 4s or 5s, let's write "yes". Else, "no".

long <- long |>
  mutate(across(4:7, ~ if_else(.x %in% c("4", "5"), "yes", "no"), .names = "bin_{.col}"))

long |> glimpse()
# =============================

# NOTE: How do we evaluate if answers are correct? We need a key:
responses <- c(
  "x == \"hello\"",
  "mutate()",
  "select()",
  "filter()",
  "a numeric vector",
  "geom_point()",
  "the median",
  "means and standard errors",
  "the resulting data object is not tidy",
  "it specifies the mappings between variables and visual properties"
)


# NOTE: Next, we glue the responses to the main table:
long <- long |>
  mutate(correction = responses, .by = id)

long


# NOTE: Now we can evaluate the answers:
long <- long |>
  mutate(correct_answer = if_else(str_to_lower(response) == correction, 1, 0))

glimpse(long)

# NOTE: Correction col is no longer needed
long <- long |>
  select(-correction)

# NOTE: We're finally ready to create figures and run our analysis. If you want, make all chr into fct now:
long <- long |>
  mutate(across(where(is_character), as_factor))

long

# NOTE: Part 1.1: Practice

# QUESTION:
# 1. What are some obvious figures for these data?
# 2. Can you combine multiple variables in the same figure? What variables would you combine?
# 3. What relationships do you observe between the variables plotted?

ggplot(data = long, aes(x = bin_exp_data, y = correct_answer)) +
  stat_summary()

ggplot(data = long, aes(x = bin_exp_data, y = correct_answer)) +
  stat_summary(geom = "bar", alpha = 0.2, color = "black") +
  stat_summary() +
  facet_grid(~difficulty)

ggplot(data = long, aes(x = difficulty, y = correct_answer)) +
  stat_summary(aes(fill = bin_exp_data), geom = "bar", alpha = 0.2, color = "black") +
  stat_summary()

# NOTE: Part 2: Aesthetics
# Let's use both our survey data and danish-1.csv here.

# NOTE: Percentages, colours, font, labels, themes

ggplot(data = long, aes(x = difficulty, y = correct_answer)) +
  stat_summary(aes(fill = bin_exp_data), geom = "bar", alpha = 0.2, color = "black") +
  stat_summary() +
  # NOTE: Percentages
  scale_y_continuous(labels = percent_format()) +
  # NOTE: Labels
  labs(
    x = "Difficulty",
    y = "Accuracy",
    fill = "Experience with data analysis?"
  ) +
  # NOTE: Adjust theme + font family
  theme_classic(base_family = "Futura") +
  # NOTE: Position of legend
  theme(legend.position = "top") +
  # Change colours
  scale_fill_manual(values = c("slateblue", "darkorange2")) +
  coord_flip()

# NOTE: Removing legend and using title instead

colours <- c("darkorange2", "slateblue")
names(colours) <- c("no", "yes")

customTitle <- glue::glue('Accuracy for people
<span style="color:{colours["yes"]}">**with**</span> and
<span style="color:{colours["no"]}">**without**</span> experience in data analysis')

ggplot(data = long, aes(x = difficulty, y = correct_answer)) +
  stat_summary(aes(fill = bin_exp_data), geom = "bar", alpha = 0.2, color = "black") +
  stat_summary() +
  # NOTE: Percentages
  scale_y_continuous(labels = percent_format()) +
  # NOTE: Labels
  labs(
    x = "Difficulty",
    y = "Accuracy",
    fill = "Experience with data analysis?",
    title = customTitle
  ) +
  # NOTE: Adjust theme + font family
  theme_classic(base_family = "Futura") +
  # NOTE: Position of legend
  theme(
    legend.position = "none",
    plot.title = ggtext::element_markdown()
  ) +
  # Change colours
  scale_fill_manual(values = c("slateblue", "darkorange2")) +
  coord_flip()

# HACK: Another option is to use the marquee package; similar idea


# QUESTION: Go back to the Danish data set
# and create a figure with customized aesthetics.
# You decide what aspects of the figure you'd like to alter.

# ANSWER: Here's one option
ggplot(data = d, aes(
  x = Affix |> fct_reorder(RT, .fun = mean),
  y = RT
)) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar", linewidth = 3,
    alpha = 0.2,
    width = 0,
    color = "slateblue"
  ) +
  stat_summary() +
  theme_classic(base_family = "Futura") +
  theme_classic() +
  labs(
    y = "Reaction time",
    x = "Affix in study",
    title = "Reaction times by affix"
  ) +
  scale_y_continuous(labels = unit_format(suffix = "ms"))

# NOTE: Now using labels to avoid legends
# Suppose we have five participants and two suffixes only

d_sub <- d |>
  filter(
    Participant %in% c(
      "Part_5", "Part_15", "Part_8"
    ),
    Affix %in% c(
      "bar", "ere"
    )
  )

d_sub


# NOTE: Now let's create a figure comparing the three participants
ggplot(data = d_sub, aes(
  x = fct_reorder(Affix, RT),
  y = RT,
  color = Participant
)) +
  stat_summary(geom = "line", aes(group = Affix)) +
  stat_summary()

# NOTE: Not bad, but we can improve this *a lot*

# NOTE: First, let's get rid of the legend and mode
# the info on participants inside. We will use geom_label
# but we have to "summarize" the info to make it work:

ggplot(data = d_sub, aes(
  x = fct_reorder(Affix, RT),
  y = RT,
  color = Participant
)) +
  stat_summary(geom = "line", aes(group = Affix)) +
  stat_summary() +
  geom_label(aes(label = Participant))

# PROBLEM: Do you see the issue here?

ggplot(data = d_sub, aes(
  x = fct_reorder(Affix, RT),
  y = RT,
  color = Participant
)) +
  # WARN: We normally don't do lines with cat x, but...
  stat_summary(
    geom = "line", aes(group = Participant),
    linetype = "dashed"
  ) +
  stat_summary() +
  geom_label(
    data = d_sub |> dplyr::filter(
      Affix == "bar"
    ) |>
      summarize(RT = mean(RT), .by = c(Affix, Participant)),
    aes(x = Affix, y = RT, label = Participant),
    fontface = "bold",
    family = "Comic Sans MS",
    # NOTE: Perhaps you don't want to omit errors?
    position = position_nudge(x = 0.2)
  ) +
  theme_classic(base_family = "Comic Sans MS") +
  theme(legend.position = "none") +
  scale_color_manual(values = c("slateblue", "darkorange2", "black")) +
  scale_y_continuous(labels = unit_format(suffix = "ms")) +
  labs(
    x = "Affix",
    y = "Reaction time",
    title = "Using labels instead of legends",
    subtitle = "PS. Never use Comic Sans MS in a figure!"
  )


# NOTE: Part 3: Ordinal data -> back to our survey
# Plotting ordinal data can be tricky; here's why

# WARN: Option 1: treat scale as continuous
ggplot(data = long, aes(x = bin_exp_data, y = difficulty |> as.integer())) +
  # WARN: We may want to bootstrap CIs, since this is likely not normally distributed
  stat_summary(fun.data = mean_cl_boot)

# NOTE: How normal is our scalar variable...?
ggplot(data = long, aes(x = difficulty |> as.integer())) +
  geom_histogram()

# NOTE: This can certainly work, depending on how continuous
# the data is... but let's look at a different way

# HACK: First, let's create our data:
props <- long |>
  summarize(n = n(), .by = c(difficulty, bin_exp_data)) |>
  # NOTE: This will add up to 100% by category of experience with data
  mutate(Prop = n / sum(n), .by = bin_exp_data)

props

# HACK: Now, we start our figure:
ggplot(data = props, aes(x = bin_exp_data, y = Prop)) +
  geom_col(aes(fill = difficulty))

our_plot

# HACK: Not bad, but let's improve the colors
ggplot(data = props, aes(x = bin_exp_data, y = Prop)) +
  geom_col(aes(fill = difficulty), color = "black") +
  scale_fill_brewer(palette = "OrRd")

# HACK: Now, we add % to the bars and adjust the aesthetics
ggplot(data = props, aes(x = bin_exp_data, y = Prop)) +
  geom_col(aes(fill = difficulty),
    color = "steelblue", alpha = 0.8,
    width = 0.5
  ) +
  scale_fill_brewer(palette = "Blues") + # Also try Greys
  geom_label(
    aes(label = (Prop * 100) |>
      round(2) |>
      str_c("%")),
    position = position_stack(vjust = 0.5),
    family = "Futura"
  ) +
  theme_classic(base_family = "Futura") +
  labs(
    x = "Experience with data?",
    y = NULL,
    fill = "Difficulty:"
  ) +
  # NOTE: This will make sure higher difficulties are to the right
  scale_y_continuous(labels = percent_format()) +
  # coord_flip() +
  theme(
    legend.position = "top"
  )

# NOTE: Another option (more minimalist); see here: https://gdgarcia.ca/posts/2023-10-23-scalar-data/index.html
ggplot(data = props, aes(x = bin_exp_data, y = Prop)) +
  geom_col(aes(fill = difficulty),
    color = "steelblue", alpha = 0.8,
    width = 0.5
  ) +
  scale_fill_brewer(palette = "Blues") + # Also try Greys
  geom_label(
    aes(label = (Prop * 100) |>
      round(2) |>
      str_c("%")),
    position = position_stack(vjust = 0.5),
    family = "Futura"
  ) +
  theme_classic(base_family = "Futura") +
  labs(
    x = "Experience with data?",
    y = NULL,
    fill = "Difficulty:"
  ) +
  # NOTE: This will make sure higher difficulties are to the right
  scale_y_reverse(labels = percent_format()) +
  coord_flip() +
  theme(
    legend.position = "top",
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )
