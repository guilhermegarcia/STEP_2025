library(tidyverse)
library(scales)
library(janitor)

# NOTE: Load q-1 and danish-1
#
# NOTE: Part 1: Data cleaning + key tidyverse functions (review)
# mutate, filter, select, if_else, summarize, rename, arrange, etc.
# Real scenario: you have some chaotic data to analyse
# First, you need to "clean" it; only then can you truly work with it
# So before discussing figures, we need to make sure we know how
# to prepare the data, i.e., how to clean it.


# QUESTION: Does the file follow the tidy format? Is it long or wide?
# NOTE: First step: print names of cols

q <- read_csv2("data/survey.csv")

glimpse(q)
names(q)

# Step 1: standardize col names
q <- q |> clean_names()
q

# Step 2: remove useless columns
names(q)
q <- q |>
  select(-matches("^(this_will|heure|nom|adresse|total|quiz|feedback|points)"))

names(q)
q

# Step 3: Wide to long / Long to wide transform
long <- q |>
  pivot_longer(
    names_to = "question",
    values_to = "answer",
    cols = 6:15
  )

names(long)
names(q)

# Step 4: Rename remaining columns (long names etc.)
long <- long |>
  rename(
    "int_data" = 2,
    "int_coding" = 3,
    "exp_data" = 4,
    "exp_r" = 5,
    "difficulty" = 6
  )

long

# Step 5: Correct the answers
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

long <- long |>
  mutate(
    KEY = responses,
    .by = id
  )

long <- long |>
  mutate(
    correct = if_else(str_to_lower(answer) == str_to_lower(KEY), 1, 0)
  )

# Let's check that this works:
long |>
  select(c(answer, KEY, correct)) |>
  sample_n(size = 10)
# head(n = 10)

long

# FINAL STEPS:
# Adding dichotomous variables
names(long)

long <- long |>
  mutate(
    across(
      2:6, # targets of the function
      ~ if_else( # function to be applied
        .x > 3, "yes", "no" # conditional
      ),
      .names = "bin_{.col}" # names for the new columns
    )
  )

long <- long |>
  select(
    id:difficulty,
    bin_int_data:bin_difficulty,
    question, answer, correct
  ) |>
  mutate(id = str_c("part_", id)) |>
  mutate(
    across(where(is_character), as_factor)
  ) |>
  mutate(
    across(2:6, ~ factor(.x, ordered = TRUE))
  )

long |> glimpse()

# Create a copy with a more descriptive name
survey_step <- long
# save(survey_step, file = "data/survey_clean.RData")

# NOTE: Part 1.1: Practice

# QUESTION:
# 1. What are some obvious figures for these data?
ggplot(data = long, aes(x = exp_r, y = correct)) +
  stat_summary()

# 2. Can you combine multiple variables in the same figure? What variables would you combine?
ggplot(data = long, aes(x = exp_r, y = correct)) +
  stat_summary() +
  facet_grid(~int_coding,
    labeller = "label_both",
    scales = "free"
  )

# 3. What relationships do you observe between the variables plotted?


# NOTE: Part 2: Aesthetics
# Let's use both our survey data and danish-1.csv here.

# NOTE: Percentages, colours, font, labels, themes
ggplot(
  data = long,
  aes(
    x = exp_r,
    y = correct,
    color = bin_difficulty
  )
) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar", width = 0,
    linewidth = 3,
    # color = "black",
    # color = "slateblue",
    alpha = 0.2,
    position = position_dodge(width = 0.7),
    show.legend = FALSE
  ) +
  stat_summary(position = position_dodge(width = 0.7)) +
  theme_bw(
    base_size = 13,
    base_family = "Futura"
  ) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("darkorange2", "slateblue")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Experience with R",
    y = "Accuracy in survey",
    color = "Perceived difficulty",
    title = "Our figure",
    # subtitle = "Never use Comic Sans",
    caption = "This is a caption"
  )

# ggsave("~/Desktop/my_figure.jpeg",
#        width = 6,
#        height = 4,
#        dpi = 500)

# QUESTION: Go back to the Danish data set
# and create a figure with customized aesthetics.
# You decide what aspects of the figure you'd like to alter.

d <- read_csv("data/danish-1.csv")
d

ggplot(
  data = d,
  aes(
    x = Affix |> fct_reorder(RT, .fun = mean),
    y = RT
  )
) +
  # stat_summary(
  #   geom = "line",
  #   aes(group = Participant),
  #   alpha = 0.1
  # ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar", width = 0,
    linewidth = 3,
    color = "slateblue",
    alpha = 0.2
  ) +
  stat_summary(position = position_dodge(width = 0.7)) +
  theme_classic(
    base_size = 13,
    base_family = "Futura"
  ) +
  labs(
    x = "Affix",
    y = "Reaction time (ms)",
    title = "Danish data"
  )


# NOTE: Labels instead of keys:
# Create a subset with just the following speakers:
# Part_5, Part_15, and Part_8
# And only the suffixes "bar" and "ede"

ggplot(
  data = d |>
    filter(
      Participant %in% c("Part_5", "Part_15", "Part_8"),
      Affix %in% c("bar", "ede")
    ),
  aes(
    x = Affix |> fct_reorder(RT, .fun = mean),
    y = RT,
    color = Participant
  )
) +
  stat_summary(aes(group = Participant),
    geom = "line",
    linetype = "dashed",
    alpha = 0.5,
    position = position_dodge(width = 0.7)
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar", width = 0,
    position = position_dodge(width = 0.7),
    linewidth = 3,
    # color = "slateblue",
    alpha = 0.2
  ) +
  stat_summary(position = position_dodge(width = 0.7)) +
  geom_label(
    data = d |>
      filter(
        Participant %in% c("Part_5", "Part_15", "Part_8"),
        Affix %in% c("bar")
      ) |>
      summarize(
        RT = mean(RT),
        .by = c(Affix, Participant)
      ),
    aes(label = Participant),
    position = position_dodge(width = 0.7),
    # NOTE: Or you can use position_nudge(x = ...) if
    # you prefer to have the labels *next* to the means
    family = "Futura"
  ) +
  theme_classic(
    base_size = 13,
    base_family = "Futura"
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Affix",
    y = "Reaction time (ms)",
    title = "Danish data"
  ) +
  # NOTE: If you want to change colours:
  scale_color_manual(
    values = c("black", "darkorange2", "slateblue")
  )


# NOTE: What if you wanted to remove the key from
# our previous figure (long)?
# NOTE: Markdown: Removing legend and using title instead

colours <- c("darkorange2", "slateblue")
names(colours) <- c("difficult", "easy")

customTitle <- glue::glue('Accuracy based on
<span style="color:{colours["difficult"]}">**difficult**</span> and
<span style="color:{colours["easy"]}">**easy**</span>
                          impressions')

# HACK: Make this into a snippet so you won't need
# to remember it.

ggplot(
  data = long,
  aes(
    x = exp_r,
    y = correct,
    color = bin_difficulty
  )
) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "errorbar", width = 0,
    linewidth = 3,
    # color = "black",
    # color = "slateblue",
    alpha = 0.2,
    position = position_dodge(width = 0.7),
    show.legend = FALSE
  ) +
  stat_summary(position = position_dodge(width = 0.7)) +
  theme_bw(
    base_size = 13,
    base_family = "Futura"
  ) +
  theme(
    legend.position = "none",
    plot.title = ggtext::element_markdown()
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("darkorange2", "slateblue")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Experience with R",
    y = "Accuracy",
    color = "Perceived difficulty",
    title = customTitle,
    # subtitle = "Never use Comic Sans",
    # caption = "This is a caption"
  )

# NOTE: EXTRA: Plotting ordinal data
# See tutorial here:
# https://gdgarcia.ca/posts/2023-10-23-scalar-data/index.html
