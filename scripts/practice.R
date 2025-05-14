# NOTE: Practice script for days 1, 2, 3
# Load phonetique.RData using load()
load("data/phonetique.RData")
phon

# NOTE: PART 0 — Descriptive Summaries

### 1. Basic distribution
# Q: What is the average number of hours of instruction across the two Conditions?
# A:
phon |>
  summarize(Mean_Hours = mean(Hours), .by = Condition)

### 2. Group comparison
# Q: What is the mean gain score by L1? Sort from high to low gain.
# A:
phon |>
  mutate(Gain = Post - Pre) |>
  summarize(Mean_Gain = mean(Gain), .by = c(L1)) |>
  arrange(desc(Mean_Gain))

### 3. Most effective condition
# Q: Which group shows the largest average improvement? Consider combining L1 and Condition.
# A:
phon |>
  mutate(Gain = Post - Pre) |>
  summarize(Mean_Gain = mean(Gain), .by = c(L1, Condition)) |>
  arrange(desc(Mean_Gain))

### 4. Individual case
# Q: Who is the learner with the largest improvement? How about the smallest?
# A: Bear in mind that each row is *already* a single speaker, so there's
# only one obs per person
phon |>
  mutate(Gain = Post - Pre) |>
  select(ID, Gain) |>
  arrange(desc(Gain)) |>
  slice(c(1, n())) # HACK: This is quite useful to extract first and last obs

## NOTE: PART 1 — Geoms and Variable Relationships

### 5. Visualize gain by Condition
# Q: Create a boxplot showing gain scores across Conditions and L1. What patterns do you notice?
# A: We might as well add gain as a permanent col (!)
phon <- phon |>
  mutate(Gain = Post - Pre)

ggplot(data = phon, aes(x = Condition, y = Gain)) +
  geom_boxplot(aes(fill = L1))
# A: Overall, we see an effect of the phonétique method; the Danish group
# outperforms the other groups in this condition


### 6. Visualize Age vs gain
# Q: Create a scatterplot showing Age on the x-axis and gain score on the y-axis. Fit a line to it.
# Does it look like there's an effect?
# A: No.

ggplot(data = phon, aes(x = Age, y = Gain)) +
  geom_point() +
  stat_smooth(method = "lm")

### 7. Add a third variable
# Q: Add `Condition` to the figure
# Does your answer to the previous question change?
# A: Not really.

ggplot(data = phon, aes(x = Age, y = Gain)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_grid(~Condition)


### 8. Global vs local aesthetics
# Q: Create two versions of the previous plot:
# - one where color has a global scope
# - one with `aes(color = Condition)` applied only to one layer
# A: Global:
ggplot(data = phon, aes(x = Age, y = Gain, color = Condition)) +
  geom_point() +
  stat_smooth(method = "lm")

# A: Local:
ggplot(data = phon, aes(x = Age, y = Gain)) +
  geom_point(aes(color = Condition)) +
  stat_smooth(method = "lm")


## NOTE: PART 2 — Data Cleaning and Presentation

### 9. Recode Condition
# Q: Recode `Condition` to English labels (e.g., "phonétique" → "Phonetic-based", "traditionnelle" → "Traditional"). Use the recoded version in a new plot.
# A:
phon <- phon |>
  mutate(Condition = Condition |> fct_recode(
    "Traditional" = "traditionnelle",
    "Phonetic-based" = "phonétique"
  ))

ggplot(data = phon, aes(x = Age, y = Gain, color = Condition)) +
  geom_point() +
  stat_smooth(method = "lm")



### 10. Long format
# Q: Transform the dataset to long format with columns: `Time` (Pre vs Post), `Score`. Plot Pre vs Post by Condition.
# A:
long <- phon |>
  pivot_longer(
    names_to = "Time",
    values_to = "Score",
    cols = Pre:Post
  )
long

ggplot(data = long, aes(x = Time, y = Score, fill = Condition)) +
  geom_boxplot()

### 11. Percent change
# Q: Create a new column for percent gain: `(Post - Pre) / Pre * 100`. Visualize it by `L1`.
# A:
phon <- phon |>
  mutate(Perc_Gain = ((Post - Pre) / Pre) * 100)

ggplot(data = phon, aes(x = L1, y = Perc_Gain)) +
  stat_summary()

## NOTE: PART 3 — Advanced Visualization and Modeling

### 12. Label best learners
# Q: Plot gain scores with `geom_col()` and use `geom_label()` to label learners with a gain above 20 points.
# You can get rid of the labels along the x axis since `geom_label()` will do the job.
# A:
ggplot(
  data = phon |> filter(Gain > 20),
  aes(
    x = ID |> fct_reorder(Gain),
    y = Gain,
    label = ID
  )
) +
  geom_col() +
  geom_label(position = position_nudge(y = 2)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

### 13. Use patchwork
# Q: Combine two plots:
# - gain by Condition
# - gain by L1
# What aesthetic changes can you make since both plots share the same y axis?
# A:
p1 <- ggplot(data = phon, aes(x = Condition, y = Gain)) +
  stat_summary()
p2 <- ggplot(data = phon, aes(x = L1, y = Gain)) +
  stat_summary()

p1 + p2

### 14. Use plotly
# Q: Create an interactive version of the scatterplot from Question 6 using plotly.
# A:
ggplot(data = phon, aes(x = Age, y = Gain)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplotly()


### 15. Save plot
# Q: Save the plot from Question 5 as a JPEG file.
# A:
ggplot(data = phon, aes(x = Condition, y = Gain)) +
  geom_boxplot(aes(fill = L1))

ggsave(file = "~/Desktop/plot15.jpeg", dpi = 500, width = 6, height = 3)
