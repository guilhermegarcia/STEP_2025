# NOTE: Practice script for days 1, 2, 3
# Load phonetique.RData using load()

# NOTE: PART 0 — Descriptive Summaries

### 1. Basic distribution
# Q: What is the average number of hours of instruction across the two Conditions?
# A:

### 2. Group comparison
# Q: What is the mean gain score by L1? Sort from high to low gain.
# A:

### 3. Most effective condition
# Q: Which group shows the largest average improvement? Consider combining L1 and Condition.
# A:

### 4. Individual case
# Q: Who is the learner with the largest improvement? How about the smallest?
# A:

## NOTE: PART 1 — Geoms and Variable Relationships

### 5. Visualize gain by Condition
# Q: Create a boxplot showing gain scores across Conditions and L1. What patterns do you notice?
# A:

### 6. Visualize Age vs gain
# Q: Create a scatterplot showing Age on the x-axis and gain score on the y-axis. Fit a line to it.
# Does it look like there's an effect?
# A:

### 7. Add a third variable
# Q: Add `Condition` to the figure
# Does your answer to the previous question change?
# A:

### 8. Global vs local aesthetics
# Q: Create two versions of the previous plot:
# - one where color has a global scope
# - one with `aes(color = Condition)` applied only to one layer
# A:


## NOTE: PART 2 — Data Cleaning and Presentation

### 9. Recode Condition
# Q: Recode `Condition` to English labels (e.g., "phonétique" → "Phonetic-based", "traditionnelle" → "Traditional"). Use the recoded version in a new plot.
# A:


### 10. Long format
# Q: Transform the dataset to long format with columns: `Time` (Pre vs Post), `Score`. Plot Pre vs Post by Condition.
# A:

### 11. Percent change
# Q: Create a new column for percent gain. Visualize it by `L1`.
# A:


## NOTE: PART 3 — Advanced Visualization and Modeling

### 12. Label best learners
# Q: Plot gain scores with `geom_col()` and use `geom_label()` to label learners with a gain above 20 points.
# You can get rid of the labels along the x axis since `geom_label()` will do the job.
# A:

### 13. Use patchwork
# Q: Combine two plots:
# - gain by Condition
# - gain by L1
# What aesthetic changes can you make since both plots share the same y axis?
# A:

### 14. Use plotly
# Q: Create an interactive version of the scatterplot from Question 6 using plotly.
# A:


### 15. Save plot
# Q: Save the plot from Question 5 as a JPEG file.
# A:
