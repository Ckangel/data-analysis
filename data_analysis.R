# -------------------------------
# Day 1: Loading and Summarizing
# -------------------------------
data(mtcars)

# View first few rows
head(mtcars)

# Summary statistics for mpg
summary(mtcars$mpg)

# Mean, median, standard deviation
mean(mtcars$mpg)
median(mtcars$mpg)
sd(mtcars$mpg)

# Histogram
hist(mtcars$mpg,
  main = "Histogram of Miles per Gallon", # nolint
  xlab = "MPG",
  col = "lightblue",
  border = "black"
)

# -------------------------------
# Day 2: Filtering, Grouping, Bar Chart
# -------------------------------
library(dplyr) # nolint
library(ggplot2)

# Filter cars with mpg > 25
high_mpg <- subset(mtcars, mpg > 25)
print(high_mpg)

# Group by cylinders and calculate average mpg
grouped_data <- mtcars %>% # nolint
  group_by(cyl) %>% # nolint
  summarise(avg_mpg = mean(mpg))
print(grouped_data)

# Bar chart
ggplot(grouped_data, aes(x = factor(cyl), y = avg_mpg)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average MPG by Cylinder Count",
    x = "Number of Cylinders",
    y = "Average MPG"
  )

# -------------------------------
# Day 3: Scatterplots & Correlation
# -------------------------------
# Base R scatterplot
library(dplyr)
library(ggplot2)

plot(mtcars$hp, mtcars$mpg,
  main = "Horsepower vs. MPG",
  xlab = "Horsepower",
  ylab = "Miles per Gallon",
  pch = 19, col = "darkgreen"
)

# Correlation
cor(mtcars$hp, mtcars$mpg)
cor(mtcars[, c("mpg", "hp", "wt", "disp")])

# ggplot scatterplot with regression line
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Horsepower vs. MPG with Regression Line",
    x = "Horsepower",
    y = "Miles per Gallon"
  )

# -------------------------------
# Day 4: Reusable Functions
# -------------------------------
library(dplyr)
library(ggplot2)

filter_by_mpg <- function(data, min_mpg) {
  subset(data, mpg > min_mpg) # nolint
}

group_avg_mpg <- function(data, group_col) {
  data %>% # nolint
    group_by({{ group_col }}) %>% # nolint
    summarise(avg_mpg = mean(mpg)) # nolint
}

plot_avg_mpg <- function(data, group_col) {
  grouped <- group_avg_mpg(data, {{ group_col }})
  ggplot(grouped, aes(x = factor({{ group_col }}), y = avg_mpg)) + # nolint
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = paste("Average MPG by", deparse(substitute(group_col))),
      x = deparse(substitute(group_col)),
      y = "Average MPG"
    )
}

analyze_mpg <- function(data, min_mpg, group_col) {
  filtered <- filter_by_mpg(data, min_mpg)
  summary <- group_avg_mpg(filtered, {{ group_col }})
  print(summary)
  plot_avg_mpg(filtered, {{ group_col }})
}

# Example usage
analyze_mpg(mtcars, 20, cyl)

# -------------------------------
# Day 5: Polished Visualizations
# -------------------------------
# Labels and titles
library(dplyr)
library(ggplot2)

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  labs(
    title = "Average MPG by Cylinder Count",
    subtitle = "Data from mtcars dataset",
    x = "Number of Cylinders",
    y = "Average Miles per Gallon",
    caption = "Source: R built-in mtcars dataset"
  )

# Custom colors
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("4" = "forestgreen", "6" = "goldenrod", "8" = "firebrick")) + # nolint
  labs(
    title = "Average MPG by Cylinder Count",
    x = "Cylinders",
    y = "Average MPG"
  )

# Themes
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Average MPG by Cylinder Count") +
  theme_minimal(base_size = 14)

# Labels on bars
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  stat_summary(
    fun = "mean", geom = "text", aes(label = round(..y.., 1)),
    vjust = -0.5, color = "black"
  ) +
  labs(
    title = "Average MPG by Cylinder Count",
    x = "Cylinders",
    y = "Average MPG"
  )

# -------------------------------
# Day 6: Multiple Visualizations
# -------------------------------
library(dplyr)
library(ggplot2)

hist_plot <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of MPG", x = "Miles per Gallon", y = "Count") +
  theme_minimal()

scatter_plot <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Horsepower vs MPG", x = "Horsepower", y = "Miles per Gallon") +
  theme_classic()

bar_plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  labs(title = "Average MPG by Cylinder Count", x = "Cylinders", y = "Average MPG") + # nolint
  theme_light()

library(patchwork)
(hist_plot | scatter_plot) / bar_plot

library(gridExtra)
grid.arrange(hist_plot, scatter_plot, bar_plot, ncol = 2)

# -------------------------------
# Day 7: Reporting with R Markdown
# -------------------------------
# Create report.Rmd separately with text + code chunks
# Render with:
library(dplyr)
library(ggplot2)

rmarkdown::render("report.Rmd") # nolint
