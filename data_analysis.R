# Load built-in dataset
data(mtcars)

# View first few rows
head(mtcars)

# Summary statistics for mpg (miles per gallon)
summary(mtcars$mpg)

# Mean and median
mean(mtcars$mpg)
median(mtcars$mpg)

# Standard deviation
sd(mtcars$mpg)

# Histogram of mpg
hist(mtcars$mpg,
    main = "Histogram of Miles per Gallon",
    xlab = "MPG",
    col = "lightblue",
    border = "black")

# Load dataset
data(mtcars)

# Filter cars with mpg greater than 25
high_mpg <- subset(mtcars, mpg > 25)

# Display results
print(high_mpg)

# Install dplyr if not already installed
# install.packages("dplyr")

library(dplyr)

# Group by number of cylinders and calculate average mpg
grouped_data <- mtcars %>%
  group_by(cyl) %>%
  summarise(avg_mpg = mean(mpg))

print(grouped_data)

# Install ggplot2 if not already installed
# install.packages("ggplot2")

library(ggplot2)

# Bar chart of average mpg by cylinder count
ggplot(grouped_data, aes(x = factor(cyl), y = avg_mpg)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average MPG by Cylinder Count",
       x = "Number of Cylinders",
       y = "Average MPG")

# Load dataset
data(mtcars)

# Basic scatterplot
plot(mtcars$hp, mtcars$mpg,
     main = "Horsepower vs. MPG",
     xlab = "Horsepower",
     ylab = "Miles per Gallon",
     pch = 19, col = "darkgreen")

# Calculate correlation between horsepower and mpg
cor(mtcars$hp, mtcars$mpg)

# Correlation matrix for multiple variables
cor(mtcars[, c("mpg", "hp", "wt", "disp")])

library(ggplot2)

# Scatterplot with regression line
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Horsepower vs. MPG with Regression Line",
       x = "Horsepower",
       y = "Miles per Gallon")

# Function to filter cars by minimum mpg
filter_by_mpg <- function(data, min_mpg) {
  subset(data, mpg > min_mpg)
}

# Example usage
data(mtcars)
high_mpg_cars <- filter_by_mpg(mtcars, 25)
print(high_mpg_cars)

library(dplyr)

# Function to group by a column and calculate average mpg
group_avg_mpg <- function(data, group_col) {
  data %>%
    group_by({{ group_col }}) %>%
    summarise(avg_mpg = mean(mpg))
}

# Example usage
avg_by_cyl <- group_avg_mpg(mtcars, cyl)
print(avg_by_cyl)

library(ggplot2)

# Function to plot average mpg by a grouping column
plot_avg_mpg <- function(data, group_col) {
  grouped <- data %>%
    group_by({{ group_col }}) %>%
    summarise(avg_mpg = mean(mpg))
  
  ggplot(grouped, aes(x = factor({{ group_col }}), y = avg_mpg)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Average MPG by", deparse(substitute(group_col))),
         x = deparse(substitute(group_col)),
         y = "Average MPG")
}

# Example usage
plot_avg_mpg(mtcars, cyl)

analyze_mpg <- function(data, min_mpg, group_col) {
  # Step 1: Filter
  filtered <- filter_by_mpg(data, min_mpg)
  
  # Step 2: Group and summarize
  summary <- group_avg_mpg(filtered, {{ group_col }})
  print(summary)
  
  # Step 3: Plot
  plot_avg_mpg(filtered, {{ group_col }})
}

# Example usage
analyze_mpg(mtcars, 20, cyl)

library(ggplot2)

# Basic bar chart with labels
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  labs(title = "Average MPG by Cylinder Count",
       subtitle = "Data from mtcars dataset",
       x = "Number of Cylinders",
       y = "Average Miles per Gallon",
       caption = "Source: R built-in mtcars dataset")

# Custom colors for bars
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = c("4" = "forestgreen", "6" = "goldenrod", "8" = "firebrick")) +
  labs(title = "Average MPG by Cylinder Count",
       x = "Cylinders",
       y = "Average MPG")

# Using a clean theme
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(title = "Average MPG by Cylinder Count") +
  theme_minimal(base_size = 14)

# Try other themes: theme_classic(), theme_dark(), theme_light()
# Bar chart with labels on bars
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 1)),
               vjust = -0.5, color = "black") +
  labs(title = "Average MPG by Cylinder Count",
       x = "Cylinders",
       y = "Average MPG")
