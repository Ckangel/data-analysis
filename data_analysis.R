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
