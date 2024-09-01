# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the sleep dataset
data(sleep)

# View the structure and summary of the dataset
str(sleep)
summary(sleep)

# View first few rows of the dataset
head(sleep)

# Boxplot of sleep increase by drug group
ggplot(sleep, aes(x = group, y = extra, fill = group)) +
  geom_boxplot() +
  labs(title = "Increase in Hours of Sleep by Drug Group",
       x = "Drug Group",
       y = "Increase in Hours of Sleep") +
  theme_minimal()

# Load necessary libraries
library(dplyr)

# Extract the data for each group
group1 <- sleep$extra[sleep$group == 1]
group2 <- sleep$extra[sleep$group == 2]

# Perform the paired t-test
t_test_result <- t.test(group1, group2, paired = TRUE)

# Print t-test results
print(t_test_result)


# Calculate sleep prolongation
sleep_prolongation <- with(sleep, extra[group == 2] - extra[group == 1])
summary(sleep_prolongation)

# Plot sleep prolongation
stripchart(sleep_prolongation, method = "stack", xlab = "Hours",
           main = "Sleep Prolongation (n = 10)")
boxplot(sleep_prolongation, horizontal = TRUE, add = TRUE,
        at = .6, pars = list(boxwex = 0.5, staplewex = 0.25))


# Calculate the difference in sleep increase between groups
sleep_diff <- with(sleep, extra[group == 2] - extra[group == 1])

# Violin plot of sleep increase by drug group
ggplot(sleep, aes(x = group, y = extra, fill = group)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Distribution and Density of Sleep Increase by Drug Group",
       x = "Drug Group",
       y = "Increase in Hours of Sleep") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral"))
