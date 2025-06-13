# Load necessary libraries
library(ggplot2)
library(dplyr)
library(transport)  # for wasserstein1d if desired
set.seed(42)

# Generate two 1D distributions
n <- 1000
dist1 <- rnorm(n, mean = 0, sd = 1)  # Standard normal
dist2 <- c(rnorm(n/2, mean = -2, sd = 0.5), rnorm(n/2, mean = 2, sd = 0.5))  # Bimodal

# Create a combined data frame
df <- data.frame(
  value = c(dist1, dist2),
  group = rep(c("Distribution A", "Distribution B"), each = n)
)

# Compute empirical CDFs
ecdf_df <- df %>%
  group_by(group) %>%
  arrange(value) %>%
  mutate(ecdf = ecdf(value)(value)) %>%
  ungroup()

# Set consistent colors
colors <- c("Distribution A" = "#1b9e77", "Distribution B" = "#d95f02")

# Plot the CDFs
ggplot(ecdf_df, aes(x = value, y = ecdf, color = group)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  labs(
    title = "Empirical Cumulative Distribution Functions",
    subtitle = "Wasserstein distance = area between these curves",
    x = "Value",
    y = "ECDF",
    color = "Distribution"
  ) +
  theme_minimal(base_size = 14)

# Optionally compute the Wasserstein-1 distance numerically
# Note: requires sorted samples of equal size
wasserstein_dist <- wasserstein1d(sort(dist1), sort(dist2), p = 1)
print(paste("Wasserstein-1 distance:", round(wasserstein_dist, 3)))
