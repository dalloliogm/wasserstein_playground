# Load required packages
library(ggplot2)
library(dplyr)
library(transport)  # for wasserstein1d

set.seed(42)

# Generate distributions
n <- 1000
dist1 <- rnorm(n, mean = 0, sd = 1)
dist2 <- c(rnorm(n/2, mean = -2, sd = 0.5), rnorm(n/2, mean = 2, sd = 0.5))

# Common x-axis grid
x_grid <- sort(unique(c(dist1, dist2)))
x_grid <- seq(min(x_grid), max(x_grid), length.out = 500)

# Compute empirical CDFs
ecdf1 <- ecdf(dist1)(x_grid)
ecdf2 <- ecdf(dist2)(x_grid)

# Data for plotting
plot_df <- data.frame(
  x = x_grid,
  ecdf1 = ecdf1,
  ecdf2 = ecdf2
)

# Long format for ggplot
plot_long <- tidyr::pivot_longer(plot_df, cols = c("ecdf1", "ecdf2"),
                                 names_to = "group", values_to = "ecdf") %>%
  mutate(group = recode(group,
                        ecdf1 = "Distribution A",
                        ecdf2 = "Distribution B"))

# Plot with shaded area between CDFs
ggplot() +
  geom_ribbon(data = plot_df, aes(x = x, ymin = pmin(ecdf1, ecdf2), ymax = pmax(ecdf1, ecdf2)),
              fill = "gray80", alpha = 0.6) +
  geom_line(data = plot_long, aes(x = x, y = ecdf, color = group), size = 1.2) +
  scale_color_manual(values = c("Distribution A" = "#1b9e77", "Distribution B" = "#d95f02")) +
  labs(
    title = "Wasserstein-1 Distance as Area Between CDFs",
    subtitle = "Shaded area ≈ transport cost (Earth Mover's Distance)",
    x = "Value",
    y = "Empirical CDF",
    color = "Distribution"
  ) +
  theme_minimal(base_size = 14)

# Optional: print Wasserstein-1 distance
wasserstein_dist <- wasserstein1d(sort(dist1), sort(dist2), p = 1)
print(paste("Wasserstein-1 distance:", round(wasserstein_dist, 3)))
