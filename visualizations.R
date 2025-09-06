# Load necessary library
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)


features <- c(
  "order_hour_of_day", "order_dow", "days_since_prior_order",
  "reorder_ratio", "preferred_hour", "prod_reorder_rate",
  "product_popularity", "avg_cart_position", "time_since_last_purchase"
)

ggplot(final_train_df, aes(y = order_hour_of_day)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Boxplot of order_hour_of_day", y = "Order Hour of Day") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(final_train_df, aes(y = order_dow)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  labs(title = "Boxplot of order_dow", y = "Order Day of Week") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(final_train_df, aes(y = days_since_prior_order)) +
  geom_boxplot(fill = "lightpink", color = "darkred") +
  theme_minimal() +
  labs(title = "Boxplot of days_since_prior_order", y = "Days Since Prior Order") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(final_train_df, aes(y = reorder_ratio)) +
  geom_boxplot(fill = "lightyellow", color = "orange") +
  theme_minimal() +
  labs(title = "Boxplot of reorder_ratio", y = "Reorder Ratio") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(final_train_df, aes(y = preferred_hour)) +
  geom_boxplot(fill = "lightcyan", color = "blue") +
  theme_minimal() +
  labs(title = "Boxplot of preferred_hour", y = "Preferred Hour") +
  theme(plot.title = element_text(hjust = 0.5))


#HAS OUTLIERS
ggplot(final_train_df, aes(y = prod_reorder_rate)) +
  geom_boxplot(fill = "lavender", color = "purple") +
  theme_minimal() +
  labs(title = "Boxplot of prod_reorder_rate", y = "Product Reorder Rate") +
  theme(plot.title = element_text(hjust = 0.5))

#HAS OUTLIERS
ggplot(final_train_df, aes(y = product_popularity_log)) +
  geom_boxplot(fill = "peachpuff", color = "brown") +
  theme_minimal() +
  labs(title = "Boxplot of product_popularity_log", y = " product_popularity_log") +
  theme(plot.title = element_text(hjust = 0.5))

#HAS OUTLIERS
ggplot(final_train_df, aes(y = avg_cart_position)) +
  geom_boxplot(fill = "plum", color = "darkviolet") +
  theme_minimal() +
  labs(title = "Boxplot of avg_cart_position", y = "Average Cart Position") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(final_train_df, aes(y = time_since_last_purchase)) +
  geom_boxplot(fill = "lightcoral", color = "firebrick") +
  theme_minimal() +
  labs(title = "Boxplot of time_since_last_purchase", y = "Time Since Last Purchase") +
  theme(plot.title = element_text(hjust = 0.5))




# Load ggplot2 if not already
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Plot histogram + kernel density for avg_cart_position
ggplot(final_train_df, aes(x = product_popularity)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1, 
                 fill = "lightblue", 
                 color = "black", 
                 alpha = 0.6) +
  geom_density(color = "red", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Histogram and Density Plot of Average Cart Position",
    x = "Product Popularity",
    y = "Density"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate quantiles for avg_cart_position
quantile(final_train_df$product_popularity, probs = seq(0, 1, 0.01), na.rm = TRUE)


############### REMOVING OUTLIERS - product_popularity #########################

# Apply log(1 + x) to avoid log(0)
final_train_df[, product_popularity_log := log1p(product_popularity)]

# Visualize new log-transformed feature
ggplot(final_train_df, aes(x = product_popularity_log)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Histogram of Log-Transformed Product Popularity",
    x = "Log(Product Popularity + 1)",
    y = "Density"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(final_train_df, aes(y = product_popularity_log)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", outlier.colour = "red", outlier.shape = 1) +
  theme_minimal() +
  labs(
    title = "Boxplot of Log-Transformed Product Popularity",
    y = "Log(Product Popularity + 1)",
    x = ""
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#DELETE product_popularity COLUMN
final_train_df[, product_popularity := NULL]


###### REMOVING OUTLIERS - avg_cart_position ############

ggplot(final_train_df, aes(x = avg_cart_position)) +
  geom_histogram(binwidth = 1, fill = "plum", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Histogram of Average Cart Position",
    x = "Average Cart Position",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(final_train_df, aes(x = avg_cart_position)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "plum", color = "black", alpha = 0.6) +
  geom_density(color = "darkviolet", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Histogram and Density of Average Cart Position",
    x = "Average Cart Position",
    y = "Density"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

quantile(final_train_df$avg_cart_position, probs = seq(0, 1, 0.01), na.rm = TRUE)

filtered_df <- final_train_df[avg_cart_position > 30]

Q1 <- quantile(final_train_df$avg_cart_position, 0.25, na.rm = TRUE)
Q3 <- quantile(final_train_df$avg_cart_position, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

final_train_df <- final_train_df[
  avg_cart_position >= lower_bound & avg_cart_position <= upper_bound
]

summary(final_train_df$avg_cart_position)  # check new min, max
boxplot(final_train_df$avg_cart_position)  # confirm visually



##### REMOVING OUTLIERS - prod_reorder_rate #############

library(ggplot2)

ggplot(final_train_df, aes(x = prod_reorder_rate)) +
  geom_histogram(binwidth = 0.05, fill = "lavender", color = "purple", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Histogram of Product Reorder Rate",
    x = "Product Reorder Rate",
    y = "Count"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

quantile(final_train_df$prod_reorder_rate, probs = seq(0, 1, 0.01), na.rm = TRUE)
