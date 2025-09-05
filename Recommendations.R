# Ensure you're predicting using the same scaled features
d_final <- xgb.DMatrix(data = as.matrix(final_train_df[, ..features]))

# Predict reorder probabilities using the trained model
final_train_df[, reorder_prob := predict(model, d_final)]

library(data.table)

# Sort by user_id and reorder_prob (descending)
setorder(final_train_df, user_id, -reorder_prob)

# Select top-N products per user
top_n <- 5
top_n_df <- final_train_df[, head(.SD, top_n), by = user_id]

# Group recommended product_ids into a list per user
recommendations <- top_n_df[, .(recommended_products = list(product_id)), by = user_id]

# Create a space-separated string of product IDs
recommendations[, product_list := sapply(recommended_products, function(x) paste(x, collapse = " "))]
recommendations[, recommended_products := NULL]  # optional clean-up

write.csv(recommendations[, .(user_id, product_list)], "user_recommendations.csv", row.names = FALSE)


##############################
# Sort and pick top N
setorder(final_train_df, user_id, -reorder_prob)
top_n <- 5
top_n_df <- final_train_df[, head(.SD, top_n), by = user_id]

library(ggplot2)

# Count top recommended products
top_products <- top_n_df[, .N, by = product_id][order(-N)]

# Plot top 20 most recommended products
ggplot(top_products[1:20], aes(x = reorder(as.factor(product_id), -N), y = N)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(
    title = "Top 20 Most Frequently Recommended Products",
    x = "Product ID",
    y = "Recommendation Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Count how many products were recommended per user
user_rec_counts <- top_n_df[, .N, by = user_id]

# Plot histogram
ggplot(user_rec_counts, aes(x = N)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Number of Recommendations per User",
    x = "Number of Products Recommended",
    y = "Number of Users"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
