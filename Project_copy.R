# Total number of orders per user
user_total_orders <- final_train_df[, .(total_orders = max(order_number)), by = user_id]

# Average days between orders per user
user_avg_days <- final_train_df[, .(avg_days_between_orders = mean(days_since_prior_order, na.rm = TRUE)), by = user_id]

# User reorder ratio: how often do they reorder products?
user_reorder_ratio <- final_train_df[, .(reorder_ratio = sum(reordered) / .N), by = user_id]

# Preferred order day and hour (most frequent day/hour)
user_pref_day <- final_train_df[, .N, by = .(user_id, order_dow)][, .SD[which.max(N)], by = user_id][, .(user_id, preferred_day = order_dow)]
user_pref_hour <- final_train_df[, .N, by = .(user_id, order_hour_of_day)][, .SD[which.max(N)], by = user_id][, .(user_id, preferred_hour = order_hour_of_day)]

# Merge all user features
user_features <- Reduce(function(x, y) merge(x, y, by = "user_id"), list(user_total_orders, user_avg_days, user_reorder_ratio, user_pref_day, user_pref_hour))

names(user_features)
#############################################################################

# Reorder rate for product
product_reorder_rate <- final_train_df[, .(prod_reorder_rate = sum(reordered) / .N), by = product_id]

# Product popularity (number of unique users)
product_popularity <- final_train_df[, .(product_popularity = uniqueN(user_id)), by = product_id]

# Average cart position
product_cart_pos <- final_train_df[, .(avg_cart_position = mean(add_to_cart_order, na.rm = TRUE)), by = product_id]

# Merge all product features
product_features <- Reduce(function(x, y) merge(x, y, by = "product_id"), list(product_reorder_rate, product_popularity, product_cart_pos))

names(product_features)

#############################################################################

# Number of times user ordered product
user_prod_orders <- final_train_df[, .(up_order_count = .N), by = .(user_id, product_id)]

# Time since last purchase (based on max order_number)
last_order_time <- final_train_df[, .(last_order = max(order_number)), by = .(user_id, product_id)]
current_user_max_order <- final_train_df[, .(user_max_order = max(order_number)), by = user_id]
user_prod_last_order_gap <- merge(last_order_time, current_user_max_order, by = "user_id")
user_prod_last_order_gap[, time_since_last_purchase := user_max_order - last_order]

# Whether reordered last time (get last order and check if reordered)
user_prod_last_reordered <- final_train_df[, .SD[which.max(order_number)], by = .(user_id, product_id)][, .(user_id, product_id, last_reordered = reordered)]

# Merge all user-product features
user_product_features <- Reduce(function(x, y) merge(x, y, by = c("user_id", "product_id")), 
                                list(user_prod_orders, user_prod_last_order_gap[, .(user_id, product_id, time_since_last_purchase)], user_prod_last_reordered))

names(user_product_features)

############################################################################
# Merge user_features on user_id
final_train_df <- merge(final_train_df, user_features, by = "user_id", all.x = TRUE) 

# Merge product_features on product_id
final_train_df <- merge(final_train_df, product_features, by = "product_id", all.x = TRUE)

# Merge user_product_features on both user_id and product_id
final_train_df <- merge(final_train_df, user_product_features, by = c("user_id", "product_id"), all.x = TRUE)
