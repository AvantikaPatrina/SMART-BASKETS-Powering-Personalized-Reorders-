
library(data.table)

orders_df <- fread("orders.csv")
aisles_df <- fread("aisles.csv")
#order_products_prior_df <- fread("order_products_prior.csv")
order_products_train_df <-fread("order_products_train.csv")
products_df <-fread("products.csv")
departments_df <-fread("departments.csv")




#final_prior_df <- merge(order_products_prior_df, orders_df, by = "order_id", all = FALSE)

# Get unique user_ids (assuming 'user_id' column exists in final_df)
#unique_users <- unique(final_prior_df$user_id)[1:2000]

# Filter final_df to include only those user_ids
#final_prior_df <- final_prior_df[user_id %in% unique_users]

#rm(order_products_prior_df)


final_train_df <- merge(order_products_train_df, orders_df, by = "order_id", all = FALSE)

# Get unique user_ids (assuming 'user_id' column exists in final_df)
#unique_users <- unique(final_train_df$user_id)[1:2000]

# Filter final_df to include only those user_ids
#final_train_df <- final_train_df[user_id %in% unique_users]

#rm(order_products_train_df)


final_train_df <- merge(final_train_df, products_df, by = "product_id", all = FALSE)

final_train_df <- merge(final_train_df, departments_df, by = "department_id", all = FALSE)

final_train_df <- merge(final_train_df, aisles_df, by = "aisle_id", all = FALSE)



# Base R
#write.csv(final_prior_df, "instacart.csv", row.names = FALSE)


save.image(file = "my_workspace.RData")

