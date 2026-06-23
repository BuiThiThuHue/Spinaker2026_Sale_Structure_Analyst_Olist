# Sales structure analysis for the Olist dataset
# Question:
# Which products were most often purchased?
# What does not find their customers?

library(tidyverse)
library(lubridate)
library(scales)

if (!requireNamespace("factoextra", quietly = TRUE)) {
  stop("Package 'factoextra' is required. Install it with install.packages('factoextra').")
}

if (!requireNamespace("cluster", quietly = TRUE)) {
  stop("Package 'cluster' is required. Install it with install.packages('cluster').")
}

library(factoextra)

get_project_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_file <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(active_file) && nzchar(active_file)) {
      return(dirname(active_file))
    }
  }

  getwd()
}

project_dir <- get_project_dir()
data_dir <- file.path(project_dir, "olist dataset")

if (!dir.exists(data_dir)) {
  stop(
    "Data folder not found: ", data_dir, "\n",
    "Please open sales_structure_olist.R from the project folder in RStudio, ",
    "or set project_dir manually to the folder that contains 'olist dataset'."
  )
}

orders <- read_csv(file.path(data_dir, "olist_orders_dataset.csv"), show_col_types = FALSE)
order_items <- read_csv(file.path(data_dir, "olist_order_items_dataset.csv"), show_col_types = FALSE)
products <- read_csv(file.path(data_dir, "olist_products_dataset.csv"), show_col_types = FALSE)
category_translation <- read_csv(file.path(data_dir, "product_category_name_translation.csv"), show_col_types = FALSE)
payments <- read_csv(file.path(data_dir, "olist_order_payments_dataset.csv"), show_col_types = FALSE)
reviews <- read_csv(file.path(data_dir, "olist_order_reviews_dataset.csv"), show_col_types = FALSE)

# 1. Data inspection --------------------------------------------------------

glimpse(orders)
glimpse(order_items)
glimpse(products)
glimpse(category_translation)

orders %>% count(order_status, sort = TRUE)

order_items %>%
  summarise(
    order_item_rows = n(),
    distinct_orders = n_distinct(order_id),
    distinct_products = n_distinct(product_id),
    distinct_sellers = n_distinct(seller_id),
    total_item_revenue = sum(price, na.rm = TRUE),
    total_freight = sum(freight_value, na.rm = TRUE)
  )

products %>%
  summarise(
    catalog_products = n_distinct(product_id),
    catalog_categories = n_distinct(product_category_name, na.rm = TRUE),
    missing_category = sum(is.na(product_category_name))
  )

# 2. Joins ------------------------------------------------------------------

sales_items <- order_items %>%
  left_join(
    products %>%
      select(
        product_id,
        product_category_name,
        product_weight_g,
        product_length_cm,
        product_height_cm,
        product_width_cm,
        product_photos_qty
      ),
    by = "product_id"
  ) %>%
  left_join(category_translation, by = "product_category_name") %>%
  left_join(
    orders %>%
      select(order_id, customer_id, order_status, order_purchase_timestamp),
    by = "order_id"
  ) %>%
  mutate(
    category = coalesce(product_category_name_english, product_category_name, "unknown"),
    order_purchase_timestamp = ymd_hms(order_purchase_timestamp)
  )

delivered_sales_items <- sales_items %>%
  filter(order_status == "delivered")

# 3. Most often purchased products -----------------------------------------

top_products <- delivered_sales_items %>%
  group_by(product_id, category) %>%
  summarise(
    units_sold = n(),
    orders = n_distinct(order_id),
    customers = n_distinct(customer_id),
    revenue = sum(price, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(units_sold), desc(orders), desc(revenue))

top_products_ranked <- top_products %>%
  arrange(desc(units_sold), desc(revenue)) %>%
  mutate(
    product_rank = row_number(),
    product_name = str_to_title(str_replace_all(category, "_", " ")),
    product_label = paste0(product_name, " (", str_sub(product_id, 1, 8), ")"),
    sales_share = units_sold / sum(units_sold),
    cumulative_sales_share = cumsum(sales_share)
  )

top_products %>%
  slice_head(n = 20) %>%
  print(n = 20)

top_categories <- delivered_sales_items %>%
  group_by(category) %>%
  summarise(
    units_sold = n(),
    orders = n_distinct(order_id),
    customers = n_distinct(customer_id),
    revenue = sum(price, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(units_sold))

top_categories_ranked <- top_categories %>%
  arrange(desc(units_sold), desc(revenue)) %>%
  mutate(
    category_label = str_to_title(str_replace_all(category, "_", " ")),
    category_rank = row_number(),
    sales_share = units_sold / sum(units_sold),
    revenue_share = revenue / sum(revenue),
    cumulative_sales_share = cumsum(sales_share)
  )

top_categories %>%
  slice_head(n = 20) %>%
  print(n = 20)

# 4. Products/categories that do not find customers -------------------------

unsold_products <- products %>%
  anti_join(order_items, by = "product_id") %>%
  left_join(category_translation, by = "product_category_name") %>%
  mutate(category = coalesce(product_category_name_english, product_category_name, "unknown"))

no_delivered_sales_products <- products %>%
  anti_join(delivered_sales_items %>% distinct(product_id), by = "product_id") %>%
  left_join(category_translation, by = "product_category_name") %>%
  mutate(category = coalesce(product_category_name_english, product_category_name, "unknown"))

unsold_products %>%
  count(category, sort = TRUE) %>%
  print(n = 30)

no_delivered_sales_products %>%
  count(category, sort = TRUE) %>%
  print(n = 30)

least_purchased_products <- top_products %>%
  arrange(units_sold, orders, revenue) %>%
  slice_head(n = 20)

least_purchased_categories <- top_categories %>%
  arrange(units_sold, orders, revenue) %>%
  slice_head(n = 20)

least_purchased_products %>%
  print(n = 20)

least_purchased_categories %>%
  print(n = 20)

long_tail_summary <- top_products_ranked %>%
  summarise(
    sold_products = n(),
    products_with_one_unit_sold = sum(units_sold == 1),
    share_products_one_unit_sold = products_with_one_unit_sold / sold_products,
    share_sales_top_10_products = sum(sales_share[product_rank <= 10]),
    share_sales_top_100_products = sum(sales_share[product_rank <= 100]),
    products_needed_for_50pct_sales = min(product_rank[cumulative_sales_share >= 0.50]),
    products_needed_for_80pct_sales = min(product_rank[cumulative_sales_share >= 0.80])
  )

category_summary <- top_categories_ranked %>%
  summarise(
    sold_categories = n(),
    share_sales_top_5_categories = sum(sales_share[category_rank <= 5]),
    share_revenue_top_5_categories = sum(revenue_share[category_rank <= 5]),
    categories_needed_for_80pct_sales = min(category_rank[cumulative_sales_share >= 0.80])
  )

median_orders <- median(top_categories_ranked$orders)
median_value_per_order <- median(top_categories_ranked$revenue / top_categories_ranked$orders)

category_strategy <- top_categories_ranked %>%
  mutate(
    revenue_per_order = revenue / orders,
    volume_bucket = if_else(orders >= median_orders, "High Volume", "Low Volume"),
    value_bucket = if_else(revenue_per_order >= median_value_per_order, "High Value", "Low Value"),
    strategic_bucket = case_when(
      orders >= median_orders & revenue_per_order >= median_value_per_order ~ "High Volume - High Value",
      orders >= median_orders & revenue_per_order < median_value_per_order ~ "High Volume - Low Value",
      orders < median_orders & revenue_per_order >= median_value_per_order ~ "Low Volume - High Value",
      TRUE ~ "Low Volume - Low Value"
    )
  )

category_perf <- top_categories_ranked %>%
  mutate(revenue_per_order = revenue / orders)

strategic_bucket_table <- category_strategy %>%
  mutate(
    strategic_bucket = factor(
      strategic_bucket,
      levels = c(
        "High Volume - High Value",
        "High Volume - Low Value",
        "Low Volume - High Value",
        "Low Volume - Low Value"
      )
    )
  ) %>%
  arrange(strategic_bucket, desc(revenue)) %>%
  group_by(strategic_bucket) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  select(
    strategic_bucket,
    category_label,
    orders,
    revenue_per_order,
    revenue,
    sales_share,
    revenue_share
  )

long_tail_summary
category_summary
strategic_bucket_table

# 5. Advanced commercial health clustering ---------------------------------

# Prepare skewed e-commerce features with log transform and standard scaling.
commercial_scaled <- category_perf %>%
  select(category_label, units_sold, revenue, revenue_per_order) %>%
  mutate(
    across(
      c(units_sold, revenue, revenue_per_order),
      log1p,
      .names = "log_{.col}"
    )
  ) %>%
  select(category_label, starts_with("log_")) %>%
  column_to_rownames("category_label") %>%
  scale()

# Evaluate the number of clusters with K-means WSS and PAM silhouette.
plot_kmeans_elbow <- fviz_nbclust(
  commercial_scaled,
  kmeans,
  method = "wss"
) +
  labs(
    title = "Elbow Method for K-means",
    subtitle = "Commercial health features: log-transformed and scaled"
  ) +
  theme_minimal(base_size = 12)

plot_pam_silhouette <- fviz_nbclust(
  commercial_scaled,
  cluster::pam,
  method = "silhouette"
) +
  labs(
    title = "Average Silhouette Method for PAM",
    subtitle = "Commercial health features: log-transformed and scaled"
  ) +
  theme_minimal(base_size = 12)

# Run K-means and PAM clustering with k = 3.
set.seed(42)

kmeans_fit <- kmeans(
  commercial_scaled,
  centers = 3,
  nstart = 25
)

pam_fit <- cluster::pam(
  commercial_scaled,
  k = 3
)

# Append cluster labels back to the category-level performance table.
category_perf_clustered <- category_perf %>%
  mutate(
    KMeans_Cluster = factor(kmeans_fit$cluster),
    PAM_Cluster = factor(pam_fit$clustering)
  )

# Visualize clusters in a 2D PCA space.
plot_kmeans_clusters <- fviz_cluster(
  kmeans_fit,
  data = commercial_scaled,
  repel = TRUE,
  labelsize = 3,
  palette = c("#276b62", "#315c8c", "#a65345")
) +
  labs(
    title = "K-means Clusters: Commercial Health",
    subtitle = "PCA projection of category-level volume, revenue, and revenue per order"
  ) +
  theme_minimal(base_size = 12)

plot_pam_clusters <- fviz_cluster(
  pam_fit,
  data = commercial_scaled,
  repel = TRUE,
  labelsize = 3,
  palette = c("#276b62", "#315c8c", "#a65345")
) +
  labs(
    title = "PAM Clusters: Commercial Health",
    subtitle = "PAM is robust because each group is represented by a real category medoid"
  ) +
  theme_minimal(base_size = 12)

# Profile PAM clusters because medoid-based groups are easier to interpret.
pam_cluster_profile <- category_perf_clustered %>%
  group_by(PAM_Cluster) %>%
  summarise(
    total_categories = n(),
    avg_units_sold = mean(units_sold, na.rm = TRUE),
    avg_revenue = mean(revenue, na.rm = TRUE),
    avg_revenue_per_order = mean(revenue_per_order, na.rm = TRUE),
    .groups = "drop"
  )

pam_cluster_profile

# 6. ggplot2 visualisations -------------------------------------------------

plot_top_categories <- top_categories_ranked %>%
  slice_max(units_sold, n = 15) %>%
  mutate(category_label = fct_reorder(category_label, units_sold)) %>%
  ggplot(aes(x = units_sold, y = category_label)) +
  geom_col(fill = "#276b62", width = 0.74) +
  geom_text(aes(label = comma(units_sold)), hjust = -0.12, size = 3.2) +
  scale_x_continuous(labels = comma) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Demand is concentrated in a small set of categories",
    subtitle = "Top 15 categories by delivered units sold",
    x = "Units sold",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 35, 10, 10))

plot_top_products <- top_products_ranked %>%
  slice_max(units_sold, n = 15) %>%
  mutate(product_label = fct_reorder(product_label, units_sold)) %>%
  ggplot(aes(x = units_sold, y = product_label, fill = category)) +
  geom_col(show.legend = FALSE, width = 0.74) +
  geom_text(aes(label = comma(units_sold)), hjust = -0.12, size = 3.1) +
  scale_x_continuous(labels = comma) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Top individual products by delivered units",
    subtitle = "Olist does not provide real product names, so category names are used with short product IDs",
    x = "Units sold",
    y = "Product"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 35, 10, 10))

plot_least_categories <- least_purchased_categories %>%
  mutate(category_label = str_to_title(str_replace_all(category, "_", " ")),
         category_label = fct_reorder(category_label, units_sold)) %>%
  ggplot(aes(x = units_sold, y = category_label)) +
  geom_col(fill = "#a65345", width = 0.74) +
  geom_text(aes(label = comma(units_sold)), hjust = -0.12, size = 3.1) +
  scale_x_continuous(labels = comma) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Small categories have limited observed demand",
    subtitle = "Lowest-volume categories among delivered orders",
    x = "Units sold",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 35, 10, 10))

plot_strategic_matrix <- category_strategy %>%
  group_by(volume_bucket, value_bucket, strategic_bucket) %>%
  summarise(
    categories = n(),
    orders = sum(orders),
    revenue = sum(revenue),
    .groups = "drop"
  ) %>%
  mutate(
    order_share = orders / sum(orders),
    revenue_share = revenue / sum(revenue),
    label = paste0(
      strategic_bucket,
      "\n", categories, " categories",
      "\nRevenue: ", percent(revenue_share, accuracy = 0.1),
      "\nOrders: ", percent(order_share, accuracy = 0.1)
    ),
    volume_bucket = factor(volume_bucket, levels = c("Low Volume", "High Volume")),
    value_bucket = factor(value_bucket, levels = c("Low Value", "High Value"))
  ) %>%
  ggplot(aes(x = volume_bucket, y = value_bucket, fill = revenue_share)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = label), color = "#1f2933", size = 3.8, lineheight = 0.95) +
  scale_fill_gradient(low = "#f3efe7", high = "#276b62", labels = percent) +
  labs(
    title = "Strategic Category Matrix: Volume vs. Value",
    subtitle = "Buckets are split by median delivered orders and median revenue per order",
    x = NULL,
    y = NULL,
    fill = "Revenue share"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray35"),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold", size = 11),
    axis.title = element_blank(),
    legend.position = "right",
    plot.margin = margin(10, 15, 10, 10)
  )

plot_product_pareto <- top_products_ranked %>%
  ggplot(aes(x = product_rank, y = cumulative_sales_share)) +
  geom_line(color = "#276b62", linewidth = 1) +
  geom_hline(yintercept = c(0.5, 0.8), linetype = "dashed", color = "#8a8a8a", linewidth = 0.5) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Product demand has a long-tail pattern",
    subtitle = "Cumulative share of delivered units by product rank",
    x = "Products ranked by units sold",
    y = "Cumulative share of units sold"
  ) +
  theme_minimal(base_size = 12)

plot_category_share <- top_categories_ranked %>%
  slice_head(n = 12) %>%
  mutate(
    category_label = fct_reorder(category_label, sales_share),
    share_label = percent(sales_share, accuracy = 0.1)
  ) %>%
  ggplot(aes(x = sales_share, y = category_label)) +
  geom_col(fill = "#6d5f8d", width = 0.74) +
  geom_text(aes(label = share_label), hjust = -0.12, size = 3.1) +
  scale_x_continuous(labels = percent) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Category share of delivered units",
    subtitle = "Top 12 categories only",
    x = "Share of units sold",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(10, 35, 10, 10))

plot_top_categories
plot_top_products
plot_least_categories
plot_strategic_matrix
plot_product_pareto
plot_category_share
plot_kmeans_elbow
plot_pam_silhouette
plot_kmeans_clusters
plot_pam_clusters

# 7. Optional exports -------------------------------------------------------

dir.create("outputs", showWarnings = FALSE)

write_csv(top_products, "outputs/top_products.csv")
write_csv(top_categories, "outputs/top_categories.csv")
write_csv(least_purchased_products, "outputs/least_purchased_products.csv")
write_csv(least_purchased_categories, "outputs/least_purchased_categories.csv")
write_csv(unsold_products, "outputs/unsold_products.csv")
write_csv(no_delivered_sales_products, "outputs/no_delivered_sales_products.csv")
write_csv(strategic_bucket_table, "outputs/strategic_bucket_table.csv")
write_csv(category_perf_clustered, "outputs/category_perf_clustered.csv")
write_csv(pam_cluster_profile, "outputs/pam_cluster_profile.csv")

ggsave("outputs/top_categories.png", plot_top_categories, width = 9, height = 6, dpi = 300)
ggsave("outputs/top_products.png", plot_top_products, width = 9, height = 6, dpi = 300)
ggsave("outputs/least_categories.png", plot_least_categories, width = 9, height = 6, dpi = 300)
ggsave("outputs/strategic_matrix.png", plot_strategic_matrix, width = 8.5, height = 6, dpi = 300)
ggsave("outputs/product_pareto.png", plot_product_pareto, width = 8, height = 6, dpi = 300)
ggsave("outputs/category_share.png", plot_category_share, width = 8, height = 6, dpi = 300)
ggsave("outputs/kmeans_elbow.png", plot_kmeans_elbow, width = 8, height = 5, dpi = 300)
ggsave("outputs/pam_silhouette.png", plot_pam_silhouette, width = 8, height = 5, dpi = 300)
ggsave("outputs/kmeans_clusters.png", plot_kmeans_clusters, width = 8, height = 6, dpi = 300)
ggsave("outputs/pam_clusters.png", plot_pam_clusters, width = 8, height = 6, dpi = 300)
