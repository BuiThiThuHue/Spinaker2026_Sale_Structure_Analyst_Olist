# Olist E-commerce EDA converted from data_eda_2.ipynb

library(tidyverse)
library(lubridate)
library(scales)

get_project_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_file <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(active_file) && nzchar(active_file)) return(dirname(active_file))
  }
  getwd()
}

theme_exec <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3),
      plot.subtitle = element_text(color = "gray35"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

project_dir <- get_project_dir()
data_dir <- file.path(project_dir, "olist dataset")

if (!dir.exists(data_dir)) {
  stop("Data folder not found: ", data_dir)
}

customers <- read_csv(file.path(data_dir, "olist_customers_dataset.csv"), show_col_types = FALSE)
geolocation <- read_csv(file.path(data_dir, "olist_geolocation_dataset.csv"), show_col_types = FALSE)
order_items <- read_csv(file.path(data_dir, "olist_order_items_dataset.csv"), show_col_types = FALSE)
payments <- read_csv(file.path(data_dir, "olist_order_payments_dataset.csv"), show_col_types = FALSE)
reviews <- read_csv(file.path(data_dir, "olist_order_reviews_dataset.csv"), show_col_types = FALSE)
orders <- read_csv(file.path(data_dir, "olist_orders_dataset.csv"), show_col_types = FALSE)
products <- read_csv(file.path(data_dir, "olist_products_dataset.csv"), show_col_types = FALSE)
sellers <- read_csv(file.path(data_dir, "olist_sellers_dataset.csv"), show_col_types = FALSE)
translations <- read_csv(file.path(data_dir, "product_category_name_translation.csv"), show_col_types = FALSE)

dir.create("outputs_notebook_r", showWarnings = FALSE)

# 1. Core joined table -------------------------------------------------------

item_sales_data <- order_items %>%
  left_join(products, by = "product_id") %>%
  left_join(translations, by = "product_category_name") %>%
  left_join(orders, by = "order_id") %>%
  mutate(
    category = coalesce(product_category_name_english, product_category_name, "unknown"),
    clean_category = str_to_title(str_replace_all(category, "_", " ")),
    order_purchase_timestamp = ymd_hms(order_purchase_timestamp),
    order_delivered_customer_date = ymd_hms(order_delivered_customer_date),
    order_estimated_delivery_date = ymd_hms(order_estimated_delivery_date)
  )

clean_reviews <- reviews %>%
  filter(!is.na(review_score)) %>%
  group_by(order_id) %>%
  summarise(review_score = mean(review_score), .groups = "drop")

# 2. Top categories and operational status ---------------------------------

top_20_volume <- item_sales_data %>%
  filter(!is.na(product_category_name_english)) %>%
  count(category, clean_category, name = "total_purchased", sort = TRUE) %>%
  slice_head(n = 20)

top_20_status_analysis <- item_sales_data %>%
  semi_join(top_20_volume, by = c("category", "clean_category")) %>%
  group_by(category, clean_category) %>%
  summarise(
    total_purchased = n(),
    delivered_count = sum(order_status == "delivered", na.rm = TRUE),
    failed_count = sum(order_status %in% c("canceled", "unavailable"), na.rm = TRUE),
    failure_rate_pct = failed_count / total_purchased,
    .groups = "drop"
  ) %>%
  arrange(desc(total_purchased))

plot_top_20_volume <- top_20_status_analysis %>%
  mutate(clean_category = fct_reorder(clean_category, total_purchased)) %>%
  ggplot(aes(x = total_purchased, y = clean_category)) +
  geom_col(fill = "#276b62", width = 0.72) +
  geom_text(aes(label = comma(total_purchased)), hjust = -0.12, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.14))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Top 20 Most Purchased Categories",
    subtitle = "By total order-item rows",
    x = "Items purchased",
    y = NULL
  ) +
  theme_exec()

plot_failure_rate <- top_20_status_analysis %>%
  mutate(clean_category = fct_reorder(clean_category, failure_rate_pct)) %>%
  ggplot(aes(x = failure_rate_pct, y = clean_category)) +
  geom_col(fill = "#a65345", width = 0.72) +
  geom_text(aes(label = percent(failure_rate_pct, accuracy = 0.1)), hjust = -0.12, size = 3) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.18))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Cancellation and Unavailable Rate",
    subtitle = "Failure rate among top 20 categories",
    x = "Failure rate",
    y = NULL
  ) +
  theme_exec()

# 3. Review sentiment -------------------------------------------------------

sentiment_colors <- c(
  "Positive (4-5 Stars)" = "#4CAF50",
  "Neutral (3 Stars)" = "#9E9E9E",
  "Negative (1-2 Stars)" = "#F44336"
)

sentiment_analysis <- item_sales_data %>%
  semi_join(top_20_volume, by = c("category", "clean_category")) %>%
  inner_join(clean_reviews, by = "order_id") %>%
  mutate(sentiment = case_when(
    review_score >= 4 ~ "Positive (4-5 Stars)",
    review_score >= 2.5 ~ "Neutral (3 Stars)",
    TRUE ~ "Negative (1-2 Stars)"
  )) %>%
  count(clean_category, sentiment, name = "review_count") %>%
  group_by(clean_category) %>%
  mutate(percent = review_count / sum(review_count)) %>%
  ungroup()

plot_sentiment_top20 <- sentiment_analysis %>%
  ggplot(aes(x = fct_reorder(clean_category, percent), y = percent, fill = sentiment)) +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = sentiment_colors) +
  labs(
    title = "Review Sentiment by Top 20 Categories",
    subtitle = "Deduplicated order reviews",
    x = NULL,
    y = "Share of reviews",
    fill = "Sentiment"
  ) +
  theme_exec()

# 4. Geography --------------------------------------------------------------

geo_analysis <- item_sales_data %>%
  semi_join(top_20_volume, by = c("category", "clean_category")) %>%
  left_join(customers, by = "customer_id") %>%
  count(clean_category, customer_state, name = "items_sold") %>%
  group_by(clean_category) %>%
  mutate(percent_of_category = items_sold / sum(items_sold)) %>%
  ungroup()

plot_geo_heatmap <- geo_analysis %>%
  ggplot(aes(x = customer_state, y = clean_category, fill = percent_of_category)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#ebf4f6", high = "#08306b", labels = percent) +
  labs(
    title = "Geographic Concentration of Top Categories",
    subtitle = "Share of category sales by Brazilian state",
    x = "Customer state",
    y = NULL,
    fill = "% of category"
  ) +
  theme_exec() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

# 5. Macro segments, revenue and freight -----------------------------------

segment_data <- item_sales_data %>%
  filter(!is.na(product_category_name_english)) %>%
  mutate(macro_segment = case_when(
    category %in% c("bed_bath_table", "furniture_decor", "housewares", "home_confort", "office_furniture", "home_construction") ~ "Home & Furniture",
    category %in% c("health_beauty", "perfumery", "fashion_bags_accessories", "watches_gifts", "luggage_accessories") ~ "Fashion & Beauty",
    category %in% c("computers_accessories", "telephony", "electronics", "consoles_games", "pc_gamer", "audio") ~ "Tech & Electronics",
    category %in% c("sports_leisure", "auto", "garden_tools", "toys", "musical_instruments") ~ "Sports, Auto & Leisure",
    category %in% c("baby", "stationery", "pet_shop", "books_general_interest") ~ "Family, Pets & Hobbies",
    TRUE ~ "Other/Miscellaneous"
  )) %>%
  count(macro_segment, name = "total_items_sold", sort = TRUE) %>%
  mutate(market_share_pct = total_items_sold / sum(total_items_sold))

plot_macro_segments <- segment_data %>%
  mutate(macro_segment = fct_reorder(macro_segment, total_items_sold)) %>%
  ggplot(aes(x = total_items_sold, y = macro_segment)) +
  geom_col(fill = "#2CA02C", width = 0.72) +
  geom_text(aes(label = percent(market_share_pct, accuracy = 0.1)), hjust = -0.12, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.14))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Macro-Segment Market Share",
    subtitle = "Category groups converted into high-level departments",
    x = "Items sold",
    y = NULL
  ) +
  theme_exec()

revenue_data <- item_sales_data %>%
  filter(!is.na(product_category_name_english)) %>%
  group_by(category, clean_category) %>%
  summarise(
    total_revenue = sum(price, na.rm = TRUE),
    total_volume = n(),
    average_price = mean(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 20)

plot_revenue_volume <- revenue_data %>%
  ggplot(aes(x = total_volume, y = total_revenue)) +
  geom_point(color = "#276b62", size = 3.5, alpha = 0.75) +
  geom_text(aes(label = clean_category), vjust = -0.9, size = 3, check_overlap = TRUE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ")) +
  labs(
    title = "Top Revenue Categories: Revenue vs. Volume",
    subtitle = "Separates volume drivers from value-heavy categories",
    x = "Items sold",
    y = "Item revenue"
  ) +
  theme_exec()

logistics_analysis <- item_sales_data %>%
  semi_join(top_20_volume, by = c("category", "clean_category")) %>%
  group_by(category, clean_category) %>%
  summarise(
    total_product_revenue = sum(price, na.rm = TRUE),
    total_freight_cost = sum(freight_value, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE),
    avg_freight = mean(freight_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(freight_burden_pct = total_freight_cost / total_product_revenue) %>%
  arrange(desc(freight_burden_pct))

plot_freight_burden <- logistics_analysis %>%
  mutate(clean_category = fct_reorder(clean_category, freight_burden_pct)) %>%
  ggplot(aes(x = freight_burden_pct, y = clean_category)) +
  geom_col(fill = "#c48a2c", width = 0.72) +
  geom_text(aes(label = percent(freight_burden_pct, accuracy = 0.1)), hjust = -0.12, size = 3) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.16))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Logistics Friction by Category",
    subtitle = "Freight cost as a share of item price",
    x = "Freight burden",
    y = NULL
  ) +
  theme_exec()

# 6. Top vs bottom diagnostics ---------------------------------------------

all_volume <- item_sales_data %>%
  filter(!is.na(product_category_name_english)) %>%
  count(category, clean_category, name = "total_purchased", sort = TRUE)

target_categories <- bind_rows(
  all_volume %>% slice_head(n = 20) %>% mutate(performance_group = "Top 20 Categories"),
  all_volume %>% slice_tail(n = 20) %>% mutate(performance_group = "Bottom 20 Categories")
)

review_comparison <- item_sales_data %>%
  inner_join(target_categories, by = c("category", "clean_category")) %>%
  inner_join(clean_reviews, by = "order_id") %>%
  mutate(sentiment = case_when(
    review_score >= 4 ~ "Positive (4-5 Stars)",
    review_score >= 2.5 ~ "Neutral (3 Stars)",
    TRUE ~ "Negative (1-2 Stars)"
  )) %>%
  count(clean_category, performance_group, sentiment, name = "review_count") %>%
  group_by(clean_category, performance_group) %>%
  mutate(percent = review_count / sum(review_count)) %>%
  ungroup()

negative_sort <- review_comparison %>%
  filter(sentiment == "Negative (1-2 Stars)") %>%
  select(clean_category, negative_pct = percent)

review_comparison <- review_comparison %>%
  left_join(negative_sort, by = "clean_category") %>%
  mutate(negative_pct = replace_na(negative_pct, 0))

plot_review_comparison <- review_comparison %>%
  ggplot(aes(x = fct_reorder(clean_category, negative_pct), y = percent, fill = sentiment)) +
  geom_col(width = 0.72) +
  coord_flip() +
  facet_wrap(~ performance_group, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = sentiment_colors) +
  labs(
    title = "Review Sentiment: Top 20 vs. Bottom 20 Categories",
    subtitle = "Sorted by negative review share",
    x = NULL,
    y = "Share of reviews",
    fill = "Sentiment"
  ) +
  theme_exec() +
  theme(axis.text.y = element_text(size = 7.5), panel.spacing = unit(2, "lines"))

delivery_trends <- item_sales_data %>%
  inner_join(target_categories, by = c("category", "clean_category")) %>%
  filter(order_status == "delivered", !is.na(order_delivered_customer_date), !is.na(order_purchase_timestamp)) %>%
  mutate(delivery_days = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days"))) %>%
  filter(delivery_days >= 0, delivery_days <= 100)

plot_delivery_box <- delivery_trends %>%
  ggplot(aes(x = fct_reorder(clean_category, delivery_days, .fun = median), y = delivery_days, fill = performance_group)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3, alpha = 0.75) +
  coord_flip() +
  facet_wrap(~ performance_group, scales = "free_y") +
  scale_fill_manual(values = c("Top 20 Categories" = "#1F77B4", "Bottom 20 Categories" = "#FF7F0E")) +
  labs(
    title = "Actual Delivery Time Distribution",
    subtitle = "Top vs. bottom categories",
    x = NULL,
    y = "Days to deliver"
  ) +
  theme_exec() +
  theme(legend.position = "none", axis.text.y = element_text(size = 7.5))

friction_distribution <- item_sales_data %>%
  inner_join(target_categories, by = c("category", "clean_category")) %>%
  filter(!is.na(price), price > 0, !is.na(freight_value)) %>%
  mutate(individual_freight_ratio = freight_value / price) %>%
  filter(individual_freight_ratio <= 1.5)

plot_freight_violin <- friction_distribution %>%
  ggplot(aes(x = fct_reorder(clean_category, individual_freight_ratio, .fun = median), y = individual_freight_ratio, fill = performance_group)) +
  geom_violin(scale = "width", alpha = 0.75, color = "#444444") +
  coord_flip() +
  facet_wrap(~ performance_group, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Top 20 Categories" = "#2CA02C", "Bottom 20 Categories" = "#9467BD")) +
  labs(
    title = "Freight Friction Distribution",
    subtitle = "Freight cost relative to item price",
    x = NULL,
    y = "Freight / price"
  ) +
  theme_exec() +
  theme(legend.position = "none", axis.text.y = element_text(size = 7.5))

# 7. Correlation, sellers and seasonality -----------------------------------

statistical_summary <- item_sales_data %>%
  inner_join(target_categories, by = c("category", "clean_category")) %>%
  inner_join(clean_reviews, by = "order_id") %>%
  filter(!is.na(order_delivered_customer_date), !is.na(order_purchase_timestamp), !is.na(price), price > 0) %>%
  mutate(
    delivery_days = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days")),
    freight_ratio = freight_value / price
  ) %>%
  select(review_score, delivery_days, freight_ratio) %>%
  drop_na()

cor_matrix <- cor(statistical_summary, method = "spearman")

seller_accountability <- item_sales_data %>%
  inner_join(target_categories %>% filter(performance_group == "Bottom 20 Categories"), by = c("category", "clean_category")) %>%
  filter(!is.na(order_delivered_customer_date), !is.na(order_purchase_timestamp)) %>%
  mutate(delivery_days = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days"))) %>%
  group_by(clean_category, seller_id) %>%
  summarise(
    total_items_sold = n(),
    avg_delivery_days = mean(delivery_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_items_sold > 10) %>%
  arrange(desc(avg_delivery_days))

seasonality_trends <- item_sales_data %>%
  filter(!is.na(order_delivered_customer_date), !is.na(order_purchase_timestamp)) %>%
  mutate(
    purchase_month = floor_date(order_purchase_timestamp, "month"),
    delivery_days = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days"))
  ) %>%
  group_by(purchase_month) %>%
  summarise(
    total_orders = n(),
    avg_delivery_time = mean(delivery_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_orders > 100)

plot_seasonality <- seasonality_trends %>%
  ggplot(aes(x = purchase_month)) +
  geom_col(aes(y = total_orders), fill = "#E0E0E0", alpha = 0.75) +
  geom_line(aes(y = rescale(avg_delivery_time, to = range(total_orders))), color = "#D32F2F", linewidth = 1.1) +
  geom_point(aes(y = rescale(avg_delivery_time, to = range(total_orders))), color = "#D32F2F", size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Supply Chain Stress Test",
    subtitle = "Order volume bars with scaled average delivery-time line",
    x = "Purchase month",
    y = "Order volume"
  ) +
  theme_exec() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sentiment_over_time <- item_sales_data %>%
  filter(!is.na(order_purchase_timestamp)) %>%
  inner_join(clean_reviews, by = "order_id") %>%
  mutate(
    purchase_month = floor_date(order_purchase_timestamp, "month"),
    is_negative = review_score <= 2
  ) %>%
  group_by(purchase_month) %>%
  summarise(
    total_reviews = n(),
    pct_negative = mean(is_negative),
    .groups = "drop"
  ) %>%
  filter(total_reviews > 100)

plot_sentiment_time <- sentiment_over_time %>%
  ggplot(aes(x = purchase_month, y = pct_negative)) +
  annotate("rect", xmin = as.Date("2017-11-01"), xmax = as.Date("2018-03-01"), ymin = -Inf, ymax = Inf, alpha = 0.18, fill = "orange") +
  geom_line(color = "#F44336", linewidth = 1.1) +
  geom_point(color = "#F44336", size = 2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Negative Review Trend Over Time",
    subtitle = "Highlighted window: Nov 2017 to Mar 2018",
    x = "Purchase month",
    y = "Negative review share"
  ) +
  theme_exec() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. Transaction clustering -------------------------------------------------

if (requireNamespace("cluster", quietly = TRUE)) {
  ml_data <- item_sales_data %>%
    filter(!is.na(price), !is.na(freight_value), !is.na(order_delivered_customer_date), !is.na(order_purchase_timestamp)) %>%
    mutate(delivery_days = as.numeric(difftime(order_delivered_customer_date, order_purchase_timestamp, units = "days"))) %>%
    select(order_id, price, freight_value, delivery_days) %>%
    filter(price < 1000, freight_value < 100, delivery_days > 0, delivery_days < 60) %>%
    drop_na()

  scaled_features <- ml_data %>%
    select(price, freight_value, delivery_days) %>%
    scale()

  set.seed(42)
  kmeans_model <- kmeans(scaled_features, centers = 3, nstart = 25)

  ml_data <- ml_data %>% mutate(cluster_id = factor(kmeans_model$cluster))

  cluster_summary <- ml_data %>%
    group_by(cluster_id) %>%
    summarise(
      total_sales = n(),
      avg_price = mean(price),
      avg_freight = mean(freight_value),
      avg_delivery_days = mean(delivery_days),
      freight_burden_pct = mean(freight_value) / mean(price),
      .groups = "drop"
    )

  plot_kmeans_transactions <- ml_data %>%
    ggplot(aes(x = price, y = freight_value, color = cluster_id)) +
    geom_point(alpha = 0.35, size = 1.2) +
    scale_color_manual(values = c("1" = "#4CAF50", "2" = "#FFC107", "3" = "#F44336")) +
    labs(
      title = "K-means Transaction Segmentation",
      subtitle = "Operational profiles based on price, freight and delivery days",
      x = "Product price (R$)",
      y = "Freight cost (R$)",
      color = "Cluster"
    ) +
    theme_exec()
}

# 9. Product-level demand ---------------------------------------------------

product_transactions <- products %>%
  left_join(translations, by = "product_category_name") %>%
  left_join(order_items, by = "product_id") %>%
  left_join(orders %>% select(order_id, customer_id, order_status), by = "order_id") %>%
  left_join(customers %>% select(customer_id, customer_unique_id), by = "customer_id")

product_level <- product_transactions %>%
  group_by(product_id, product_category_name, product_category_name_english) %>%
  summarise(
    total_units_sold = sum(!is.na(order_id)),
    total_revenue = sum(replace_na(price, 0), na.rm = TRUE),
    avg_price = if_else(total_units_sold > 0, total_revenue / total_units_sold, 0),
    unique_customers = n_distinct(customer_unique_id[!is.na(customer_unique_id)]),
    total_orders = n_distinct(order_id[!is.na(order_id)]),
    .groups = "drop"
  ) %>%
  mutate(
    product_category = coalesce(product_category_name_english, product_category_name, "unknown"),
    product_category = str_to_title(str_replace_all(product_category, "_", " ")),
    clean_product_label = paste0(str_trunc(product_category, 28), "\n", str_sub(product_id, 1, 8))
  )

most_purchased_products <- product_level %>%
  arrange(desc(total_units_sold), desc(unique_customers), desc(total_revenue)) %>%
  slice_head(n = 20)

least_purchased_products <- product_level %>%
  arrange(total_units_sold, unique_customers, total_orders, total_revenue) %>%
  slice_head(n = 20)

plot_top_products <- most_purchased_products %>%
  mutate(clean_product_label = fct_reorder(clean_product_label, total_units_sold)) %>%
  ggplot(aes(x = total_units_sold, y = clean_product_label)) +
  geom_col(fill = "#1F77B4", width = 0.72) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 20 Most Often Purchased Products",
    x = "Units sold",
    y = NULL
  ) +
  theme_exec()

plot_weak_products <- least_purchased_products %>%
  mutate(clean_product_label = fct_reorder(clean_product_label, total_revenue)) %>%
  ggplot(aes(x = total_revenue, y = clean_product_label)) +
  geom_col(fill = "#D62728", width = 0.72) +
  scale_x_continuous(labels = label_dollar(prefix = "R$ ")) +
  labs(
    title = "Products With Weakest Observed Demand",
    x = "Revenue",
    y = NULL
  ) +
  theme_exec()

# 10. Print and export ------------------------------------------------------

top_20_status_analysis
sentiment_analysis
segment_data
revenue_data
logistics_analysis
cor_matrix
head(seller_accountability, 20)
most_purchased_products
least_purchased_products
if (exists("cluster_summary")) cluster_summary

plot_top_20_volume
plot_failure_rate
plot_sentiment_top20
plot_geo_heatmap
plot_macro_segments
plot_revenue_volume
plot_freight_burden
plot_review_comparison
plot_delivery_box
plot_freight_violin
plot_seasonality
plot_sentiment_time
if (exists("plot_kmeans_transactions")) plot_kmeans_transactions
plot_top_products
plot_weak_products

write_csv(top_20_status_analysis, "outputs_notebook_r/top_20_status_analysis.csv")
write_csv(segment_data, "outputs_notebook_r/macro_segment_share.csv")
write_csv(revenue_data, "outputs_notebook_r/top_20_revenue_categories.csv")
write_csv(logistics_analysis, "outputs_notebook_r/freight_burden_top20.csv")
write_csv(seller_accountability, "outputs_notebook_r/bottom_category_seller_accountability.csv")
write_csv(most_purchased_products, "outputs_notebook_r/most_purchased_products.csv")
write_csv(least_purchased_products, "outputs_notebook_r/least_purchased_products.csv")
if (exists("cluster_summary")) write_csv(cluster_summary, "outputs_notebook_r/transaction_cluster_summary.csv")

ggsave("outputs_notebook_r/top_20_volume.png", plot_top_20_volume, width = 9, height = 6, dpi = 300)
ggsave("outputs_notebook_r/failure_rate.png", plot_failure_rate, width = 9, height = 6, dpi = 300)
ggsave("outputs_notebook_r/sentiment_top20.png", plot_sentiment_top20, width = 10, height = 7, dpi = 300)
ggsave("outputs_notebook_r/geographic_heatmap.png", plot_geo_heatmap, width = 11, height = 7, dpi = 300)
ggsave("outputs_notebook_r/macro_segments.png", plot_macro_segments, width = 9, height = 5, dpi = 300)
ggsave("outputs_notebook_r/revenue_volume.png", plot_revenue_volume, width = 9, height = 6, dpi = 300)
ggsave("outputs_notebook_r/freight_burden.png", plot_freight_burden, width = 9, height = 6, dpi = 300)
ggsave("outputs_notebook_r/review_comparison.png", plot_review_comparison, width = 11, height = 8, dpi = 300)
ggsave("outputs_notebook_r/delivery_boxplot.png", plot_delivery_box, width = 11, height = 8, dpi = 300)
ggsave("outputs_notebook_r/freight_violin.png", plot_freight_violin, width = 11, height = 8, dpi = 300)
ggsave("outputs_notebook_r/seasonality.png", plot_seasonality, width = 11, height = 6, dpi = 300)
ggsave("outputs_notebook_r/sentiment_time.png", plot_sentiment_time, width = 11, height = 6, dpi = 300)
if (exists("plot_kmeans_transactions")) ggsave("outputs_notebook_r/kmeans_transactions.png", plot_kmeans_transactions, width = 10, height = 7, dpi = 300)
ggsave("outputs_notebook_r/top_products.png", plot_top_products, width = 10, height = 7, dpi = 300)
ggsave("outputs_notebook_r/weak_products.png", plot_weak_products, width = 10, height = 7, dpi = 300)
