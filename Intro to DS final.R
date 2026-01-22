# ============================================================
# 1. Load required packages
# ============================================================

install.packages(c("tidyverse","readODS","janitor","cluster","scales","ggplot2"))

library(tidyverse)   
library(readODS)     
library(janitor)     
library(cluster)     
library(scales)     
library(ggplot2)


# ============================================================
# 2. Import ORR passenger journeys and kilometres datasets
# ============================================================

passenger_journeys_raw <- read_ods("table-1223-passenger-journeys-by-operator.ods", sheet = 3)
passenger_kilometres_raw <- read_ods("table-1233-passenger-kilometres-by-operator.ods", sheet = 3)


# ============================================================
# 3. Cleaning: fix headers + remove metadata rows
# ============================================================

# ---- Journeys table ----
header_row_j <- match(TRUE, str_detect(as.character(passenger_journeys_raw[[1]]), "Time\\s*period"))
if (is.na(header_row_j)) stop("Header row with 'Time period' not found in passenger_journeys_raw")

journeys_fixed <- passenger_journeys_raw
names(journeys_fixed) <- as.character(journeys_fixed[header_row_j, ])     
journeys_fixed <- journeys_fixed[-(1:header_row_j), ] %>%               
  clean_names() %>%                                                      
  rename(time_period = 1) %>%                                            
  filter(str_detect(as.character(time_period), "to"))                    
#Ensuring time period is the header of the row in Journeys table 

# ---- Kilometres table ----
header_row_k <- match(TRUE, str_detect(as.character(passenger_kilometres_raw[[1]]), "Time\\s*period"))
if (is.na(header_row_k)) stop("Header row with 'Time period' not found in passenger_kilometres_raw")

km_fixed <- passenger_kilometres_raw
names(km_fixed) <- as.character(km_fixed[header_row_k, ])
km_fixed <- km_fixed[-(1:header_row_k), ] %>%
  clean_names() %>%
  rename(time_period = 1) %>%
  filter(str_detect(as.character(time_period), "to"))
#Ensuring time period is the header of the row in Kilometres table 


# ===================================================================================
# 4. Clean numeric columns (removing special characters like commas and footnotes )
# ===================================================================================

clean_numeric <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("", ".", "..")] <- NA           
  x <- gsub(",", "", x)                      
  x <- gsub("\\[.*?\\]", "", x)             
  as.numeric(x)                 
}

journeys_1223b <- journeys_fixed %>%
  mutate(across(-time_period, clean_numeric))

kilometres_1233b <- km_fixed %>%
  mutate(across(-time_period, clean_numeric))


# ============================================================
# 5. Convert to long format for analysis and plotting
# ============================================================

journeys_long_format <- journeys_1223b %>%
  pivot_longer(-time_period, names_to = "operator", values_to = "journeys")

kilometres_long_format <- kilometres_1233b %>%
  pivot_longer(-time_period, names_to = "operator", values_to = "kilometres")

# Extract operator mapping from the true header row
operator_lookup <- data.frame(
  code = paste0("X", seq_along(names(journeys_1223b)[-1])),
  operator = names(journeys_1223b)[-1],
  stringsAsFactors = FALSE
)

operator_lookup

write.csv(operator_lookup, "operator_lookup.csv", row.names = FALSE)


# ============================================================
# 6. Heatmap of passenger journeys over time
# ============================================================

last_n_quarters <- 20       #Showing 20 quarters

period_levels <- unique(journeys_long$time_period)
recent_levels <- tail(period_levels, last_n_quarters)

op_order <- journeys_long %>%       #Ordering operators by average journeys
  group_by(operator) %>%
  summarise(avg_journeys = mean(journeys, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_journeys)) %>%
  pull(operator)

journeys_heatmap <- journeys_long %>%
  filter(time_period %in% recent_levels) %>%
  mutate(
    time_period = factor(time_period, levels = recent_levels),
    operator = factor(operator, levels = op_order),
    operator_pretty = str_replace_all(as.character(operator), "_", " ") %>%
      str_to_title() %>%
      str_replace_all(" Million.*$", "") %>%     
      str_replace_all(" Note \\d+$", "") %>%      #Removing suffixes 
      str_squish()
  )

ggplot(journeys_heatmap, aes(x = time_period, y = operator_pretty, fill = journeys)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", na.value = "grey90") +
  labs(
    title = "Passenger Journeys Heatmap by Operator (Recent Quarters)",
    x = "Time Period",
    y = "Operator",
    fill = "Journeys"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8)
  )


# ============================================================
# 7. Calculating volatility (SD) of journeys
# ============================================================

journeys_volatility <- journeys_long_format %>%
  group_by(operator) %>%
  summarise(journeys_volatility = sd(journeys, na.rm = TRUE))

journeys_volatility

write.csv(
  journeys_volatility,
  file = "journeys_volatility_by_operator.csv",  #Saving as CSV file
  row.names = FALSE
)


# ============================================================
# 8. Volatility bar chart
# ============================================================

options(repr.plot.width = 12, repr.plot.height = 8)

journeys_volatility_plot <- journeys_volatility %>%
  mutate(
    operator_pretty = operator %>%
      str_replace_all("_", " ") %>%
      str_remove_all(" million.*$") %>%
      str_remove_all(" note \\d+$") %>%
      str_to_title() %>%
      stringr::str_wrap(width = 28)
  )

ggplot(journeys_volatility_plot,                    # Using journeys_volatility object created above.
       aes(x = reorder(operator_pretty, journeys_volatility),
           y = journeys_volatility)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Volatility in Passenger Journeys by Operator",
    x = "Operator",
    y = "Standard Deviation (Volatility)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 8),     # y-axis labels (operators) after flip
    plot.margin = margin(10, 10, 10, 30)      # extra left space so labels aren’t cut
  )


# ============================================================
# 9. K-means clustering of operators by volatility
# ============================================================

vol_data <- journeys_volatility %>% select(journeys_volatility)
row.names(vol_data) <- journeys_volatility$operator

set.seed(123)
clusters <- kmeans(vol_data, centers = 3).  #Clustering operators into 3 groups based on SD

journeys_volatility$cluster <- as.factor(clusters$cluster)

operator_volatility_cluster <- data.frame(
  operator = rownames(vol_data),
  volatility = vol_data$journeys_volatility,
  cluster = clusters$cluster
)

operator_volatility_cluster %>%
  arrange(cluster, desc(volatility))

operator_cluster_clean <- operator_volatility_cluster %>%
  mutate(
    operator = operator %>%
      str_replace_all("_", " ") %>%
      str_remove_all(" million.*$") %>%
      str_remove_all(" note \\d+$") %>%
      str_to_title()
  )

write.csv(
  operator_cluster_clean,
  file = "operator_clustering_by_volatility.csv",
  row.names = FALSE
)


# ============================================================
# 10. Clustering visualisation results
# ============================================================

options(repr.plot.width = 12, repr.plot.height = 8)

journeys_volatility_plot <- journeys_volatility %>%
  mutate(
    operator_pretty = operator %>%
      str_replace_all("_", " ") %>%
      str_remove_all(" million.*$") %>%
      str_remove_all(" note \\d+$") %>%
      str_to_title() %>%
      stringr::str_wrap(width = 28)
  )

ggplot(journeys_volatility_plot,
       aes(x = reorder(operator_pretty, journeys_volatility), y = journeys_volatility, colour = cluster))+
  geom_point(size = 4) +
  coord_flip() +
  labs(
    title = "Clustering Operators by Demand Volatility",
    x = "Operator",
    y = "Volatility (SD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 8),     # operator labels 
    plot.margin = margin(10, 10, 10, 30)
  )


# ============================================================
# 11. Save plots
# ============================================================

ggsave("journeys_heatmap.png", width = 10, height = 6, dpi = 300)
ggsave("journeys_volatility.png", width = 8, height = 10, dpi = 300)
ggsave("journeys_clusters.png", width = 8, height = 6, dpi = 300)
