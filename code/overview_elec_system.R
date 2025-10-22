library(ggplot2)
library(scatterpie)
library(ggiraph)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggforce)

setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/input_data/electricity_system")

tx_ <- read_csv("input_data-electricity_system_lines.csv")
load_zones_ <- read_csv("input_data-electricity_system_load_zones.csv")
group_cap_ <- read_csv("input_data-electricity_system_groups.csv")
total_cap_ <- read_csv("input_data-electricity_system_capacity.csv")
india_sf <- st_read("/Users/shradheyprasad/Desktop/emLab/wri-india/website/input_data/map/india/india-polygon.shp")

# CHECK THE DATA STRUCTURE
cat("Original group_cap_ structure:\n")
print(head(group_cap_, 20))
cat("\nNumber of rows per zone/tech:\n")
print(group_cap_ %>% count(load_zone, technology) %>% filter(n > 1))

# AGGREGATE - sum all capacity by load_zone AND technology
group_cap_clean <- group_cap_ %>%
  group_by(load_zone, technology) %>%
  summarise(capacity_mw = sum(capacity_mw, na.rm = TRUE), .groups = "drop") %>%
  filter(capacity_mw > 0)  # Remove zeros

cat("\nCleaned group_cap_clean:\n")
print(head(group_cap_clean, 20))
cat("\nShould be 1 row per zone/tech:\n")
print(group_cap_clean %>% count(load_zone, technology) %>% filter(n > 1))

calculate_radius <- function(total_mw) {
  return(sqrt(sqrt(sqrt(total_mw / 1000))) / 2.5)
}

tech_colors <- c(
  "Coal" = "#343a40", "Gas" = "#6c757d", "Diesel" = "#924B00",
  "Solar" = "#ef9226", "Wind" = "#8dc0cd", "Hydro" = "#2a648a",
  "Other" = "#6ba661", "Pumped Storage" = "#6a96ac", "Nuclear" = "#800080"
)

# Use the CLEANED data
cap_wide <- group_cap_clean %>%
  pivot_wider(names_from = technology, values_from = capacity_mw, values_fill = 0)

pie_data <- load_zones_ %>%
  inner_join(total_cap_, by = "load_zone") %>%
  inner_join(cap_wide, by = "load_zone") %>%
  mutate(radius = calculate_radius(capacity_mw))

tech_cols <- names(tech_colors)[names(tech_colors) %in% names(pie_data)]

# Create tooltip - use CLEANED data
pie_data <- pie_data %>%
  rowwise() %>%
  mutate(
    tooltip = {
      # Use THIS row's load_zone value
      this_zone <- load_zone
      
      # Filter for THIS specific zone only
      cap_info <- group_cap_clean %>% 
        filter(load_zone == this_zone) %>% 
        arrange(desc(capacity_mw))
      
      paste0(
        "<b style='font-size:14px'>", load_zone_abr, "</b><br>",
        "Total: <b>", format(round(capacity_mw), big.mark = ","), " MW</b><br><br>",
        "<b>Capacity by Technology:</b><br>",
        paste("<b>", cap_info$technology, ":</b> ", 
              format(round(cap_info$capacity_mw), big.mark = ","), " MW",
              collapse = "<br>")
      )
    },
    data_id = load_zone
  ) %>%
  ungroup()

# Create circle polygons for interactive overlay
create_circle_polygon <- function(x, y, r, n = 50) {
  theta <- seq(0, 2*pi, length.out = n)
  data.frame(
    x = x + r * cos(theta),
    y = y + r * sin(theta)
  )
}

circle_data <- do.call(rbind, lapply(1:nrow(pie_data), function(i) {
  circle <- create_circle_polygon(
    pie_data$longitude[i], 
    pie_data$latitude[i], 
    pie_data$radius[i] * 1.1
  )
  circle$zone_id <- pie_data$load_zone[i]
  circle$tooltip <- pie_data$tooltip[i]
  circle$data_id <- pie_data$data_id[i]
  return(circle)
}))

tx_segments <- tx_ %>%
  left_join(load_zones_ %>% select(load_zone, lon_from = longitude, lat_from = latitude),
           by = c("load_zone_from" = "load_zone")) %>%
  left_join(load_zones_ %>% select(load_zone, lon_to = longitude, lat_to = latitude),
           by = c("load_zone_to" = "load_zone")) %>%
  filter(!is.na(lon_from) & !is.na(lon_to)) %>%
  mutate(
    capacity_category = case_when(
      max_mw < 1000 ~ "50 - 1,000",
      max_mw >= 1000 & max_mw < 2500 ~ "1,001 - 2,500",
      max_mw >= 2500 & max_mw < 5000 ~ "2,501 - 5,000",
      max_mw >= 5000 & max_mw < 10000 ~ "5,001 - 10,000",
      max_mw >= 10000 ~ "10,001 - 20,993"
    ),
    line_width = case_when(
      max_mw < 1000 ~ 0.5,
      max_mw >= 1000 & max_mw < 2500 ~ 1.0,
      max_mw >= 2500 & max_mw < 5000 ~ 2.0,
      max_mw >= 5000 & max_mw < 10000 ~ 4.0,
      max_mw >= 10000 ~ 6.0
    ),
    tooltip = paste0(
      "<b>Transmission Line</b><br>",
      "Capacity: ", format(round(max_mw), big.mark = ","), " MW<br>",
      "From: ", load_zone_from, "<br>",
      "To: ", load_zone_to
    ),
    data_id = paste0("line_", row_number())
  )

tx_segments$capacity_category <- factor(
  tx_segments$capacity_category,
  levels = c("50 - 1,000", "1,001 - 2,500", "2,501 - 5,000", "5,001 - 10,000", "10,001 - 20,993")
)

map_bounds <- st_bbox(india_sf)
legend_x <- map_bounds["xmax"] - 3
legend_y_base <- (map_bounds["ymax"] + map_bounds["ymin"]) / 2

capacity_legend_circles <- data.frame(
  capacity_gw = c(40, 1, 0.01),
  capacity_mw = c(40000, 1000, 10)
)
capacity_legend_circles$radius <- calculate_radius(capacity_legend_circles$capacity_mw)
capacity_legend_circles$x <- legend_x
capacity_legend_circles$y <- legend_y_base - 2

p <- ggplot() +
  geom_sf(data = india_sf, fill = "#d3d3d3", color = "white", size = 0.5) +
  geom_segment_interactive(
    data = tx_segments,
    aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,
        color = capacity_category, size = line_width,
        tooltip = tooltip, data_id = data_id),
    alpha = 0.6, lineend = "round"
  ) +
  geom_scatterpie(
    data = pie_data,
    aes(x = longitude, y = latitude, r = radius),
    cols = tech_cols, color = "black", size = 0.2, alpha = 0.9
  ) +
  geom_polygon_interactive(
    data = circle_data,
    aes(x = x, y = y, group = zone_id, 
        tooltip = tooltip, data_id = data_id),
    fill = "transparent",
    color = NA,
    alpha = 0
  ) +
  geom_text(
    data = pie_data,
    aes(
      x = longitude + case_when(
        position == 0 ~ -radius * 1.8, position == 1 ~ radius * 1.8,
        position == 2 ~ 0, position == 3 ~ 0, TRUE ~ 0
      ),
      y = latitude + case_when(
        position == 0 ~ 0, position == 1 ~ 0,
        position == 2 ~ -radius * 1.8, position == 3 ~ radius * 1.8, TRUE ~ 0
      ),
      label = load_zone_abr
    ),
    size = 5.5, fontface = "bold", color = "black"
  ) +
  geom_circle(
    data = capacity_legend_circles,
    aes(x0 = x, y0 = y - 1.5, r = radius),
    fill = "white", color = "black", size = 0.3, alpha = 0.8
  ) +
  annotate("text", x = legend_x, y = legend_y_base - 1.75,
           label = "Existing Capacity", size = 4.5, fontface = "bold", hjust = 0.5) +
  annotate("text", x = legend_x, y = legend_y_base - 2.5,
           label = "0.01/1/40 (GW)", size = 4, hjust = 0.5) +
  scale_color_manual(
    name = "Existing Transmission (MW)",
    values = c("50 - 1,000" = "#95d5d2", "1,001 - 2,500" = "#6ec9c5",
               "2,501 - 5,000" = "#4abdb8", "5,001 - 10,000" = "#2cb7b5",
               "10,001 - 20,993" = "#1a9b99"),
    guide = guide_legend(override.aes = list(size = c(0.5, 1.0, 2.0, 4.0, 6.0)))
  ) +
  scale_size_identity() +
  scale_fill_manual(
    name = "Resources", values = tech_colors, breaks = tech_cols,
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold")
  )

interactive_plot <- girafe(
  ggobj = p,
  width_svg = 14,
  height_svg = 15,
  options = list(
    opts_hover(css = ""),
    opts_tooltip(
      css = "background-color:white;color:black;padding:12px;border-radius:5px;box-shadow:0 0 15px rgba(0,0,0,0.4);font-family:Arial;font-size:13px;line-height:1.5;",
      opacity = 1
    ),
    opts_zoom(max = 5)
  )
)

interactive_plot

library(htmlwidgets)
htmltools::save_html(interactive_plot, "/Users/shradheyprasad/Desktop/emLab/repo/website-test/india_2020.html")
