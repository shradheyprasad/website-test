library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(htmlwidgets)
library(stringr)

# ============================================================================
# STORAGE-ONLY CAPACITY STACKED BAR CHART
# Uses capacity_mwh (energy), shown in GWh
# Battery / Pumped Storage / Hydrogen only
# ============================================================================

# ============================================================================
# DATA IMPORT
# ============================================================================

# Set working directory so you don't need full paths
setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")

# Capacity file
# Expected columns:
# period, technology, scenario, status (existing/new),
# capacity_mw (power), capacity_mwh (energy)
capacity_file <- "capacity/cost-grouped_capacity.csv"

cat("Reading capacity data...\n")
capacity_raw <- read_csv(capacity_file)

# ============================================================================
# TECHNOLOGY ORDER AND COLORS
# ============================================================================

# Full tech order from your main plot (kept for stacking order consistency)
tech_order <- c(
  "Coal",
  "Gas",
  "Diesel",
  "Nuclear",
  "Other",
  "Hydro",
  "Solar",
  "Wind",
  "Pumped Storage",
  "Battery",
  "Hydrogen",
  "Transmission Losses"
)

# Storage subset
storage_techs <- c("Battery", "Pumped Storage", "Hydrogen")

# Define colors for each technology (reuse palette from main plot)
tech_colors <- c(
  "Coal" = "#343a40",
  "Gas" = "#6c757d",
  "Diesel" = "#924B00",
  "Nuclear" = "#800080",
  "Hydro" = "#2a648a",
  "Solar" = "#ef9226",
  "Wind" = "#8dc0cd",
  "Battery" = "#f4e04d",
  "Pumped Storage" = "#6a96ac",
  "Hydrogen" = "teal",
  "Transmission Losses" = "#656d4a",
  "Other" = "#999999"
)

cat("\n=== Storage Technologies (plotted) ===\n")
for (i in seq_along(storage_techs)) {
  cat(sprintf("%2d. %s\n", i, storage_techs[i]))
}
cat("\n")

# ============================================================================
# SCENARIO NAME MAPPING AND ORDER
# Must mirror the main capacity plot so scenarios align visually
# ============================================================================

scenario_mapping <- tibble(
  original = c(
    "VRE & ESS (low) Coal (low)",
    "VRE & ESS (low) Coal (high)",
    "VRE & ESS (mid) Coal (low) - REF",
    "VRE & ESS (mid) Coal (high)",
    "VRE & ESS (high) Coal (low)",
    "VRE & ESS (high) Coal (high)"
  ),
  short = c(
    "VRE & ESS (low) Coal (low)",
    "VRE & ESS (low) Coal (high)",
    "VRE & ESS (mid) Coal (low) - REF",
    "VRE & ESS (mid) Coal (high)",
    "VRE & ESS (high) Coal (low)",
    "VRE & ESS (high) Coal (high)"
  )
)

desired_scenario_order <- c(
  "VRE & ESS (low) Coal (low)",
  "VRE & ESS (low) Coal (high)",
  "VRE & ESS (mid) Coal (low) - REF",
  "VRE & ESS (mid) Coal (high)",
  "VRE & ESS (high) Coal (low)",
  "VRE & ESS (high) Coal (high)"
)

cat("\n=== Scenario Name Mapping (storage plot) ===\n")
print(scenario_mapping)
cat("\n")

# ============================================================================
# PREP DATA (filter to storage only, use capacity_mwh)
# ============================================================================

cat("Preparing storage capacity data...\n")

capacity_df <- capacity_raw %>%
  # convert MWh -> GWh for readability
  mutate(capacity_gwh = capacity_mwh / 1e3) %>%
  # attach human-readable scenario label
  left_join(scenario_mapping, by = c("scenario" = "original")) %>%
  mutate(
    scenario_display = ifelse(!is.na(short), short, scenario)
  ) %>%
  select(-short) %>%
  # keep only storage technologies
  filter(technology %in% storage_techs)

# enforce scenario left→right order
capacity_df <- capacity_df %>%
  mutate(
    scenario_display = factor(scenario_display, levels = desired_scenario_order)
  )

# aggregate to period × scenario_display × technology × status
capacity_df <- capacity_df %>%
  group_by(period, scenario_display, technology, status) %>%
  summarise(capacity_gwh = sum(capacity_gwh, na.rm = TRUE), .groups = "drop")

# enforce technology factor order for stacking consistency
capacity_df$technology <- factor(capacity_df$technology, levels = tech_order)

# figure out axes
periods <- sort(unique(capacity_df$period))
scenarios <- levels(capacity_df$scenario_display)

cat("Periods:", paste(periods, collapse = ", "), "\n")
cat("Scenarios:", paste(scenarios, collapse = ", "), "\n")
cat("Technologies in storage plot:", paste(unique(capacity_df$technology), collapse = ", "), "\n\n")

n_periods <- length(periods)
n_scenarios <- length(scenarios)

# bar geometry (matches main figure logic)
bar_width <- 0.9 / n_scenarios

# compute x positions for each (period, scenario_display)
x_positions <- data.frame()
for (i_period in seq_len(n_periods)) {
  period_val <- periods[i_period]
  x_base <- i_period - 1  # cluster index for that period
  
  for (i_scen in seq_len(n_scenarios)) {
    scen_val <- scenarios[i_scen]
    x_pos <- x_base + (i_scen - 1) * bar_width
    
    x_positions <- rbind(
      x_positions,
      data.frame(
        period = period_val,
        scenario_display = scen_val,
        x_position = x_pos,
        period_index = i_period - 1,
        scenario_index = i_scen - 1
      )
    )
  }
}

# attach x positions
capacity_df <- capacity_df %>%
  left_join(x_positions, by = c("period", "scenario_display"))

if (any(is.na(capacity_df$x_position))) {
  warning("Some (period, scenario_display) pairs did not get x_position in storage plot.")
}

# ============================================================================
# STACKING HELPERS
# (same logic you used for GW, now applied to GWh)
# ============================================================================

# Technologies that actually appear in this filtered dataset,
# kept in the global order for consistent vertical stacking.
technologies <- tech_order[tech_order %in% unique(as.character(capacity_df$technology))]

cat("Technologies included in STORAGE plot (bottom to top):", paste(technologies, collapse = ", "), "\n\n")

# bottom for 'existing' layer = sum of existing for prior (lower) techs
compute_bottom_existing <- function(df_all, tech_list, tech_current, full_df) {
  tech_index <- which(tech_list == tech_current)
  if (tech_index <= 1) {
    return(rep(0, nrow(df_all)))
  }
  prev_tecs <- tech_list[1:(tech_index - 1)]
  
  bottoms <- numeric(nrow(df_all))
  for (i in seq_len(nrow(df_all))) {
    p_i <- df_all$period[i]
    s_i <- df_all$scenario_display[i]
    
    sum_prev <- full_df %>%
      filter(
        period == p_i,
        scenario_display == s_i,
        technology %in% prev_tecs,
        status == "existing"
      ) %>%
      summarise(total = sum(capacity_gwh, na.rm = TRUE)) %>%
      pull(total)
    
    if (length(sum_prev) == 0) sum_prev <- 0
    bottoms[i] <- sum_prev
  }
  bottoms
}

# bottom for 'new' layer = total existing (all techs) + new from lower techs
compute_bottom_new <- function(df_all, tech_list, tech_current, full_df) {
  tech_index <- which(tech_list == tech_current)
  prev_tecs <- if (tech_index > 1) tech_list[1:(tech_index - 1)] else character(0)
  
  bottoms <- numeric(nrow(df_all))
  for (i in seq_len(nrow(df_all))) {
    p_i <- df_all$period[i]
    s_i <- df_all$scenario_display[i]
    
    base_existing <- full_df %>%
      filter(
        period == p_i,
        scenario_display == s_i,
        status == "existing"
      ) %>%
      summarise(total = sum(capacity_gwh, na.rm = TRUE)) %>%
      pull(total)
    if (length(base_existing) == 0) base_existing <- 0
    
    base_new_prev <- full_df %>%
      filter(
        period == p_i,
        scenario_display == s_i,
        technology %in% prev_tecs,
        status == "new"
      ) %>%
      summarise(total = sum(capacity_gwh, na.rm = TRUE)) %>%
      pull(total)
    if (length(base_new_prev) == 0) base_new_prev <- 0
    
    bottoms[i] <- base_existing + base_new_prev
  }
  bottoms
}

# ============================================================================
# BUILD STORAGE-ONLY FIGURE
# ============================================================================
fig_storage <- plot_ly()

# Pass 1: "existing" layer (hatched fill)
for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- capacity_df %>%
    filter(technology == tech, status == "existing") %>%
    arrange(period, scenario_display)
  
  if (nrow(tech_data) > 0 && any(tech_data$capacity_gwh > 0, na.rm = TRUE)) {
    tech_data$bottom <- compute_bottom_existing(
      df_all        = tech_data,
      tech_list     = technologies,
      tech_current  = tech,
      full_df       = capacity_df
    )
    
    hover_text <- paste0(
      "<b>", tech, " (Existing)</b><br>",
      "Period: ", tech_data$period, "<br>",
      "Scenario: ", tech_data$scenario_display, "<br>",
      "Storage Capacity: ", sprintf("%.2f", tech_data$capacity_gwh), " GWh"
    )
    
    fig_storage <- fig_storage %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$capacity_gwh,
        name = tech,
        legendgroup = tech,
        showlegend = FALSE,
        marker = list(
          color = tech_color,
          pattern = list(
            shape = "/",
            bgcolor = tech_color,
            fgcolor = "white",
            size = 8,
            solidity = 0.3
          ),
          line = list(color = "white", width = 0.5)
        ),
        width = bar_width,
        base = tech_data$bottom,
        text = hover_text,
        hoverinfo = "text",
        textposition = "none"
      )
  }
}

# Pass 2: "new" layer (solid fill, drives legend)
for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- capacity_df %>%
    filter(technology == tech, status == "new") %>%
    arrange(period, scenario_display)
  
  if (nrow(tech_data) > 0 && any(tech_data$capacity_gwh > 0, na.rm = TRUE)) {
    tech_data$bottom <- compute_bottom_new(
      df_all        = tech_data,
      tech_list     = technologies,
      tech_current  = tech,
      full_df       = capacity_df
    )
    
    hover_text <- paste0(
      "<b>", tech, " (New)</b><br>",
      "Period: ", tech_data$period, "<br>",
      "Scenario: ", tech_data$scenario_display, "<br>",
      "Storage Capacity: ", sprintf("%.2f", tech_data$capacity_gwh), " GWh"
    )
    
    fig_storage <- fig_storage %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$capacity_gwh,
        name = tech,
        legendgroup = tech,
        showlegend = TRUE,  # only the "new" layer shows up in legend
        marker = list(
          color = tech_color,
          line = list(color = "white", width = 0.5)
        ),
        width = bar_width,
        base = tech_data$bottom,
        text = hover_text,
        hoverinfo = "text",
        textposition = "none"
      )
  }
}

# ============================================================================
# LAYOUT (STORAGE PLOT)
# ============================================================================

# x ticks at the center of each period group
x_tick_positions <- x_positions %>%
  group_by(period) %>%
  summarise(x_center = mean(x_position), .groups = "drop")

fig_storage <- fig_storage %>%
  layout(
    barmode = "stack",
    title = list(
      text = "Existing & New Storage Capacity by Period and Scenario",
      font = list(size = 18, family = "Arial")
    ),
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = x_tick_positions$x_center,
      ticktext = x_tick_positions$period,
      tickfont = list(size = 14),
      showgrid = FALSE,
      range = c(
        -bar_width / 2,
        (n_periods - 1) + (n_scenarios * bar_width) + bar_width / 2
      )
    ),
    yaxis = list(
      title = "Existing & New Storage Capacity (GWh)",
      titlefont = list(size = 16),
      tickfont = list(size = 12),
      showgrid = TRUE,
      gridcolor = "lightgray",
      zeroline = TRUE
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    legend = list(
      title = list(text = "<b>Technology</b>", font = list(size = 14)),
      font = list(size = 11),
      x = 1.02,
      y = 1,
      xanchor = "left",
      yanchor = "top"
    ),
    margin = list(l = 80, r = 200, t = 80, b = 80)
  )

# ============================================================================
# SHOW AND SAVE
# ============================================================================

fig_storage

output_storage_path <- "/Users/shradheyprasad/Desktop/emLab/repo/website-test/storage_capacity_by_period.html"
saveWidget(fig_storage, output_storage_path, selfcontained = TRUE)

