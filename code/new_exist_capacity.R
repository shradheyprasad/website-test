library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(htmlwidgets)
library(stringr)

# ============================================================================
# CAPACITY STACKED BAR CHART - Matching Python matplotlib style
# ============================================================================

# ============================================================================
# DATA IMPORT
# ============================================================================

# Set working directory so you don't need full paths
setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")

# Capacity file
# Expected columns (long format):
# period, technology, scenario, status (existing/new), capacity_mw
capacity_file <- "capacity/cost-grouped_capacity.csv"

cat("Reading capacity data...\n")
capacity_raw <- read_csv(capacity_file)

# ============================================================================
# TECHNOLOGY ORDER AND COLORS
# ============================================================================

# Define technology order (bottom to top in stacked bar)
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

# Define colors for each technology
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

cat("\n=== Technology Stacking Order (bottom to top) ===\n")
for (i in seq_along(tech_order)) {
  cat(sprintf("%2d. %s\n", i, tech_order[i]))
}
cat("\n")

# ============================================================================
# SCENARIO NAME MAPPING AND ORDER
# ============================================================================
# 1. Map long scenario codes in your data to readable labels
#    Left column = value in your CSV in `scenario`
#    Right column = what you want to show in the plot
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

# 2. Desired left→right order for scenarios inside each period group
desired_scenario_order <- c(
  "VRE & ESS (low) Coal (low)",
  "VRE & ESS (low) Coal (high)",
  "VRE & ESS (mid) Coal (low) - REF",
  "VRE & ESS (mid) Coal (high)",
  "VRE & ESS (high) Coal (low)",
  "VRE & ESS (high) Coal (high)"
)

# ============================================================================
# PREP DATA FOR STACKED GROUPED BAR PLOT
# ============================================================================

cat("Preparing capacity data...\n")

capacity_df <- capacity_raw %>%
  # MW -> GW for readability
  mutate(capacity_gw = capacity_mw / 1e3) %>%
  # attach nice scenario label
  left_join(scenario_mapping, by = c("scenario" = "original")) %>%
  mutate(
    scenario_display = ifelse(!is.na(short), short, scenario)
  ) %>%
  select(-short)

# force scenario_display to factor with fixed plotting order
capacity_df <- capacity_df %>%
  mutate(
    scenario_display = factor(scenario_display, levels = desired_scenario_order)
  )

# aggregate to period × scenario_display × technology × status
capacity_df <- capacity_df %>%
  group_by(period, scenario_display, technology, status) %>%
  summarise(capacity_gw = sum(capacity_gw, na.rm = TRUE), .groups = "drop")

# make sure technology respects stacking order
capacity_df$technology <- factor(capacity_df$technology, levels = tech_order)

# figure out plot axes
periods <- sort(unique(capacity_df$period))
scenarios <- levels(capacity_df$scenario_display)

cat("Periods:", paste(periods, collapse = ", "), "\n")
cat("Scenarios:", paste(scenarios, collapse = ", "), "\n")
cat("Technologies present:", paste(unique(capacity_df$technology), collapse = ", "), "\n\n")

n_periods <- length(periods)
n_scenarios <- length(scenarios)

# Bar geometry:
# - each period is one cluster centered at integer indices 0,1,2,...
# - within each cluster, we place 1 bar per scenario across that space.
bar_width <- 0.9 / n_scenarios  # take ~90% of slot for full cluster

# assign x positions for each (period, scenario_display)
x_positions <- data.frame()
for (i_period in seq_len(n_periods)) {
  period_val <- periods[i_period]
  x_base <- i_period - 1  # cluster index (0-based like in Python loops)
  
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

# join x positions back to data
capacity_df <- capacity_df %>%
  left_join(x_positions, by = c("period", "scenario_display"))

# warn if any missing x positions (shouldn't happen if mapping covered all)
if (any(is.na(capacity_df$x_position))) {
  warning("Some (period, scenario_display) pairs were not assigned x positions.")
}

# ============================================================================
# STACKING HELPERS
# ============================================================================

# We'll draw each bar in two passes per technology:
#   1. existing (hatched)
#   2. new (solid)
# We have to compute the "base" (bottom height) manually.

# technologies that actually show up in data, in defined stack order
technologies <- tech_order[tech_order %in% unique(as.character(capacity_df$technology))]
cat("Technologies included in plot (bottom to top):", paste(technologies, collapse = ", "), "\n\n")

# base for 'existing': sum of 'existing' for all technologies below it
compute_bottom_existing <- function(df_all, tech_list, tech_current, full_df) {
  tech_index <- which(tech_list == tech_current)
  if (tech_index <= 1) {
    return(rep(0, nrow(df_all)))
  }
  prev_tecs <- tech_list[1:(tech_index-1)]
  
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
      summarise(total = sum(capacity_gw, na.rm = TRUE)) %>%
      pull(total)
    if (length(sum_prev) == 0) sum_prev <- 0
    bottoms[i] <- sum_prev
  }
  bottoms
}

# base for 'new':
#   all existing from all techs
# + new from technologies below this one
compute_bottom_new <- function(df_all, tech_list, tech_current, full_df) {
  tech_index <- which(tech_list == tech_current)
  prev_tecs <- if (tech_index > 1) tech_list[1:(tech_index-1)] else character(0)
  
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
      summarise(total = sum(capacity_gw, na.rm = TRUE)) %>%
      pull(total)
    if (length(base_existing) == 0) base_existing <- 0
    
    base_new_prev <- full_df %>%
      filter(
        period == p_i,
        scenario_display == s_i,
        technology %in% prev_tecs,
        status == "new"
      ) %>%
      summarise(total = sum(capacity_gw, na.rm = TRUE)) %>%
      pull(total)
    if (length(base_new_prev) == 0) base_new_prev <- 0
    
    bottoms[i] <- base_existing + base_new_prev
  }
  bottoms
}

# ============================================================================
# BUILD FIGURE
# ============================================================================

fig <- plot_ly()

# Pass 1: draw "existing" layer for each technology with hatch pattern
for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- capacity_df %>%
    filter(technology == tech, status == "existing") %>%
    arrange(period, scenario_display)
  
  if (nrow(tech_data) > 0 && any(tech_data$capacity_gw > 0, na.rm = TRUE)) {
    tech_data$bottom <- compute_bottom_existing(
      df_all        = tech_data,
      tech_list     = technologies,
      tech_current  = tech,
      full_df       = capacity_df
    )
    
    # Build hover text for each segment
    hover_text <- paste0(
      "<b>", tech, " (Existing)</b><br>",
      "Period: ", tech_data$period, "<br>",
      "Scenario: ", tech_data$scenario_display, "<br>",
      "Capacity: ", sprintf("%.2f", tech_data$capacity_gw), " GW"
    )
    
    fig <- fig %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$capacity_gw,
        name = tech,
        legendgroup = tech,
        showlegend = FALSE,  # legend will come from "new" layer
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
        
        # hover handling
        text = hover_text,
        hoverinfo = "text",
        textposition = "none"  # do NOT draw text on the bar itself
      )
  }
}


# Pass 2: draw "new" layer for each technology in solid color with legend
for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- capacity_df %>%
    filter(technology == tech, status == "new") %>%
    arrange(period, scenario_display)
  
  if (nrow(tech_data) > 0 && any(tech_data$capacity_gw > 0, na.rm = TRUE)) {
    tech_data$bottom <- compute_bottom_new(
      df_all        = tech_data,
      tech_list     = technologies,
      tech_current  = tech,
      full_df       = capacity_df
    )
    
    # Build hover text for each segment
    hover_text <- paste0(
      "<b>", tech, " (New)</b><br>",
      "Period: ", tech_data$period, "<br>",
      "Scenario: ", tech_data$scenario_display, "<br>",
      "Capacity: ", sprintf("%.2f", tech_data$capacity_gw), " GW"
    )
    
    fig <- fig %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$capacity_gw,
        name = tech,
        legendgroup = tech,
        showlegend = TRUE,   # show each tech once in legend (solid fill)
        marker = list(
          color = tech_color,
          line = list(color = "white", width = 0.5)
        ),
        width = bar_width,
        base = tech_data$bottom,
        
        # hover handling
        text = hover_text,
        hoverinfo = "text",
        textposition = "none"  # hide labels on the bars
      )
  }
}

# ============================================================================
# AXIS TICKS AND LAYOUT
# ============================================================================

# Put xticks at the center of each period's group (average x_position of that period)
x_tick_positions <- x_positions %>%
  group_by(period) %>%
  summarise(x_center = mean(x_position), .groups = "drop")

fig <- fig %>%
  layout(
    barmode = "stack",
    title = list(
      text = "Existing & New Capacity by Period and Scenario",
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
        -bar_width/2,
        (n_periods - 1) + (n_scenarios * bar_width) + bar_width/2
      )
    ),
    yaxis = list(
      title = "Existing & New Capacity (GW)",
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

fig

output_path <- "/Users/shradheyprasad/Desktop/emLab/repo/website-test/capacity_by_period.html"
saveWidget(fig, output_path, selfcontained = TRUE)

