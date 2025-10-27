library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(htmlwidgets)
library(stringr)

# ============================================================================
# DISPATCH / GENERATION STACKED BAR CHART (TWh)
# period-clustered, scenario side-by-side, tech-stacked
# Handles positive and negative generation like Python _plot_dispatch
# ============================================================================

# ============================================================================
# DATA IMPORT
# ============================================================================

# Working directory for consistency
setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")

# Dispatch file
# Expected columns in the CSV:
# period, technology, load_zone, scenario, power_mw, load_zone_abr, region
dispatch_file <- "dispatch/cost-grouped_dispatch.csv"

cat("Reading dispatch data...\n")
dispatch_raw <- read_csv(dispatch_file)

# ============================================================================
# TECHNOLOGY ORDER AND COLORS
# (same palette and order you're using elsewhere)
# ============================================================================

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

cat("\n=== Technology Stacking / Drawing Order (bottom to top) ===\n")
for (i in seq_along(tech_order)) {
  cat(sprintf("%2d. %s\n", i, tech_order[i]))
}
cat("\n")

# ============================================================================
# SCENARIO NAME MAPPING AND ORDER
# Must match your other plots
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

cat("\n=== Scenario Name Mapping (dispatch plot) ===\n")
print(scenario_mapping)
cat("\n")

# ============================================================================
# PREP DATA
# Steps:
# - Sum across load zones
# - Convert MW -> TWh (power_mw / 1e6)
# - Attach readable scenario labels
# - Aggregate to period × scenario × technology
# ============================================================================

cat("Preparing dispatch generation data...\n")

dispatch_df <- dispatch_raw %>%
  # convert to TWh-equivalent scale as in Python (_plot_dispatch uses units=1e6)
  mutate(power_twh = power_mw / 1e6) %>%
  # attach shorter scenario labels
  left_join(scenario_mapping, by = c("scenario" = "original")) %>%
  mutate(
    scenario_display = ifelse(!is.na(short), short, scenario)
  ) %>%
  select(-short)

# enforce global scenario order
dispatch_df <- dispatch_df %>%
  mutate(
    scenario_display = factor(scenario_display, levels = desired_scenario_order)
  )

# aggregate to period × scenario_display × technology
# sum across load_zone, region, etc.
dispatch_df <- dispatch_df %>%
  group_by(period, scenario_display, technology) %>%
  summarise(power_twh = sum(power_twh, na.rm = TRUE), .groups = "drop")

# enforce technology order for stacking and legend
dispatch_df$technology <- factor(dispatch_df$technology, levels = tech_order)

# figure out axis layout info
periods <- sort(unique(dispatch_df$period))
scenarios <- levels(dispatch_df$scenario_display)

cat("Periods:", paste(periods, collapse = ", "), "\n")
cat("Scenarios (ordered):", paste(scenarios, collapse = ", "), "\n")
cat("Technologies present:", paste(unique(dispatch_df$technology), collapse = ", "), "\n\n")

n_periods <- length(periods)
n_scenarios <- length(scenarios)

# We'll replicate how you spaced bars in the capacity plots:
# Each period is a cluster (index 0,1,2,...), and inside each period there are bars
# for each scenario left-to-right in desired_scenario_order.

bar_width <- 0.9 / n_scenarios

x_positions <- data.frame()
for (i_period in seq_len(n_periods)) {
  period_val <- periods[i_period]
  x_base <- i_period - 1  # cluster index (0-based)
  
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

dispatch_df <- dispatch_df %>%
  left_join(x_positions, by = c("period", "scenario_display"))

if (any(is.na(dispatch_df$x_position))) {
  warning("Some (period, scenario_display) rows did not get x_position.")
}

# ============================================================================
# STACKING HELPERS FOR DISPATCH
#
# This mimics the Python _plot_dispatch logic:
# - We loop over technologies in tech_order.
# - For each period-scenario pair, we split into positive stack (>=0)
#   and negative stack (<0).
# - The 'base' for a positive tech is the sum of previous positive techs.
# - The 'base' for a negative tech is the sum of previous negative techs
#   (which will be <= 0).
#
# We compute per-row base before adding traces.
# ============================================================================

compute_bottom_dispatch <- function(df_all, tech_list, tech_current, full_df) {
  tech_index <- which(tech_list == tech_current)
  # techs that come before current one in drawing order
  prev_tecs <- if (tech_index > 1) tech_list[1:(tech_index - 1)] else character(0)
  
  bottoms <- numeric(nrow(df_all))
  
  for (i in seq_len(nrow(df_all))) {
    p_i <- df_all$period[i]
    s_i <- df_all$scenario_display[i]
    current_val <- df_all$power_twh[i]
    
    # get all previous tech rows for the same period-scenario
    prev_rows <- full_df %>%
      filter(
        period == p_i,
        scenario_display == s_i,
        technology %in% prev_tecs
      )
    
    if (current_val >= 0) {
      # stack on top of prior positives
      base_val <- prev_rows %>%
        filter(power_twh >= 0) %>%
        summarise(total = sum(power_twh, na.rm = TRUE)) %>%
        pull(total)
      if (length(base_val) == 0) base_val <- 0
      bottoms[i] <- base_val
    } else {
      # stack downward under prior negatives
      base_val <- prev_rows %>%
        filter(power_twh < 0) %>%
        summarise(total = sum(power_twh, na.rm = TRUE)) %>%
        pull(total)
      if (length(base_val) == 0) base_val <- 0
      bottoms[i] <- base_val
    }
  }
  
  bottoms
}

# ============================================================================
# BUILD FIGURE
# We'll add one trace per technology. Each trace will:
#   - draw both positive and negative contributions for that tech
#   - have its own color
#   - set base manually for each bar segment
# Hover shows Period, Scenario, Technology, TWh.
# ============================================================================

fig_dispatch <- plot_ly()

# Use technologies that actually exist in the dispatch dataset, in tech_order
technologies <- tech_order[tech_order %in% unique(as.character(dispatch_df$technology))]

cat("Technologies included in DISPATCH plot (draw order):", paste(technologies, collapse = ", "), "\n\n")

for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- dispatch_df %>%
    filter(technology == tech) %>%
    arrange(period, scenario_display)
  
  if (nrow(tech_data) > 0 && any(tech_data$power_twh != 0, na.rm = TRUE)) {
    # compute bases for this technology across all period/scenario bars
    tech_data$bottom <- compute_bottom_dispatch(
      df_all       = tech_data,
      tech_list    = technologies,
      tech_current = tech,
      full_df      = dispatch_df
    )
    
    hover_text <- paste0(
      "<b>", tech, "</b><br>",
      "Period: ", tech_data$period, "<br>",
      "Scenario: ", tech_data$scenario_display, "<br>",
      "Generation: ", sprintf("%.2f", tech_data$power_twh), " TWh"
    )
    
    fig_dispatch <- fig_dispatch %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$power_twh,
        name = tech,
        legendgroup = tech,
        showlegend = TRUE,
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
# LAYOUT (FIXED barmode)
# Tick locations are centered per period cluster (average x_position for that period)
# ============================================================================

x_tick_positions <- x_positions %>%
  group_by(period) %>%
  summarise(x_center = mean(x_position), .groups = "drop")

fig_dispatch <- fig_dispatch %>%
  layout(
    # IMPORTANT: prevent Plotly from doing its own stacking
    barmode = "overlay",
    
    title = list(
      text = "Electricity Generation by Period and Scenario",
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
      title = "Electricity Generation (TWh)",
      titlefont = list(size = 16),
      tickfont = list(size = 12),
      showgrid = TRUE,
      gridcolor = "lightgray",
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 0.5
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
    margin = list(l = 80, r = 200, t = 80, b = 80),
    shapes = list(
      # horizontal 0 line under everything
      list(
        type = "line",
        x0 = -1,
        x1 = (n_periods - 1) + (n_scenarios * bar_width) + 1,
        y0 = 0,
        y1 = 0,
        line = list(color = "black", width = 0.5)
      )
    )
  )

# show / save (unchanged)
fig_dispatch

output_dispatch_path <- "/Users/shradheyprasad/Desktop/emLab/repo/website-test/dispatch_generation_by_period.html"
saveWidget(fig_dispatch, output_dispatch_path, selfcontained = TRUE)

