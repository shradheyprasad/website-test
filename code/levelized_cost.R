library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(htmlwidgets)

# ============================================================================
# LEVELIZED FIXED AND VARIABLE COST BAR CHART - Matching Python matplotlib
# ============================================================================

setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")

# ============================================================================
# DATA IMPORT
# ============================================================================

# Technology costs file (df_1_ in Python)
cost_file <- "technology_cost/cost-technology_costs.csv"

# Dispatch file - we'll extract load from this (df_2_ in Python) 
dispatch_file <- "dispatch/cost-grouped_dispatch.csv"

cat("Reading cost data...\n")
cost_data <- read_csv(cost_file)

cat("Reading dispatch data...\n")
dispatch_data <- read_csv(dispatch_file)

cat("\nCost data columns:\n")
print(names(cost_data))
cat("\nDispatch data columns:\n")
print(names(dispatch_data))
cat("\n")

# ============================================================================
# TECHNOLOGY COLORS AND ORDER
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

# ============================================================================
# PARAMETERS
# ============================================================================

USD_to_INR <- 72
units <- 1  # No unit conversion for now
y_lim_max <- 70

# ============================================================================
# SCENARIO SETUP AND AGGREGATION
# ============================================================================

# Scenario mapping: cost data uses long names, dispatch uses short names
scenario_mapping <- data.frame(
  original = c(
    "VRElow_STlow_CONVmid_H2_RES_8PRM_CC_50RPS_90CAP_500GW_PIERmid",
    "VRElow_STlow_CONVhigh_H2_RES_8PRM_CC_50RPS_90CAP_500GW_PIERmid",
    "VREmid_STmid_CONVmid_H2_RES_8PRM_CC_50RPS_90CAP_500GW_PIERmid",
    "VREmid_STmid_CONVhigh_H2_RES_8PRM_CC_50RPS_90CAP_500GW_PIERmid",
    "VREhigh_SThigh_CONVmid_H2_RES_8PRM_CC_50RPS_90CAP_500GW_PIERmid",
    "VREhigh_SThigh_CONVhigh_H2_RES_8PRM_CC_50RPS_90CAP_500GW_PIERmid"
  ),
  short = c(
    "VRE & ESS (low) Coal (low)",
    "VRE & ESS (low) Coal (high)",
    "VRE & ESS (mid) Coal (low) - REF",
    "VRE & ESS (mid) Coal (high)",
    "VRE & ESS (high) Coal (low)",
    "VRE & ESS (high) Coal (high)"
  ),
  stringsAsFactors = FALSE
)

# Map cost data from long to short names
cost_data <- cost_data %>%
  left_join(scenario_mapping, by = c("scenario" = "original")) %>%
  mutate(scenario_display = ifelse(!is.na(short), short, scenario)) %>%
  select(-short)

cat("Cost data scenarios after mapping:\n")
print(unique(cost_data$scenario_display))

# Dispatch data already has short names
dispatch_data <- dispatch_data %>%
  mutate(scenario_display = scenario)

cat("Dispatch data scenarios:\n")
print(unique(dispatch_data$scenario_display))
cat("\n")

# Aggregate cost data across load zones if needed
if ("load_zone" %in% names(cost_data)) {
  cat("Aggregating cost data across load zones...\n")
  cost_data <- cost_data %>%
    group_by(scenario, scenario_display, period, technology) %>%
    summarise(
      variable_cost = sum(variable_cost, na.rm = TRUE),
      fixed_cost = sum(fixed_cost, na.rm = TRUE),
      .groups = "drop"
    )
}

# Calculate load from dispatch data
# Load = sum of power_mw across all technologies for each scenario+period
cat("Calculating load from dispatch data (sum of power_mw across technologies)...\n")
load_data <- dispatch_data %>%
  mutate(scenario_display = scenario) %>%
  group_by(scenario, scenario_display, period) %>%
  summarise(load_mw = sum(power_mw, na.rm = TRUE), .groups = "drop")

cat("\nCost data rows:", nrow(cost_data), "\n")
cat("Load data rows:", nrow(load_data), "\n")
cat("Sample load values:\n")
print(head(load_data))
cat("\n")

# ============================================================================
# DATA PREPARATION
# ============================================================================

# Detect technology column
tech_col <- if ("technology" %in% names(cost_data)) {
  "technology"
} else {
  names(cost_data)[grep("tech", names(cost_data), ignore.case = TRUE)][1]
}

cat("Using technology column:", tech_col, "\n\n")

# Get unique values from each dataset
cat("=== DATA COMPARISON ===\n")
cat("\nCOST DATA:\n")
periods_cost <- sort(unique(cost_data$period))
scenarios_cost <- sort(unique(cost_data$scenario_display))
techs_cost <- sort(unique(cost_data[[tech_col]]))

cat("Periods:", paste(periods_cost, collapse = ", "), "\n")
cat("Scenarios:\n")
for (s in scenarios_cost) cat("  - ", s, "\n")
cat("Technologies:", paste(techs_cost, collapse = ", "), "\n")

cat("\nLOAD DATA:\n")
periods_load <- sort(unique(load_data$period))
scenarios_load <- sort(unique(load_data$scenario_display))

cat("Periods:", paste(periods_load, collapse = ", "), "\n")
cat("Scenarios:\n")
for (s in scenarios_load) cat("  - ", s, "\n")

cat("\n=== MATCHING CHECK ===\n")
cat("Periods match:", identical(periods_cost, periods_load), "\n")
cat("Scenarios match:", identical(scenarios_cost, scenarios_load), "\n")

if (!identical(periods_cost, periods_load)) {
  cat("\nPeriods in cost but not in load:", setdiff(periods_cost, periods_load), "\n")
  cat("Periods in load but not in cost:", setdiff(periods_load, periods_cost), "\n")
}

if (!identical(scenarios_cost, scenarios_load)) {
  cat("\nScenarios in cost but not in load:\n")
  for (s in setdiff(scenarios_cost, scenarios_load)) cat("  - ", s, "\n")
  cat("Scenarios in load but not in cost:\n")
  for (s in setdiff(scenarios_load, scenarios_cost)) cat("  - ", s, "\n")
}

# Use only matching values
periods <- sort(intersect(periods_cost, periods_load))

# Define scenario order explicitly
scenario_order <- c(
  "VRE & ESS (low) Coal (low)",
  "VRE & ESS (low) Coal (high)",
  "VRE & ESS (mid) Coal (low) - REF",
  "VRE & ESS (mid) Coal (high)",
  "VRE & ESS (high) Coal (low)",
  "VRE & ESS (high) Coal (high)"
)

# Get scenarios that exist in both datasets and order them
scenarios_intersect <- intersect(scenarios_cost, scenarios_load)
scenarios <- scenario_order[scenario_order %in% scenarios_intersect]

data_technologies <- unique(cost_data[[tech_col]])
tech_order_filtered <- tech_order[tech_order %in% data_technologies]

cat("\n=== USING FOR PLOT ===\n")
cat("Periods:", paste(periods, collapse = ", "), "\n")
cat("Scenarios (in order):\n")
for (s in scenarios) cat("  - ", s, "\n")
cat("Technologies:", paste(tech_order_filtered, collapse = ", "), "\n")

n_periods <- length(periods)
n_scenarios <- length(scenarios)

if (n_periods == 0 || n_scenarios == 0) {
  stop("No matching periods or scenarios between cost and load data!")
}

cat("Number of periods:", n_periods, "\n")
cat("Number of scenarios:", n_scenarios, "\n\n")

# Calculate bar width
width <- 1 / (n_scenarios + 1.5)
x_base <- seq(0, n_periods - 1, length.out = n_periods)

# ============================================================================
# CREATE PLOT DATA
# ============================================================================

# Initialize segments dataframe
plot_segments <- data.frame(
  technology = character(),
  scenario_display = character(),
  period = numeric(),
  x_position = numeric(),
  value = numeric(),
  base = numeric(),
  cost_type = character(),  # "variable" or "fixed"
  color = character(),
  stringsAsFactors = FALSE
)

x_current <- x_base

for (i_scen in 1:n_scenarios) {
  scen <- scenarios[i_scen]
  
  cat(sprintf("\n=== Scenario %d/%d: %s ===\n", i_scen, n_scenarios, scen))
  
  for (i_period in 1:n_periods) {
    period <- periods[i_period]
    x_pos <- x_current[i_period]
    
    cat(sprintf("  Period %d (x=%.3f): ", period, x_pos))
    
    # Get load for this scenario+period (z in Python)
    load_val <- load_data %>%
      filter(scenario_display == scen, period == !!period) %>%
      pull(load_mw)
    
    if (length(load_val) == 0) {
      cat("NO LOAD DATA FOUND\n")
      cat(sprintf("    Looking for: scenario='%s', period=%d\n", scen, period))
      cat("    Available combinations in load_data:\n")
      matching <- load_data %>% filter(period == !!period)
      if (nrow(matching) > 0) {
        for (i in 1:min(3, nrow(matching))) {
          cat(sprintf("      scenario='%s', period=%d, load=%.0f\n", 
                     matching$scenario_display[i], matching$period[i], matching$load_mw[i]))
        }
      }
      next
    }
    
    if (is.na(load_val[1]) || load_val[1] == 0) {
      cat(sprintf("Load = %.2f (ZERO/NA - skipping)\n", load_val[1]))
      next
    }
    
    z <- load_val[1]
    cat(sprintf("Load = %.2f MW | ", z))
    
    # Check how many cost records exist
    cost_count <- cost_data %>%
      filter(scenario_display == scen, period == !!period) %>%
      nrow()
    cat(sprintf("Cost records: %d | ", cost_count))
    
    # Reset offsets for this bar
    offset <- 0
    segments_added <- 0
    
    # STEP 1: Add variable costs (with hatching)
    for (tech in tech_order_filtered) {
      tech_data <- cost_data %>%
        filter(scenario_display == scen, 
               period == !!period,
               .data[[tech_col]] == tech)
      
      if (nrow(tech_data) == 1) {
        y <- tech_data$variable_cost[1]
        
        if (!is.na(y) && y != 0) {
          # Levelized cost = cost / load
          levelized_val <- (y / z) / units
          
          segment <- data.frame(
            technology = tech,
            scenario_display = scen,
            period = period,
            x_position = x_pos,
            value = levelized_val,
            base = offset,
            cost_type = "variable",
            color = tech_colors[tech],
            stringsAsFactors = FALSE
          )
          
          plot_segments <- rbind(plot_segments, segment)
          offset <- offset + levelized_val
        }
      }
    }
    
    # STEP 2: Add fixed costs (solid, on top of variable)
    for (tech in tech_order_filtered) {
      tech_data <- cost_data %>%
        filter(scenario_display == scen, 
               period == !!period,
               .data[[tech_col]] == tech)
      
      if (nrow(tech_data) == 1) {
        y <- tech_data$fixed_cost[1]
        
        if (!is.na(y) && y != 0) {
          # Levelized cost = cost / load
          levelized_val <- (y / z) / units
          
          segment <- data.frame(
            technology = tech,
            scenario_display = scen,
            period = period,
            x_position = x_pos,
            value = levelized_val,
            base = offset,
            cost_type = "fixed",
            color = tech_colors[tech],
            stringsAsFactors = FALSE
          )
          
          plot_segments <- rbind(plot_segments, segment)
          offset <- offset + levelized_val
        }
      }
    }
    
    cat(sprintf("Segments: %d | Total: %.2f USD/MWh\n", segments_added, offset))
  }
  
  # Increment x for next scenario
  x_current <- x_current + 0.9 / n_scenarios
}

cat(sprintf("\nTotal segments: %d\n", nrow(plot_segments)))

if (nrow(plot_segments) == 0) {
  stop("No segments created - check data alignment")
}

# ============================================================================
# CREATE PLOTLY FIGURE
# ============================================================================

fig <- plot_ly()

# Add variable cost traces (with hatching)
for (tech in tech_order_filtered) {
  tech_data <- plot_segments %>%
    filter(technology == tech, cost_type == "variable")
  
  if (nrow(tech_data) > 0) {
    tech_color <- unique(tech_data$color)[1]
    
    fig <- fig %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$value,
        name = paste(tech, "(Variable)"),
        legendgroup = tech,
        showlegend = FALSE,  # Don't show in legend (will use fixed)
        marker = list(
          color = tech_color,
          pattern = list(
            shape = "/",           # Diagonal lines (like tech costs chart)
            bgcolor = tech_color,  # Use tech color as background
            fgcolor = "white",     # White hatching lines
            size = 8,
            solidity = 0.3
          ),
          line = list(color = "white", width = 0.5)
        ),
        width = width,
        base = tech_data$base,
        hovertemplate = paste0(
          "<b>", tech, " (Variable)</b><br>",
          "Period: ", tech_data$period, "<br>",
          "Scenario: ", tech_data$scenario_display, "<br>",
          "Cost: %{y:.2f} USD/MWh<br>",
          "<extra></extra>"
        )
      )
  }
}

# Add fixed cost traces (solid, show in legend)
for (tech in tech_order_filtered) {
  tech_data <- plot_segments %>%
    filter(technology == tech, cost_type == "fixed")
  
  if (nrow(tech_data) > 0) {
    fig <- fig %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$value,
        name = gsub("Transmission Losses", "Transmission", tech),
        legendgroup = tech,
        showlegend = TRUE,
        marker = list(
          color = tech_data$color,
          line = list(color = "white", width = 0.5)
        ),
        width = width,
        base = tech_data$base,
        hovertemplate = paste0(
          "<b>", tech, " (Fixed)</b><br>",
          "Period: ", tech_data$period, "<br>",
          "Scenario: ", tech_data$scenario_display, "<br>",
          "Cost: %{y:.2f} USD/MWh<br>",
          "<extra></extra>"
        )
      )
  }
}

# Add "Variable Costs" legend entry
fig <- fig %>%
  add_trace(
    type = "bar",
    x = c(NA),
    y = c(NA),
    name = "Variable Costs",
    showlegend = TRUE,
    marker = list(
      color = "lightgray",
      pattern = list(
        shape = "/",
        fgcolor = "gray"
      )
    )
  )

# ============================================================================
# LAYOUT
# ============================================================================

# Calculate x-axis tick positions
x_tick_positions <- plot_segments %>%
  group_by(period) %>%
  summarise(x_center = mean(x_position), .groups = "drop") %>%
  arrange(period)

# Calculate secondary y-axis (INR per kWh)
yp_ticks_labels <- c(0, 1, 2, 3, 4, 5)
yp_ticks <- yp_ticks_labels * 1000 / USD_to_INR / y_lim_max

fig <- fig %>%
  layout(
    barmode = "overlay",
    title = list(
      text = "Levelized Cost of Electricity",
      font = list(size = 20, family = "Arial"),
      y = 0.95
    ),
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = x_tick_positions$x_center,
      ticktext = x_tick_positions$period,
      tickfont = list(size = 18),
      showgrid = FALSE
    ),
    yaxis = list(
      title = "Costs (USD per MWh)",
      titlefont = list(size = 18),
      tickfont = list(size = 14),
      range = c(-0.1, y_lim_max),
      showgrid = TRUE,
      gridcolor = "lightgray",
      griddash = "dot",
      zeroline = TRUE,
      zerolinecolor = "black",
      zerolinewidth = 1,
      side = "left"
    ),
    yaxis2 = list(
      title = "(INR per kWh)",
      titlefont = list(size = 18),
      tickmode = "array",
      tickvals = yp_ticks * y_lim_max,
      ticktext = yp_ticks_labels,
      tickfont = list(size = 14),
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      range = c(-0.1, y_lim_max)
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    legend = list(
      title = list(text = "", font = list(size = 14)),
      font = list(size = 14),
      x = 1.075,
      y = 0.925,
      xanchor = "left",
      yanchor = "top"
    ),
    margin = list(l = 80, r = 200, t = 80, b = 80),
    width = 1200,
    height = 600
  )

# Display
fig

# ============================================================================
# SAVE
# ============================================================================

saveWidget(fig, 
           "/Users/shradheyprasad/Desktop/emLab/repo/website-test/levelized_costs.html", 
           selfcontained = TRUE)