library(plotly)
library(dplyr)
library(tidyr)
library(readr)
library(htmlwidgets)

# ============================================================================
# TECHNOLOGY COSTS BAR CHART
# ============================================================================

# ============================================================================
# DATA IMPORT
# ============================================================================

setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/input_data/technology_cost")

# Technology costs file
# Columns: period, technology, scenario, variable_cost, fixed_cost
tech_cost_file <- "cost-technology_costs.csv"

cat("Reading technology costs data...\n")
tech_cost <- read_csv(tech_cost_file)

# ============================================================================
# TECHNOLOGY COLORS
# ============================================================================

# Define technology order (bottom to top in stacked bar)
# UPDATE THIS ORDER to match your original plot
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

# Print technology order
cat("\n=== Technology Stacking Order (bottom to top) ===\n")
for (i in seq_along(tech_order)) {
  cat(sprintf("%2d. %s\n", i, tech_order[i]))
}
cat("\n")

# ============================================================================
# PREPARE DATA
# ============================================================================

cat("Preparing data...\n")

# Get unique periods and scenarios
periods <- sort(unique(tech_cost$period))
scenarios <- unique(tech_cost$scenario)

cat("Periods:", paste(periods, collapse = ", "), "\n")
cat("Scenarios:", paste(scenarios, collapse = ", "), "\n")
cat("Technologies:", paste(unique(tech_cost$technology), collapse = ", "), "\n")

n_periods <- length(periods)
n_scenarios <- length(scenarios)

# Calculate bar width and positions (matching Python logic)
# Python uses: width = 1/(len(scens_) + 1.5)
# But we need to adjust for plotly to fill the space better
bar_width <- 0.9 / n_scenarios  # Use most of the space available

# Create x positions for each scenario within each period
x_positions <- data.frame()
for (i_period in 1:n_periods) {
  period <- periods[i_period]
  x_base <- i_period - 1  # Start from 0 like Python
  
  for (i_scen in 1:n_scenarios) {
    scenario <- scenarios[i_scen]
    # Spread bars evenly across the period space
    x_pos <- x_base + (i_scen - 1) * bar_width
    
    x_positions <- rbind(x_positions, data.frame(
      period = period,
      scenario = scenario,
      x_position = x_pos,
      period_index = i_period - 1
    ))
  }
}

# Join positions to data
tech_cost <- tech_cost %>%
  left_join(x_positions, by = c("period", "scenario"))

# ============================================================================
# CREATE PLOTLY FIGURE
# ============================================================================

fig <- plot_ly()

# Use the defined technology order
# Filter to only include technologies that exist in the data
technologies <- tech_order[tech_order %in% unique(tech_cost$technology)]

cat("Technologies in plot (bottom to top):", paste(technologies, collapse = ", "), "\n\n")

# STEP 1: Add all variable costs first (bottom layer, with hatching)
for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- tech_cost %>%
    filter(technology == tech) %>%
    arrange(period, scenario)
  
  if (nrow(tech_data) > 0 && any(tech_data$variable_cost > 0)) {
    # Calculate cumulative bottom for stacking
    tech_data$bottom <- 0
    
    for (i in 1:nrow(tech_data)) {
      period <- tech_data$period[i]
      scenario <- tech_data$scenario[i]
      
      # Get sum of all technologies that come before this one (variable only)
      tech_index <- which(technologies == tech)
      if (tech_index > 1) {
        bottom <- tech_cost %>%
          filter(period == !!period, 
                 scenario == !!scenario,
                 technology %in% technologies[1:(tech_index-1)]) %>%
          summarise(total = sum(variable_cost, na.rm = TRUE)) %>%
          pull(total)
        
        tech_data$bottom[i] <- ifelse(length(bottom) > 0, bottom, 0)
      }
    }
    
    # Add variable costs with hatching pattern
    fig <- fig %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$variable_cost / 1e9,
        name = tech,
        legendgroup = tech,
        showlegend = FALSE,  # Will show in fixed cost trace
        marker = list(
          color = tech_color,
          pattern = list(
            shape = "/",  # Diagonal lines
            bgcolor = tech_color,
            fgcolor = "white",
            size = 8,
            solidity = 0.3
          ),
          line = list(color = "white", width = 0.5)
        ),
        width = bar_width,
        base = tech_data$bottom / 1e9,
        hovertemplate = paste0(
          "<b>", tech, " (Variable)</b><br>",
          "Period: ", tech_data$period, "<br>",
          "Scenario: ", tech_data$scenario, "<br>",
          "Variable Cost: %{y:.2f} B USD<br>",
          "<extra></extra>"
        )
      )
  }
}

# STEP 2: Add all fixed costs on top (solid color)
for (tech in technologies) {
  tech_color <- ifelse(tech %in% names(tech_colors), tech_colors[[tech]], "#999999")
  
  tech_data <- tech_cost %>%
    filter(technology == tech) %>%
    arrange(period, scenario)
  
  if (nrow(tech_data) > 0 && any(tech_data$fixed_cost > 0)) {
    # Calculate cumulative bottom for stacking
    tech_data$bottom <- 0
    
    for (i in 1:nrow(tech_data)) {
      period <- tech_data$period[i]
      scenario <- tech_data$scenario[i]
      
      # Bottom = all variable costs + fixed costs before this tech
      tech_index <- which(technologies == tech)
      
      # All variable costs (entire bottom layer)
      bottom_var <- tech_cost %>%
        filter(period == !!period, scenario == !!scenario) %>%
        summarise(total = sum(variable_cost, na.rm = TRUE)) %>%
        pull(total)
      
      # Fixed costs before this technology
      bottom_fix <- 0
      if (tech_index > 1) {
        bottom_fix <- tech_cost %>%
          filter(period == !!period, 
                 scenario == !!scenario,
                 technology %in% technologies[1:(tech_index-1)]) %>%
          summarise(total = sum(fixed_cost, na.rm = TRUE)) %>%
          pull(total)
      }
      
      if (length(bottom_var) == 0) bottom_var <- 0
      if (length(bottom_fix) == 0) bottom_fix <- 0
      
      tech_data$bottom[i] <- bottom_var + bottom_fix
    }
    
    # Add fixed costs (solid)
    fig <- fig %>%
      add_trace(
        type = "bar",
        x = tech_data$x_position,
        y = tech_data$fixed_cost / 1e9,
        name = tech,
        legendgroup = tech,
        showlegend = TRUE,  # Show in legend
        marker = list(
          color = tech_color,
          line = list(color = "white", width = 0.5)
        ),
        width = bar_width,
        base = tech_data$bottom / 1e9,
        hovertemplate = paste0(
          "<b>", tech, " (Fixed)</b><br>",
          "Period: ", tech_data$period, "<br>",
          "Scenario: ", tech_data$scenario, "<br>",
          "Fixed Cost: %{y:.2f} B USD<br>",
          "<extra></extra>"
        )
      )
  }
}

# ============================================================================
# LAYOUT
# ============================================================================

# Calculate x-axis tick positions (center of each period group)
x_tick_positions <- x_positions %>%
  group_by(period) %>%
  summarise(x_center = mean(x_position), .groups = "drop")

fig <- fig %>%
  layout(
    barmode = "stack",
    title = list(
      text = "Technology Costs by Period and Scenario",
      font = list(size = 18, family = "Arial")
    ),
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = x_tick_positions$x_center,
      ticktext = x_tick_positions$period,
      tickfont = list(size = 14),
      showgrid = FALSE,
      range = c(-bar_width/2, n_periods - 1 + n_scenarios * bar_width + bar_width/2)
    ),
    yaxis = list(
      title = "Technology Costs (Billion USD)",
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

# Display
fig

# ============================================================================
# SAVE
# ============================================================================

cat("\nSaving plot...\n")
saveWidget(fig, "technology_costs_by_period.html", selfcontained = TRUE)
cat("Saved to: technology_costs_by_period.html\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== PLOT SUMMARY ===\n")
cat("Number of periods:", n_periods, "\n")
cat("Number of scenarios per period:", n_scenarios, "\n")
cat("Total bars:", nrow(x_positions), "\n")
cat("Bar width:", round(bar_width, 3), "\n")