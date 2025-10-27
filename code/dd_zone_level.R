library(ggplot2)
library(scatterpie)
library(ggiraph)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(ggforce)

setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")
# ============================================================================
# MONTHLY DEMAND COMPARISON PLOT
# ============================================================================

# FILE PATHS

# Demand data file
# Expected columns: FY, Month, Day, Interval, India, scenario
demand_data_file <- "input_data/input_data-avg_demand_profiles.csv"

cat("Reading demand data...\n")
demand_data <- read_csv(demand_data_file)

# ============================================================================
# PARAMETERS
# ============================================================================

# Fiscal years to plot
fys <- c(2020, 2050)

# Scenario labels
scen_labels <- c(
  "Linearly-scaled Demand",
  "Bottom-up Modified Demand",
  "Scenario 1",
  "Scenario 2",
  "Scenario 3"
)

# Plot all days (not aggregated)
plot_all_days <- TRUE  # Set to FALSE to aggregate (avg/max/min)

# Method for aggregating days (if plot_all_days = FALSE): "avg", "max", or "min"
method <- "avg"

# Month names
months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Colors
colors <- c('#2cb7b5', '#136564', '#756a01', '#ca8250', '#d8a581')
historical_color <- '#92918b'

# ============================================================================
# FUNCTION TO PREPARE DATA FOR ONE MONTH
# ============================================================================

prepare_monthly_data <- function(df, fys, scen_labels, i_month, method = "avg") {
  
  # Filter data for this month
  df_month <- df %>%
    filter(Month == i_month + 1, FY %in% fys, scenario %in% scen_labels)
  
  # Aggregate by day (get avg/max/min across days)
  plot_data <- data.frame()
  
  for (fy in fys) {
    for (scen in scen_labels) {
      
      df_subset <- df_month %>%
        filter(FY == fy, scenario == scen)
      
      if (nrow(df_subset) == 0) next
      
      # Aggregate across days for each interval
      if (method == "avg") {
        df_agg <- df_subset %>%
          group_by(Interval) %>%
          summarise(demand = mean(India, na.rm = TRUE), .groups = "drop")
      } else if (method == "max") {
        max_day <- df_subset %>%
          group_by(Day) %>%
          summarise(total = sum(India, na.rm = TRUE), .groups = "drop") %>%
          slice_max(total, n = 1) %>%
          pull(Day)
        df_agg <- df_subset %>%
          filter(Day == max_day[1]) %>%
          select(Interval, demand = India)
      } else if (method == "min") {
        min_day <- df_subset %>%
          group_by(Day) %>%
          summarise(total = sum(India, na.rm = TRUE), .groups = "drop") %>%
          slice_min(total, n = 1) %>%
          pull(Day)
        df_agg <- df_subset %>%
          filter(Day == min_day[1]) %>%
          select(Interval, demand = India)
      }
      
      df_agg$FY <- fy
      df_agg$scenario <- scen
      plot_data <- rbind(plot_data, df_agg)
    }
  }
  
  # Convert to GWh (from MWh)
  plot_data <- plot_data %>%
    mutate(demand_gwh = demand / 1000)
  
  # Create labels and colors
  plot_data <- plot_data %>%
    mutate(
      line_label = case_when(
        FY == 2020 & scenario == "Linearly-scaled Demand" ~ "2020 Historical Demand",
        FY == 2020 & scenario == "Bottom-up Modified Demand" ~ NA_character_,
        TRUE ~ paste0("2050 ", scenario)
      ),
      line_color = case_when(
        FY == 2020 & scenario == "Linearly-scaled Demand" ~ historical_color,
        TRUE ~ colors[match(scenario, scen_labels)]
      )
    ) %>%
    filter(!is.na(line_label))  # Remove bottom-up 2020
  
  return(plot_data)
}

# ============================================================================
# FUNCTION TO CREATE PLOTLY PLOT FOR ONE MONTH
# ============================================================================

plot_monthly_demand_plotly <- function(df, fys, scen_labels, i_month, method = "avg",
                                       ylabel = "Demand (GWh)") {
  
  # Prepare data
  plot_data <- prepare_monthly_data(df, fys, scen_labels, i_month, method)
  
  # Create plotly figure
  fig <- plot_ly()
  
  # Add a trace for each unique line
  unique_lines <- plot_data %>%
    distinct(FY, scenario, line_label, line_color)
  
  for (i in 1:nrow(unique_lines)) {
    line_data <- plot_data %>%
      filter(FY == unique_lines$FY[i], scenario == unique_lines$scenario[i])
    
    fig <- fig %>%
      add_trace(
        data = line_data,
        x = ~Interval,
        y = ~demand_gwh,
        type = "scatter",
        mode = "lines",
        name = unique_lines$line_label[i],
        line = list(
          color = unique_lines$line_color[i],
          width = 2.5
        ),
        opacity = 0.75,
        hovertemplate = paste0(
          "<b>", unique_lines$line_label[i], "</b><br>",
          "Hour: %{x}<br>",
          "Demand: %{y:.1f} GWh<br>",
          "<extra></extra>"
        )
      )
  }
  
  # Layout
  fig <- fig %>%
    layout(
      title = list(
        text = months[i_month + 1],
        font = list(size = 18, family = "Arial", face = "bold"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = c(0, 12, 23),
        ticktext = c("1am", "12pm", "12am"),
        tickfont = list(size = 14),
        range = c(0, 23),
        showgrid = FALSE
      ),
      yaxis = list(
        title = ylabel,
        titlefont = list(size = 16),
        tickfont = list(size = 14),
        range = c(-5, 1075),
        showgrid = TRUE,
        gridcolor = "lightgray",
        gridwidth = 1,
        griddash = "dot",
        zeroline = TRUE,
        zerolinecolor = "black",
        zerolinewidth = 1.5
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(
        font = list(size = 12),
        x = 1.02,
        y = 1,
        xanchor = "left",
        yanchor = "top"
      ),
      margin = list(l = 80, r = 150, t = 80, b = 60),
      hovermode = "x unified"
    )
  
  return(fig)
}

# ============================================================================
# CREATE INDIVIDUAL PLOTS FOR EACH MONTH
# ============================================================================

cat("\nCreating 12 individual month plots...\n")

# Store all figures in a list
month_figures <- list()

for (i_month in 0:11) {
  cat("Creating plot for", months[i_month + 1], "...\n")
  
  fig <- plot_monthly_demand_plotly(demand_data, fys, scen_labels, 
                                    i_month = i_month, method = method)
  
  # Save to file
  month_name <- tolower(months[i_month + 1])
  filename <- paste0("monthly_demand_", month_name, ".html")
  saveWidget(fig, filename, selfcontained = TRUE)
  
  # Store in list
  month_figures[[i_month + 1]] <- fig
  
  cat("  Saved:", filename, "\n")
}

# Display first month as example
cat("\nDisplaying January plot:\n")
month_figures[[4]]
cat("\nAll 12 monthly plots created!\n")

# ============================================================================
# SAve
# ============================================================================

for (i_month in 0:11) {
  cat("Creating plot for", months[i_month + 1], "...\n")
  
  # Create the plot
  fig <- plot_monthly_demand_plotly(demand_data, fys, scen_labels, 
                                    i_month = i_month, method = method)
  
  # Define the full file path
  month_name <- tolower(months[i_month + 1])
  filename <- paste0("/Users/shradheyprasad/Desktop/emLab/repo/website-test/dd_plots/", 
                     "monthly_demand_", month_name, ".html")
  
  # Save the file
  saveWidget(fig, filename, selfcontained = TRUE)
  
  cat("  Saved:", filename, "\n")
}
