library(plotly)
library(dplyr)
library(readr)
library(htmlwidgets)

# ============================================================================
# CAPACITY AND STORAGE PLOTS - Stacked Bar Charts
# ============================================================================

setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")

# ============================================================================
# FILE PATHS - UPDATE THESE
# ============================================================================

capacity_file <- "capacity/cost-grouped_capacity.csv"
# Expected columns: scenario, load_zone, period, technology, status, capacity_mw, energy_mwh

storage_file <- "dispatch/cost-grouped_dispatch.csv"  # Same file as capacity
# Uses same file - capacity_mw for storage capacity, energy_mwh for storage energy

scen_labels_file <- "~/Desktop/emLab/wri-india/website/labels/cost-scenario_labels.csv"
# Expected columns: scenario, zone, label, color, linestyle, marker, linewidth, order

tech_labels_file <- "~/Desktop/emLab/wri-india/website/labels/technology_labels.csv"
# Expected columns: group, group_color, order

# Read data
cat("Reading data...\n")
capacity_data <- read_csv(capacity_file)
storage_data <- read_csv(storage_file)
scen_labels <- read_csv(scen_labels_file)
tech_labels <- read_csv(tech_labels_file)

cat("Data loaded successfully!\n\n")

# ============================================================================
# PLOT 1: NEW AND EXISTING CAPACITY
# ============================================================================

create_capacity_plot <- function(df, scen_labels, tech_labels,
                                 units = 1e3,
                                 ylabel = "Existing & New Capacity (GW)",
                                 y_lim_max = NULL,
                                 legend = TRUE,
                                 title = "") {
  
  # Get unique periods and technologies
  periods <- sort(unique(df$period))
  tech_order <- tech_labels %>% arrange(order)
  
  # Initialize plotly figure
  fig <- plot_ly()
  
  # Calculate bar positions
  n_scens <- nrow(scen_labels)
  bar_width <- 1 / (n_scens + 1.5)
  
  # Track legend entries
  legend_shown <- list()
  
  # Loop through scenarios
  for (i_scen in 1:nrow(scen_labels)) {
    scen <- scen_labels$scenario[i_scen]
    zone <- scen_labels$zone[i_scen]
    label <- scen_labels$label[i_scen]
    
    # Filter data for this scenario/zone
    df_scen <- df %>%
      filter(scenario == scen, load_zone == zone)
    
    # Calculate x positions for this scenario
    x_positions <- seq(0, length(periods) - 1) + (i_scen - 1) * bar_width * 0.9
    
    # Add existing capacity bars (with hatching effect using pattern)
    for (i_tech in 1:nrow(tech_order)) {
      tech <- tech_order$group[i_tech]
      color <- tech_order$group_color[i_tech]
      
      # Get existing capacity data
      existing_data <- df_scen %>%
        filter(technology == tech, status == "existing") %>%
        arrange(period)
      
      if (nrow(existing_data) > 0) {
        # Calculate cumulative bottom for stacking
        for (p_idx in 1:length(periods)) {
          period <- periods[p_idx]
          
          # Get all existing capacity up to this tech for this period
          bottom_data <- df_scen %>%
            filter(period == !!period, status == "existing") %>%
            left_join(tech_order %>% select(group, order), by = c("technology" = "group")) %>%
            filter(order < tech_order$order[i_tech]) %>%
            summarise(total = sum(capacity_mw, na.rm = TRUE))
          
          bottom <- ifelse(nrow(bottom_data) > 0, bottom_data$total[1], 0)
          
          cap_val <- existing_data %>%
            filter(period == !!period) %>%
            pull(capacity_mw)
          
          if (length(cap_val) > 0 && cap_val[1] > 0) {
            show_legend <- FALSE
            if (i_scen == 1 && p_idx == 1) {
              legend_key <- paste0("existing_", tech)
              if (!(legend_key %in% names(legend_shown))) {
                show_legend <- TRUE
                legend_shown[[legend_key]] <- TRUE
              }
            }
            
            fig <- fig %>%
              add_trace(
                x = x_positions[p_idx],
                y = cap_val[1] / units,
                type = "bar",
                width = bar_width,
                base = bottom / units,
                name = ifelse(show_legend, tech, NA),
                legendgroup = paste0("existing_", tech),
                showlegend = show_legend,
                marker = list(
                  color = color,
                  line = list(color = "lightgray", width = 1),
                  pattern = list(shape = "x", bgcolor = color, fgcolor = "lightgray")
                ),
                hovertemplate = paste0(
                  "<b>", label, "</b><br>",
                  "Period: ", period, "<br>",
                  "Tech: ", tech, "<br>",
                  "Status: Existing<br>",
                  "Capacity: %{y:.1f} GW<br>",
                  "<extra></extra>"
                )
              )
          }
        }
      }
    }
    
    # Add new capacity bars
    for (i_tech in 1:nrow(tech_order)) {
      tech <- tech_order$group[i_tech]
      color <- tech_order$group_color[i_tech]
      
      # Get new capacity data
      new_data <- df_scen %>%
        filter(technology == tech, status == "new") %>%
        arrange(period)
      
      if (nrow(new_data) > 0) {
        for (p_idx in 1:length(periods)) {
          period <- periods[p_idx]
          
          # Calculate cumulative bottom (existing + new up to this tech)
          bottom_data <- df_scen %>%
            filter(period == !!period) %>%
            left_join(tech_order %>% select(group, order), by = c("technology" = "group")) %>%
            filter(order <= tech_order$order[i_tech]) %>%
            group_by(status) %>%
            summarise(total = sum(capacity_mw, na.rm = TRUE)) %>%
            ungroup()
          
          # Sum all existing + new capacity below this tech
          bottom_existing <- bottom_data %>% filter(status == "existing") %>% pull(total)
          bottom_existing <- ifelse(length(bottom_existing) > 0, bottom_existing[1], 0)
          
          bottom_new <- df_scen %>%
            filter(period == !!period, status == "new") %>%
            left_join(tech_order %>% select(group, order), by = c("technology" = "group")) %>%
            filter(order < tech_order$order[i_tech]) %>%
            summarise(total = sum(capacity_mw, na.rm = TRUE)) %>%
            pull(total)
          
          bottom_new <- ifelse(length(bottom_new) > 0, bottom_new[1], 0)
          bottom <- bottom_existing + bottom_new
          
          cap_val <- new_data %>%
            filter(period == !!period) %>%
            pull(capacity_mw)
          
          if (length(cap_val) > 0 && cap_val[1] > 0) {
            show_legend <- FALSE
            if (i_scen == 1 && p_idx == 1) {
              legend_key <- paste0("new_", tech)
              if (!(legend_key %in% names(legend_shown))) {
                show_legend <- TRUE
                legend_shown[[legend_key]] <- TRUE
              }
            }
            
            fig <- fig %>%
              add_trace(
                x = x_positions[p_idx],
                y = cap_val[1] / units,
                type = "bar",
                width = bar_width,
                base = bottom / units,
                name = ifelse(show_legend, tech, NA),
                legendgroup = paste0("new_", tech),
                showlegend = show_legend,
                marker = list(color = color),
                hovertemplate = paste0(
                  "<b>", label, "</b><br>",
                  "Period: ", period, "<br>",
                  "Tech: ", tech, "<br>",
                  "Status: New<br>",
                  "Capacity: %{y:.1f} GW<br>",
                  "<extra></extra>"
                )
              )
          }
        }
      }
    }
  }
  
  # Add "Existing" legend entry at the top
  if (1 == 1) {  # Always add
    fig <- fig %>%
      add_trace(
        x = NULL,
        y = NULL,
        type = "bar",
        name = "Existing",
        showlegend = TRUE,
        marker = list(
          color = "white",
          line = list(color = "lightgray", width = 1),
          pattern = list(shape = "x", bgcolor = "white", fgcolor = "lightgray")
        )
      )
  }
  
  # Configure layout
  x_period_centers <- seq(0, length(periods) - 1) + (n_scens - 1) * bar_width * 0.9 / 2
  
  if (is.null(y_lim_max)) {
    y_lim_max <- max(df %>% 
                       group_by(scenario, load_zone, period) %>% 
                       summarise(total = sum(capacity_mw, na.rm = TRUE), .groups = "drop") %>% 
                       pull(total)) / units * 1.2
  }
  
  fig <- fig %>%
    layout(
      title = list(text = title, font = list(size = 18), y = 0.95),
      xaxis = list(
        title = "",
        tickvals = x_period_centers,
        ticktext = periods,
        tickfont = list(size = 16),
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = ylabel,
        titlefont = list(size = 16),
        range = c(0, y_lim_max),
        showgrid = TRUE,
        gridcolor = "lightgray",
        tickfont = list(size = 14)
      ),
      barmode = "overlay",
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      showlegend = legend,
      legend = list(
        x = 1.02,
        y = 0.5,
        xanchor = "left",
        yanchor = "middle",
        font = list(size = 13)
      ),
      margin = list(l = 80, r = 150, t = 60, b = 60)
    )
  
  return(fig)
}

# ============================================================================
# PLOT 2: NEW AND EXISTING STORAGE (CAPACITY)
# ============================================================================

create_storage_capacity_plot <- function(df, scen_labels, tech_labels,
                                         units = 1e3,
                                         ylabel = "Existing & New Capacity (GW)",
                                         y_lim_max = NULL,
                                         y_lim_min = NULL,
                                         legend = TRUE,
                                         title = "") {
  
  # Same logic as capacity plot but for storage
  fig <- create_capacity_plot(df, scen_labels, tech_labels, 
                              units = units, ylabel = ylabel,
                              y_lim_max = y_lim_max, legend = legend, title = title)
  
  return(fig)
}

# ============================================================================
# PLOT 3: NEW AND EXISTING STORAGE (ENERGY)
# ============================================================================

create_storage_energy_plot <- function(df, scen_labels, tech_labels,
                                       units = 1e3,
                                       ylabel = "Existing & New Energy (GWh)",
                                       y_lim_max = NULL,
                                       y_lim_min = NULL,
                                       legend = TRUE,
                                       title = "") {
  
  # Modify df to use energy_mwh instead of capacity_mw
  df_energy <- df %>%
    mutate(capacity_mwh = capacity_mwh)
  
  fig <- create_capacity_plot(df_energy, scen_labels, tech_labels,
                              units = units, ylabel = ylabel,
                              y_lim_max = y_lim_max, legend = legend, title = title)
  
  return(fig)
}

# ============================================================================
# CREATE 3-PANEL SUBPLOT
# ============================================================================

create_3_panel_capacity <- function() {
  
  cat("Creating 3-panel capacity plot...\n")
  
  # Create individual plots
  fig_a <- create_capacity_plot(capacity_data, scen_labels, tech_labels,
                                 y_lim_max = 3000, legend = TRUE,
                                 title = "Generation Capacity")
  
  fig_b <- create_storage_capacity_plot(storage_data, scen_labels, tech_labels,
                                        y_lim_max = 500, legend = FALSE,
                                        title = "Storage Capacity")
  
  fig_c <- create_storage_energy_plot(storage_data, scen_labels, tech_labels,
                                      y_lim_max = 2000, legend = FALSE,
                                      title = "Storage Energy")
  
  # Combine into subplot
  fig <- subplot(
    fig_a, fig_b, fig_c,
    nrows = 1,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )
  
  # Add panel labels
  annotations <- list(
    list(text = "<b>A</b>", x = 0.08, y = 1.08, xref = "paper", yref = "paper",
         xanchor = "center", showarrow = FALSE, font = list(size = 20, family = "Arial")),
    list(text = "<b>B</b>", x = 0.42, y = 1.08, xref = "paper", yref = "paper",
         xanchor = "center", showarrow = FALSE, font = list(size = 20, family = "Arial")),
    list(text = "<b>C</b>", x = 0.76, y = 1.08, xref = "paper", yref = "paper",
         xanchor = "center", showarrow = FALSE, font = list(size = 20, family = "Arial"))
  )
  
  fig <- fig %>%
    layout(
      annotations = annotations,
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 200, t = 100, b = 80)
    )
  
  return(fig)
}

# ============================================================================
# GENERATE AND SAVE
# ============================================================================

# Create individual plots or 3-panel subplot
fig_capacity <- create_3_panel_capacity()
fig_capacity

# Save
output_path <- "/Users/shradheyprasad/Desktop/emLab/repo/website-test/capacity_storage_plots.html"
saveWidget(fig_capacity, output_path, selfcontained = TRUE)
cat("\nSaved:", output_path, "\n")

cat("\n=== SUMMARY ===\n")
cat("3-panel plot created:\n")
cat("  A: Generation Capacity (existing + new by technology)\n")
cat("  B: Storage Capacity (existing + new by technology)\n")
cat("  C: Storage Energy (existing + new by technology)\n")
cat("\nExisting capacity shown with hatched pattern\n")
cat("New capacity shown with solid fill\n")