library(plotly)
library(dplyr)
library(readr)
library(htmlwidgets)

# ============================================================================
# DEMAND PROFILES SUMMARY - Individual Plots
# ============================================================================

setwd("/Users/shradheyprasad/Desktop/emLab/wri-india/website/tables")

# ============================================================================
# FILE PATHS - UPDATE THESE
# ============================================================================

system_cost_file <- "system_cost/demand-system_cost.csv"
load_data_file <- "emissions/demand-emissions.csv"
emissions_file <- "emissions/demand-emissions.csv"
clean_energy_file <- "clean_energy/demand-clean_energy.csv"
scen_labels_file <- "../labels/demand-scenario_labels.csv"

# Read data
cat("Reading data...\n")
system_cost <- read_csv(system_cost_file)
load_data <- read_csv(load_data_file)
emissions <- read_csv(emissions_file)
clean_energy <- read_csv(clean_energy_file)
scen_labels <- read_csv(scen_labels_file)

cat("Data loaded successfully!\n\n")

# ============================================================================
# HELPER FUNCTIONS FOR EACH PANEL
# ============================================================================

# Panel A: Total System Cost
create_total_cost_plot <- function(
  df,
  scen_labels,
  USD_to_INR = 72,
  units = 1e9,
  y_min = NULL,
  y_max = NULL,
  legend = FALSE,
  title = "Total System Cost",
  legend_title = "Scenario"
) {
  spec <- 5.9e9

  fig <- plot_ly()

  for (i in 1:nrow(scen_labels)) {
    scen <- scen_labels$scenario[i]
    zone <- scen_labels$zone[i]

    data_scen <- df %>%
      filter(scenario == scen, load_zone == zone) %>%
      arrange(period)

    if (nrow(data_scen) == 0) {
      next
    }

    x <- data_scen$period
    y <- data_scen$variable_cost
    w <- data_scen$fixed_cost + spec

    total_cost <- (y + w) / units

    fig <- fig %>%
      add_trace(
        x = x,
        y = total_cost,
        type = "scatter",
        mode = "lines",
        name = scen_labels$label[i],
        line = list(
          color = scen_labels$color[i],
          width = scen_labels$linewidth[i],
          dash = scen_labels$linestyle[i]
        ),
        marker = list(
          symbol = ifelse(
            is.na(scen_labels$marker[i]) || scen_labels$marker[i] == "None",
            "circle",
            scen_labels$marker[i]
          ),
          size = 2
        ),
        opacity = 0.75,
        showlegend = FALSE,
        hovertemplate = paste0(
          "<b>",
          scen_labels$label[i],
          "</b><br>",
          "Year: %{x}<br>",
          "Cost: %{y:.1f} B USD<br>",
          "<extra></extra>"
        )
      )
  }

  if (is.null(y_min)) {
    y_min <- 0
  }
  if (is.null(y_max)) {
    y_max <- 350
  }

  fig <- fig %>%
    layout(
      title = list(text = title, font = list(size = 18)),
      xaxis = list(
        title = "Year",
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "Costs (Billion USD)",
        titlefont = list(size = 16),
        range = c(y_min, y_max),
        showgrid = TRUE,
        gridcolor = "lightgray",
        griddash = "dot",
        tickfont = list(size = 14),
        side = "left"
      ),
      yaxis2 = list(
        title = "(Lakh Crores INR)",
        titlefont = list(size = 16),
        range = c(USD_to_INR * y_min / 1000, USD_to_INR * y_max / 1000),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        tickfont = list(size = 14)
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 60, b = 80),
      showlegend = FALSE
    )

  return(fig)
}

# Panel B: Levelized Cost of Electricity
create_levelized_cost_plot <- function(
  cost_data,
  load_data,
  scen_labels,
  USD_to_INR = 72,
  y_min = NULL,
  y_max = NULL,
  legend = FALSE,
  title = "Levelized Cost of Electricity",
  legend_title = "Scenario"
) {
  spec <- 5.9e9

  fig <- plot_ly()

  for (i in 1:nrow(scen_labels)) {
    scen <- scen_labels$scenario[i]
    zone <- scen_labels$zone[i]

    data_scen <- cost_data %>%
      filter(scenario == scen, load_zone == zone) %>%
      arrange(period)

    load_scen <- load_data %>%
      filter(scenario == scen, load_zone == zone) %>%
      arrange(period)

    if (nrow(data_scen) == 0 || nrow(load_scen) == 0) {
      next
    }

    x <- data_scen$period
    y <- data_scen$variable_cost
    w <- data_scen$fixed_cost + spec
    z <- load_scen$load_mw

    levelized_cost <- (y + w) / z

    fig <- fig %>%
      add_trace(
        x = x,
        y = levelized_cost,
        type = "scatter",
        mode = "lines+markers",
        name = scen_labels$label[i],
        line = list(
          color = scen_labels$color[i],
          width = scen_labels$linewidth[i],
          dash = scen_labels$linestyle[i]
        ),
        marker = list(
          symbol = ifelse(
            is.na(scen_labels$marker[i]) || scen_labels$marker[i] == "None",
            "circle",
            scen_labels$marker[i]
          ),
          size = 2
        ),
        opacity = 0.75,
        showlegend = FALSE,
        hovertemplate = paste0(
          "<b>",
          scen_labels$label[i],
          "</b><br>",
          "Year: %{x}<br>",
          "LCOE: %{y:.2f} USD/MWh<br>",
          "<extra></extra>"
        )
      )
  }

  if (is.null(y_min)) {
    y_min <- 40
  }
  if (is.null(y_max)) {
    y_max <- 70
  }

  y_secondary <- c(3, 4, 5)

  fig <- fig %>%
    layout(
      title = list(text = title, font = list(size = 18)),
      xaxis = list(
        title = "Year",
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "Costs (USD per MWh)",
        titlefont = list(size = 16),
        range = c(y_min, y_max),
        showgrid = TRUE,
        gridcolor = "lightgray",
        griddash = "dot",
        tickfont = list(size = 14),
        side = "left"
      ),
      yaxis2 = list(
        title = "(INR per kWh)",
        titlefont = list(size = 16),
        range = c(USD_to_INR * y_min / 1000, USD_to_INR * y_max / 1000),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        tickvals = y_secondary,
        ticktext = as.character(y_secondary),
        tickfont = list(size = 14)
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 60, b = 80),
      showlegend = FALSE
    )

  return(fig)
}

# Panel C: Carbon Emissions
create_emissions_plot <- function(
  df,
  scen_labels,
  units = 1e6,
  y_min = NULL,
  y_max = NULL,
  legend = FALSE,
  title = "Carbon Emissions",
  legend_title = "Scenario"
) {
  fig <- plot_ly()

  for (i in 1:nrow(scen_labels)) {
    scen <- scen_labels$scenario[i]
    zone <- scen_labels$zone[i]

    data_scen <- df %>%
      filter(scenario == scen, load_zone == zone) %>%
      arrange(period)

    if (nrow(data_scen) == 0) {
      next
    }

    x <- data_scen$period
    y <- data_scen$carbon_emissions_tons

    emissions <- y / units

    fig <- fig %>%
      add_trace(
        x = x,
        y = emissions,
        type = "scatter",
        mode = "lines+markers",
        name = scen_labels$label[i],
        line = list(
          color = scen_labels$color[i],
          width = scen_labels$linewidth[i],
          dash = scen_labels$linestyle[i]
        ),
        marker = list(
          symbol = ifelse(
            is.na(scen_labels$marker[i]) || scen_labels$marker[i] == "None",
            "circle",
            scen_labels$marker[i]
          ),
          size = 2
        ),
        opacity = 0.75,
        showlegend = FALSE,
        hovertemplate = paste0(
          "<b>",
          scen_labels$label[i],
          "</b><br>",
          "Year: %{x}<br>",
          "Emissions: %{y:.0f} Mt CO2<br>",
          "<extra></extra>"
        )
      )
  }

  if (is.null(y_min)) {
    y_min <- 0
  }
  if (is.null(y_max)) {
    y_max <- 1400
  }

  fig <- fig %>%
    layout(
      title = list(text = title, font = list(size = 18)),
      xaxis = list(
        title = "Year",
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "GHG Emissions (MtCO₂)",
        titlefont = list(size = 16),
        range = c(y_min, y_max),
        showgrid = TRUE,
        gridcolor = "lightgray",
        griddash = "dot",
        tickfont = list(size = 14)
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 60, b = 80),
      showlegend = FALSE
    )

  return(fig)
}

# Panel D: Carbon Intensity
create_emissions_intensity_plot <- function(
  df,
  scen_labels,
  y_min = NULL,
  y_max = NULL,
  legend = FALSE,
  title = "Carbon Intensity",
  legend_title = "Scenario"
) {
  fig <- plot_ly()

  for (i in 1:nrow(scen_labels)) {
    scen <- scen_labels$scenario[i]
    zone <- scen_labels$zone[i]

    data_scen <- df %>%
      filter(scenario == scen, load_zone == zone) %>%
      arrange(period)

    if (nrow(data_scen) == 0) {
      next
    }

    x <- data_scen$period
    y <- data_scen$carbon_emissions_tons
    z <- data_scen$load_mw

    emissions_intensity <- y / z

    fig <- fig %>%
      add_trace(
        x = x,
        y = emissions_intensity,
        type = "scatter",
        mode = "lines+markers",
        name = scen_labels$label[i],
        line = list(
          color = scen_labels$color[i],
          width = scen_labels$linewidth[i],
          dash = scen_labels$linestyle[i]
        ),
        marker = list(
          symbol = ifelse(
            is.na(scen_labels$marker[i]) || scen_labels$marker[i] == "None",
            "circle",
            scen_labels$marker[i]
          ),
          size = 2
        ),
        opacity = 0.75,
        showlegend = FALSE,
        hovertemplate = paste0(
          "<b>",
          scen_labels$label[i],
          "</b><br>",
          "Year: %{x}<br>",
          "Intensity: %{y:.3f} tCO2/MWh<br>",
          "<extra></extra>"
        )
      )
  }

  if (is.null(y_min)) {
    y_min <- 0
  }
  if (is.null(y_max)) {
    y_max <- 0.8
  }

  fig <- fig %>%
    layout(
      title = list(text = title, font = list(size = 18)),
      xaxis = list(
        title = "Year",
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "GHG Emissions Intensity (tCO₂/MWh)",
        titlefont = list(size = 16),
        range = c(y_min, y_max),
        showgrid = TRUE,
        gridcolor = "lightgray",
        griddash = "dot",
        tickfont = list(size = 14)
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 60, b = 80),
      showlegend = FALSE
    )

  return(fig)
}

# Panel E: Clean Energy
create_clean_energy_plot <- function(
  df,
  scen_labels,
  y_min = NULL,
  y_max = NULL,
  legend = FALSE,
  title = "Clean Energy Share",
  legend_title = "Demand"
) {
  fig <- plot_ly()

  for (i in 1:nrow(scen_labels)) {
    scen <- scen_labels$scenario[i]
    zone <- scen_labels$zone[i]

    data_scen <- df %>%
      filter(scenario == scen, load_zone == zone) %>%
      arrange(period)

    if (nrow(data_scen) == 0) {
      next
    }

    clean_data <- data_scen %>% filter(technology == "clean")
    no_clean_data <- data_scen %>% filter(technology == "no_clean")

    if (nrow(clean_data) == 0 || nrow(no_clean_data) == 0) {
      next
    }

    x <- clean_data$period
    y <- clean_data$power_mw
    z <- no_clean_data$power_mw

    clean_pct <- 100 * y / (y + z)

    fig <- fig %>%
      add_trace(
        x = x,
        y = clean_pct,
        type = "scatter",
        mode = "lines+markers",
        name = scen_labels$label[i],
        line = list(
          color = scen_labels$color[i],
          width = scen_labels$linewidth[i],
          dash = scen_labels$linestyle[i]
        ),
        marker = list(
          symbol = ifelse(
            is.na(scen_labels$marker[i]) || scen_labels$marker[i] == "None",
            "circle",
            scen_labels$marker[i]
          ),
          size = 2
        ),
        opacity = 0.75,
        showlegend = legend,
        hovertemplate = paste0(
          "<b>",
          scen_labels$label[i],
          "</b><br>",
          "Year: %{x}<br>",
          "Clean Energy: %{y:.1f}%<br>",
          "<extra></extra>"
        )
      )
  }

  if (is.null(y_min)) {
    y_min <- 0
  }
  if (is.null(y_max)) {
    y_max <- 100
  }

  fig <- fig %>%
    layout(
      title = list(text = title, font = list(size = 18)),
      xaxis = list(
        title = "Year",
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "Clean Energy (%)",
        titlefont = list(size = 16),
        range = c(y_min, y_max),
        showgrid = TRUE,
        gridcolor = "lightgray",
        griddash = "dot",
        tickfont = list(size = 14)
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 60, b = 80),
      showlegend = FALSE
    )

  # Add legend configuration if legend is TRUE
  if (legend) {
    fig <- fig %>%
      layout(
        legend = list(
          title = list(text = legend_title, font = list(size = 16)),
          x = 1.02,
          y = 0.98,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 14),
          bordercolor = "lightgray",
          borderwidth = 1,
          bgcolor = "white"
        )
      )
  }

  return(fig)
}

# ============================================================================
# CREATE AND SAVE INDIVIDUAL PLOTS
# ============================================================================

# Output directory
output_dir <- "/Users/shradheyprasad/Desktop/emLab/repo/website-test/"

# Plot A: Total System Cost
fig_a <- create_total_cost_plot(
  system_cost,
  scen_labels,
  y_min = 0,
  y_max = 350,
  legend = FALSE,
  title = "Total System Cost"
)
fig_a
saveWidget(
  fig_a,
  paste0(output_dir, "cost_plots/demand_plot_a.html"),
  selfcontained = TRUE
)

# Plot B: Levelized Cost of Electricity
fig_b <- create_levelized_cost_plot(
  system_cost,
  load_data,
  scen_labels,
  y_min = 40,
  y_max = 70,
  legend = FALSE,
  title = "Levelized Cost of Electricity"
)
fig_b
saveWidget(
  fig_b,
  paste0(output_dir, "cost_plots/demand_plot_b.html"),
  selfcontained = TRUE
)

# Plot C: Carbon Emissions
fig_c <- create_emissions_plot(
  emissions,
  scen_labels,
  y_min = 0,
  y_max = 1400,
  legend = FALSE,
  title = "Carbon Emissions"
)
fig_c
saveWidget(
  fig_c,
  paste0(output_dir, "cost_plots/demand_plot_c.html"),
  selfcontained = TRUE
)

# Plot D: Carbon Intensity
fig_d <- create_emissions_intensity_plot(
  emissions,
  scen_labels,
  y_min = 0,
  y_max = 0.8,
  legend = FALSE,
  title = "Carbon Intensity"
)
fig_d
saveWidget(
  fig_d,
  paste0(output_dir, "cost_plots/demand_plot_d.html"),
  selfcontained = TRUE
)

# Plot E: Clean Energy
fig_e <- create_clean_energy_plot(
  clean_energy,
  scen_labels,
  y_min = 0,
  y_max = 100,
  legend = TRUE,
  legend_title = "Demand",
  title = "Clean Energy Share"
)
fig_e
saveWidget(
  fig_e,
  paste0(output_dir, "cost_plots/demand_plot_e.html"),
  selfcontained = TRUE
)
