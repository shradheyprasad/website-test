library(plotly)
library(readxl)
library(dplyr)
library(htmlwidgets)


# Output directory
output_dir <- "/Users/shradheyprasad/Desktop/emLab/repo/website-test/"

# Read data
path_to_input <- "/Users/shradheyprasad/Desktop/emLab/wri-india/website/input_data/"
new_projects_excel <- paste0(path_to_input, "new_projects_v41.xlsx")
new_solar_costs_all <- read_excel(
  new_projects_excel,
  sheet = "adapted_new_solar_costs_2050"
)
new_wind_costs_all <- read_excel(
  new_projects_excel,
  sheet = "adapted_new_wind_costs_2050"
)
new_conventional_costs_all <- read_excel(
  new_projects_excel,
  sheet = "new_conventional_costs_2050"
)
new_hydrogen_cavern_costs_all <- read_excel(
  new_projects_excel,
  sheet = "new_hydrogen_cavern_costs_2050"
)
new_battery_costs_all <- read_excel(
  new_projects_excel,
  sheet = "adapted_new_battery_costs_2050"
)
new_nuclear_costs_all <- read_excel(
  new_projects_excel,
  sheet = "new_nuclear_costs_2050"
)
new_hydro_costs_all <- read_excel(
  new_projects_excel,
  sheet = "new_hydro_pumped_costs_2050"
)

# Exchange Rate
USD_to_INR <- 72

######## SOLAR ########################
####################################

# Filter data for different scenarios and technologies
df_roof_low <- new_solar_costs_all %>%
  filter(scenario == "PVlow", technology == "SolarPV_roof")
df_roof_mid <- new_solar_costs_all %>%
  filter(scenario == "PVmid", technology == "SolarPV_roof")
df_roof_high <- new_solar_costs_all %>%
  filter(scenario == "PVhigh", technology == "SolarPV_roof")

df_single_low <- new_solar_costs_all %>%
  filter(scenario == "PVlow", technology == "SolarPV_single")
df_single_mid <- new_solar_costs_all %>%
  filter(scenario == "PVmid", technology == "SolarPV_single")
df_single_high <- new_solar_costs_all %>%
  filter(scenario == "PVhigh", technology == "SolarPV_single")

df_tilt_low <- new_solar_costs_all %>%
  filter(scenario == "PVlow", technology == "SolarPV_tilt")
df_tilt_mid <- new_solar_costs_all %>%
  filter(scenario == "PVmid", technology == "SolarPV_tilt")
df_tilt_high <- new_solar_costs_all %>%
  filter(scenario == "PVhigh", technology == "SolarPV_tilt")

# Create the plot
fig <- plot_ly()

# Rooftop Solar
fig <- fig %>%
  add_trace(
    x = df_roof_mid$vintage,
    y = df_roof_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#fdd3aa', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_roof_mid$vintage,
    y = df_roof_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#fdd3aa', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(253, 211, 170, 0.3)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_roof_mid$vintage,
    y = df_roof_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#fdd3aa', width = 3),
    name = 'Rooftop',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Single-Axis Solar
fig <- fig %>%
  add_trace(
    x = df_single_mid$vintage,
    y = df_single_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#ffbc85', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_single_mid$vintage,
    y = df_single_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#ffbc85', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(255, 188, 133, 0.3)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_single_mid$vintage,
    y = df_single_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#ffbc85', width = 3),
    name = 'Single-Axis',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Fixed Tilt Solar
fig <- fig %>%
  add_trace(
    x = df_tilt_mid$vintage,
    y = df_tilt_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#f19139', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_tilt_mid$vintage,
    y = df_tilt_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#f19139', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(241, 145, 57, 0.3)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_tilt_mid$vintage,
    y = df_tilt_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#f19139', width = 3),
    name = 'Fixed',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 1150),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 1150 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/solar.html"),
  selfcontained = TRUE
)

######## WIND ########################
####################################
# Filter data for different scenarios and technologies
df_on_low <- new_wind_costs_all %>%
  filter(scenario == "WDlow", technology == "Wind")
df_on_mid <- new_wind_costs_all %>%
  filter(scenario == "WDmid", technology == "Wind")
df_on_high <- new_wind_costs_all %>%
  filter(scenario == "WDhigh", technology == "Wind")

df_off_low <- new_wind_costs_all %>%
  filter(scenario == "WDlow", technology == "Offshore")
df_off_mid <- new_wind_costs_all %>%
  filter(scenario == "WDmid", technology == "Offshore")
df_off_high <- new_wind_costs_all %>%
  filter(scenario == "WDhigh", technology == "Offshore")

# Create the plot
fig <- plot_ly()

# Offshore Wind
fig <- fig %>%
  add_trace(
    x = df_off_mid$vintage,
    y = df_off_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#3b3aa5', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_off_mid$vintage,
    y = df_off_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#3b3aa5', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(42, 100, 138, 0.35)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_off_mid$vintage,
    y = df_off_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#3b3aa5', width = 3),
    name = 'Offshore',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Onshore Wind
fig <- fig %>%
  add_trace(
    x = df_on_mid$vintage,
    y = df_on_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#8dc0cd', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_on_mid$vintage,
    y = df_on_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#8dc0cd', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(141, 192, 205, 0.35)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_on_mid$vintage,
    y = df_on_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#8dc0cd', width = 3),
    name = 'Onshore',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 3500),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 3500 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/wind.html"),
  selfcontained = TRUE
)

######## CONVENTIONAL ################
####################################

# Filter data for different scenarios and technologies
df_coal_low <- new_conventional_costs_all %>%
  filter(scenario == "CONVmid", technology == "Supercritical_Coal")
df_coal_high <- new_conventional_costs_all %>%
  filter(scenario == "CONVhigh", technology == "Supercritical_Coal")

df_ccgt <- new_conventional_costs_all %>%
  filter(scenario == "CONVmid", technology == "CCGT")

df_ct <- new_conventional_costs_all %>%
  filter(scenario == "CONVmid", technology == "CT")

# Create the plot
fig <- plot_ly()

# Coal (with fill between low and high)
fig <- fig %>%
  add_trace(
    x = df_coal_low$vintage,
    y = df_coal_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'black', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_coal_high$vintage,
    y = df_coal_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'black', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(0, 0, 0, 0.35)',
    name = 'Coal',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# CCGT (Gas) - solid line
fig <- fig %>%
  add_trace(
    x = df_ccgt$vintage,
    y = df_ccgt$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6c757d', width = 3),
    name = 'CCGT (Gas)',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# CT (Gas) - dashed line
fig <- fig %>%
  add_trace(
    x = df_ct$vintage,
    y = df_ct$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6c757d', width = 3, dash = 'dash'),
    name = 'CT (Gas)',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 2000),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 2000 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 0,
      y = 0,
      xanchor = "left",
      yanchor = "bottom",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/conventional.html"),
  selfcontained = TRUE
)

######## HYDROGEN CAVERN ################
####################################

# Filter data for different scenarios
df_low <- new_hydrogen_cavern_costs_all %>%
  filter(scenario == "STlow")
df_mid <- new_hydrogen_cavern_costs_all %>%
  filter(scenario == "STmid")
df_high <- new_hydrogen_cavern_costs_all %>%
  filter(scenario == "SThigh")

# Create the plot
fig <- plot_ly()

# Hydrogen storage
fig <- fig %>%
  add_trace(
    x = df_mid$vintage,
    y = df_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'teal', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'teal', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(0, 128, 128, 0.35)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'teal', width = 3),
    name = 'Hydrogen',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 1600),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 1600 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/hydrogen.html"),
  selfcontained = TRUE
)

######## HYDROGEN energy ################
####################################
# Filter data for different scenarios
df_low <- new_hydrogen_cavern_costs_all %>%
  filter(scenario == "STlow")
df_mid <- new_hydrogen_cavern_costs_all %>%
  filter(scenario == "STmid")
df_high <- new_hydrogen_cavern_costs_all %>%
  filter(scenario == "SThigh")

# Create the plot
fig <- plot_ly()

# Hydrogen tank storage (energy cost)
fig <- fig %>%
  add_trace(
    x = df_mid$vintage,
    y = df_low$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'teal', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_high$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'teal', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(0, 128, 128, 0.35)',
    name = 'Tank',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.2f}/kWh<extra></extra>'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_mid$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'teal', width = 3),
    showlegend = FALSE,
    hoverinfo = 'skip'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kWh)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 4),
      tickformat = ".1f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MWh)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 4 * USD_to_INR * 1000 / 1e5),
      tickformat = ".1f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/hydrogen_energy.html"),
  selfcontained = TRUE
)

######## Battery ################
####################################
# Filter data for different scenarios
df_low <- new_battery_costs_all %>%
  filter(scenario == "STlow")
df_mid <- new_battery_costs_all %>%
  filter(scenario == "STmid")
df_high <- new_battery_costs_all %>%
  filter(scenario == "SThigh")

# Create the plot
fig <- plot_ly()

# Battery storage
fig <- fig %>%
  add_trace(
    x = df_mid$vintage,
    y = df_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#e7c41f', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#e7c41f', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(231, 196, 31, 0.35)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#e7c41f', width = 3),
    name = 'Battery',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "(USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 300),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 300 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/battery.html"),
  selfcontained = TRUE
)

######## Nuclear + ROR  ################
####################################
# Hydro ROR constant cost
hydro_ROR <- 1486.11

# Filter data for nuclear
df_nuclear <- new_nuclear_costs_all %>%
  filter(scenario == "NUCLmid", technology == "Nuclear")

# Create the plot
fig <- plot_ly()

# Hydro (ROR) - constant horizontal line
fig <- fig %>%
  add_trace(
    x = df_nuclear$vintage,
    y = rep(hydro_ROR, nrow(df_nuclear)),
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#2a648a', width = 3),
    fill = 'tozeroy',
    fillcolor = 'rgba(42, 100, 138, 0.35)',
    name = 'Hydro (ROR)',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Nuclear
fig <- fig %>%
  add_trace(
    x = df_nuclear$vintage,
    y = df_nuclear$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#8d72b3', width = 3),
    fill = 'tozeroy',
    fillcolor = 'rgba(141, 114, 179, 0.35)',
    name = 'Nuclear',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 2000),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 2000 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 0,
      y = 0,
      xanchor = "left",
      yanchor = "bottom",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/nuck_ror.html"),
  selfcontained = TRUE
)

######## PHS ################
####################################
# Filter data for different scenarios
df_low <- new_hydro_costs_all %>%
  filter(scenario == "STlow")
df_mid <- new_hydro_costs_all %>%
  filter(scenario == "STmid")
df_high <- new_hydro_costs_all %>%
  filter(scenario == "SThigh")

# Create the plot
fig <- plot_ly()

# Pumped Storage Hydro
fig <- fig %>%
  add_trace(
    x = df_mid$vintage,
    y = df_low$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6a96ac', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_high$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6a96ac', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(106, 150, 172, 0.35)',
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_mid$capital_real_cost_per_kw,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6a96ac', width = 3),
    name = 'Pumped Storage',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kW<extra></extra>'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 600),
      tickformat = ",.0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MW)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 600 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 0,
      y = 0,
      xanchor = "left",
      yanchor = "bottom",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest'
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/phs.html"),
  selfcontained = TRUE
)

######## PHS energy ################
####################################

df_low <- new_hydro_costs_all %>%
  filter(scenario == "STlow")
df_mid <- new_hydro_costs_all %>%
  filter(scenario == "STmid")
df_high <- new_hydro_costs_all %>%
  filter(scenario == "SThigh")

# Create the plot
fig <- plot_ly()

# Pumped Hydro Storage (energy cost)
fig <- fig %>%
  add_trace(
    x = df_mid$vintage,
    y = df_low$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6a96ac', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_high$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6a96ac', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(106, 150, 172, 0.35)',
    name = 'Pumped Hydro',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.1f}/kWh<extra></extra>'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_mid$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#6a96ac', width = 3),
    showlegend = FALSE,
    hoverinfo = 'skip'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "Technology Cost (USD/kWh)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 40),
      tickformat = ".1f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MWh)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 40 * USD_to_INR * 1000 / 1e5),
      tickformat = ".1f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest',
    showlegend = FALSE
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/phs_energy.html"),
  selfcontained = TRUE
)

######## Battery energy ################
####################################
# Filter data for different scenarios
df_low <- new_battery_costs_all %>%
  filter(scenario == "STlow")
df_mid <- new_battery_costs_all %>%
  filter(scenario == "STmid")
df_high <- new_battery_costs_all %>%
  filter(scenario == "SThigh")

# Create the plot
fig <- plot_ly()

# Battery storage (energy cost)
fig <- fig %>%
  add_trace(
    x = df_mid$vintage,
    y = df_low$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#e7c41f', width = 1.5),
    showlegend = FALSE,
    hoverinfo = 'skip'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_high$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#e7c41f', width = 1.5),
    fill = 'tonexty',
    fillcolor = 'rgba(231, 196, 31, 0.35)',
    name = 'Battery',
    hovertemplate = 'Year: %{x}<br>Cost: $%{y:.0f}/kWh<extra></extra>'
  ) %>%
  add_trace(
    x = df_mid$vintage,
    y = df_mid$capital_real_cost_per_kwh,
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#e7c41f', width = 3),
    showlegend = FALSE,
    hoverinfo = 'skip'
  )

# Layout with dual y-axes and flexible sizing
fig <- fig %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = c(2020, 2030, 2040, 2050),
      ticktext = c("2020", "2030", "2040", "2050"),
      tickfont = list(size = 20)
    ),
    yaxis = list(
      title = "(USD/kWh)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      range = c(0, 350),
      tickformat = ".0f"
    ),
    yaxis2 = list(
      title = "(Lakh INR/MWh)",
      titlefont = list(size = 20),
      tickfont = list(size = 20),
      overlaying = "y",
      side = "right",
      range = c(0, 350 * USD_to_INR * 1000 / 1e5),
      tickformat = ",.0f"
    ),
    legend = list(
      x = 1,
      y = 1,
      xanchor = "right",
      yanchor = "top",
      orientation = "h",
      font = list(size = 20),
      bgcolor = "rgba(255,255,255,0)"
    ),
    autosize = TRUE,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    hovermode = 'closest',
    showlegend = FALSE
  )

# Display the plot
fig
saveWidget(
  fig,
  paste0(output_dir, "cost_projections/battery_energy.html"),
  selfcontained = TRUE
)
