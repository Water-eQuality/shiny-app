# --- Libraries ---
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(bslib)
library(lubridate)
library(DT)
library(scales)

# --- Heal the Bay Brand Colors ---
htb_colors <- list(
  # Primary
  htb_blue = "#40B4E5",
  
  # Secondary
  aqua = "#00B6B6",
  algae = "#90B83E",
  ocean_blue = "#005CB9",
  light_aqua = "#8ACFCF",
  light_algae = "#BACF86",
  coal_gray = "#263746",
  sunshine = "#FCC755",
  sunset_pink = "#F26859",
  white = "#FFFFFF",
  light_sunshine = "#FEDB97",
  light_sunset = "#F7A18E",
  
  # Aquarium specific
  deep_sea = "#0E4C90",
  garibaldi = "#F47E48",
  beam = "#D9D8D6",
  kelp = "#546122",
  sand = "#F3DAAB",
  coal_black = "#3F3C39"
)

# --- Prepare Site Info ---
site_info <- read_csv("data/fib_loc_na.csv", show_col_types = FALSE) %>%
  select(site_id = fpk_loc_id, location_name = fld_loc_name,
         lat = fld_latitude, lon = fld_longitude) %>%
  filter(!is.na(lat)) %>%
  distinct(site_id, .keep_all = TRUE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_join(read_sf("data/ca_counties/CA_Counties_TIGER2016.shp") %>%
            filter(NAME == "Los Angeles") %>% st_transform(4326),
          join = st_within) %>%
  filter(!is.na(NAME)) %>% st_drop_geometry()

# --- Load FIB Data ---
fib_data <- read_csv("data/fib_all_clean.csv", show_col_types = FALSE) %>%
  rename(site_id = fpk_loc_id,
         total_val = fld_total_val,
         fecal_val = fld_fecal_val,
         entero_val = fld_entero_val) %>%
  mutate(date = ymd(date)) %>%
  inner_join(site_info, by = "site_id")

fib_long <- fib_data %>%
  pivot_longer(cols = c(total_val, fecal_val, entero_val),
               names_to = "parameter", values_to = "result") %>%
  filter(!is.na(result))

projects_data <- read_csv("data/stormwater_project_loc.csv", show_col_types = FALSE)
la_county_geo <- read_sf("data/ca_counties/CA_Counties_TIGER2016.shp") %>%
  filter(NAME == "Los Angeles") %>% st_transform(4326)

# --- Load NEW Combined Stormwater Projects Data ---
combined_projects <- read_csv("data/combined_stormwater_projects_complete.csv", show_col_types = FALSE) %>%
  rename(
    name = `Project Name`,
    lat = Latitude,
    lon = Longitude,
    footprint_acres = `Project Footprint (Acres)`,
    drainage_acres = `Drainage Area (Acres)`,
    storage_capacity = `Actual Storage Capacity (Acre-feet)`,
    volume_addressed = `Cumulative Volume Addressed (Acre-feet)`,
    completion_date = `Actual Completion Date`,
    project_type = `Project Type`,
    capital_cost = `Capital Costs ($)`,
    om_cost = `Cumulative O&M Costs ($)`
  ) %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    # Clean up project types
    project_type_clean = case_when(
      str_detect(project_type, "Green Street") ~ "Green Street",
      str_detect(project_type, "LID Retrofit|Bioretention") ~ "LID/Bioretention",
      str_detect(project_type, "Infiltration Well") ~ "Infiltration Well",
      str_detect(project_type, "Regional Infiltration") ~ "Regional Infiltration",
      str_detect(project_type, "Treatment Facility") ~ "Treatment Facility",
      str_detect(project_type, "Biofiltration") ~ "Biofiltration",
      str_detect(project_type, "Diversion") ~ "Diversion",
      str_detect(project_type, "Detention") ~ "Detention",
      TRUE ~ "Other"
    ),
    # Format costs for display
    capital_cost_fmt = scales::dollar(capital_cost, accuracy = 1),
    volume_fmt = ifelse(is.na(volume_addressed), "N/A", 
                        paste0(round(volume_addressed, 2), " acre-ft"))
  ) %>%
  filter(!is.na(lat), !is.na(lon))

# --- Load Disadvantaged Communities (DAC) Data ---
# SB 535 Disadvantaged Communities based on CalEnviroScreen 4.0
dac_data <- read_csv("data/la_dac_tracts.csv", show_col_types = FALSE) %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    ces_percentile = as.numeric(ces_percentile),
    # Simplify DAC category names for display
    dac_category_short = case_when(
      str_detect(dac_category, "Top 25%") ~ "CES 4.0 Top 25%",
      str_detect(dac_category, "High Pollution") ~ "High Pollution Burden",
      str_detect(dac_category, "3.0") ~ "CES 3.0 DAC",
      TRUE ~ "Other DAC"
    ),
    # Create percentile bins for filtering
    percentile_bin = case_when(
      ces_percentile >= 95 ~ "95-100% (Highest)",
      ces_percentile >= 90 ~ "90-95%",
      ces_percentile >= 85 ~ "85-90%",
      ces_percentile >= 80 ~ "80-85%",
      ces_percentile >= 75 ~ "75-80%",
      TRUE ~ "Below 75%"
    ),
    # Format for popup
    ces_score_fmt = round(as.numeric(ces_score), 1),
    population_fmt = format(as.numeric(population), big.mark = ",")
  ) %>%
  filter(!is.na(lat), !is.na(lon))

# Create sf object for DAC points
dac_pts <- dac_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Get unique DAC categories for filtering
dac_categories <- sort(unique(dac_data$dac_category_short))

# --- Helper functions ---
pick_col <- function(df, candidates, fallback = NULL) {
  nms <- names(df); norm <- function(x) gsub("\\s+", "", tolower(x))
  n_nms <- norm(nms)
  for (cand in candidates) {
    hit <- which(n_nms == norm(cand))
    if (length(hit)) return(nms[hit[1]])
  }
  fallback
}
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# --- Spatial point data ---

# NEW Combined stormwater projects as sf object
stormwater_pts <- combined_projects %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Get unique project types for filtering
project_types <- sort(unique(combined_projects$project_type_clean))

# Legacy project points (keeping for backwards compatibility if needed)
projects_pts <- projects_data %>%
  rename(lat = Latitude, lon = Longitude, name = `Project Name`) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# WRAMPS project points
wramps_pts <- read_csv("data/wramps_clean.csv", show_col_types = FALSE) %>%
  rename(lat = Latitude, lon = Longitude, name = project) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

monitoring_sites_pts <- site_info %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

testing_sites_pts <- read_csv("data/testing_site_locations.csv", show_col_types = FALSE) %>%
  transmute(name = `Site Name`, lat = as.numeric(Latitude),
            lon = as.numeric(Longitude), order_in_ws = `Order in Watershed`) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

la_prec_stations_pts <- {
  df <- read_csv("data/la_county_prec_stations.csv", show_col_types = FALSE)
  name_col <- pick_col(df, c("name", "station", "station name", "site", "site name")) %||% {
    df$name_tmp <- paste0("Station ", seq_len(nrow(df))); "name_tmp"
  }
  lat_col  <- pick_col(df, c("latitude", "lat", "y"))
  lon_col  <- pick_col(df, c("longitude", "long", "lon", "x"))
  df %>%
    transmute(name = .data[[name_col]],
              lat  = suppressWarnings(as.numeric(.data[[lat_col]])),
              lon  = suppressWarnings(as.numeric(.data[[lon_col]]))) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# --- Watershed shapefile (fixed) ---
fixed_ws <- tryCatch({
  read_sf("data/fixed.shp") %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON")
}, error = function(e) NULL)

pick_poly_label <- function(sfobj) {
  if (is.null(sfobj)) return(NULL)
  pick_col(st_drop_geometry(sfobj),
           c("Name", "Watershed", "WS_Name", "HU_12_NAME", "label", "name"))
}

# Map colors using Heal the Bay palette
map_cols <- list(
  projects = htb_colors$algae,
  wramps = htb_colors$kelp,
  monitoring = htb_colors$htb_blue,
  testing = htb_colors$sunset_pink,
  precip = htb_colors$sunshine,
  ws_line = htb_colors$ocean_blue,
  ws_fill = htb_colors$light_aqua
)

# --- Custom CSS for Heal the Bay Theme ---
htb_css <- "
/* Heal the Bay Custom Theme */
@import url('https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@400;700&family=Roboto:wght@400;500;700&display=swap');

:root {
  --htb-blue: #40B4E5;
  --htb-ocean-blue: #005CB9;
  --htb-aqua: #00B6B6;
  --htb-algae: #90B83E;
  --htb-coal-gray: #263746;
  --htb-sunshine: #FCC755;
  --htb-sunset-pink: #F26859;
  --htb-light-aqua: #8ACFCF;
  --htb-light-algae: #BACF86;
  --htb-white: #FFFFFF;
  --htb-light-sunshine: #FEDB97;
  --htb-light-sunset: #F7A18E;
}

body {
  font-family: 'Roboto', 'Helvetica Neue', Helvetica, Arial, sans-serif;
  background-color: #f8f9fa;
  color: var(--htb-coal-gray);
}

/* Navbar Styling */
.navbar {
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-ocean-blue) 100%) !important;
  border: none !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.15);
  padding: 0.5rem 1rem;
}

.navbar-brand {
  display: flex !important;
  align-items: center !important;
  padding: 0.5rem 0;
}

.navbar-brand img {
  margin-right: 12px;
}

.navbar .navbar-nav .nav-link {
  color: rgba(255,255,255,0.9) !important;
  font-weight: 500;
  padding: 0.75rem 1.25rem !important;
  transition: all 0.2s ease;
  border-radius: 4px;
  margin: 0 2px;
}

.navbar .navbar-nav .nav-link:hover {
  color: var(--htb-white) !important;
  background-color: rgba(255,255,255,0.15);
}

.navbar .navbar-nav .nav-link.active {
  color: var(--htb-white) !important;
  background-color: rgba(255,255,255,0.2);
  font-weight: 700;
}

/* Page Headers */
h1, h2, h3, h4, h5, h6 {
  font-family: 'Roboto Slab', Georgia, serif;
  color: var(--htb-coal-gray);
}

h2 {
  color: var(--htb-ocean-blue);
  border-bottom: 3px solid var(--htb-blue);
  padding-bottom: 0.5rem;
  margin-bottom: 1.5rem;
}

/* Cards and Panels */
.card, .well, .panel {
  border: none;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  background-color: var(--htb-white);
}

.card-header {
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-aqua) 100%);
  color: var(--htb-white);
  font-family: 'Roboto Slab', Georgia, serif;
  font-weight: 700;
  border-radius: 8px 8px 0 0 !important;
}

/* Sidebar Styling */
.sidebar {
  background-color: var(--htb-white);
  border-radius: 8px;
  padding: 1.5rem;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}

/* Form Controls */
.form-control, .selectize-input, .shiny-input-container select {
  border: 2px solid #e0e0e0;
  border-radius: 6px;
  padding: 0.5rem 0.75rem;
  transition: border-color 0.2s ease, box-shadow 0.2s ease;
}

.form-control:focus, .selectize-input.focus {
  border-color: var(--htb-blue);
  box-shadow: 0 0 0 3px rgba(64, 180, 229, 0.2);
  outline: none;
}

.shiny-input-container label {
  font-weight: 500;
  color: var(--htb-coal-gray);
  margin-bottom: 0.5rem;
}

/* Buttons */
.btn-primary {
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-ocean-blue) 100%);
  border: none;
  border-radius: 6px;
  padding: 0.6rem 1.5rem;
  font-weight: 500;
  transition: all 0.2s ease;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.btn-primary:hover {
  background: linear-gradient(135deg, var(--htb-ocean-blue) 0%, #004590 100%);
  transform: translateY(-1px);
  box-shadow: 0 4px 8px rgba(0,0,0,0.15);
}

.btn-default, .btn-secondary {
  background-color: var(--htb-white);
  border: 2px solid var(--htb-blue);
  color: var(--htb-blue);
  border-radius: 6px;
  font-weight: 500;
  transition: all 0.2s ease;
}

.btn-default:hover, .btn-secondary:hover {
  background-color: var(--htb-blue);
  color: var(--htb-white);
}

/* Checkbox and Radio Inputs */
.checkbox-inline, .radio-inline {
  padding: 0.5rem 1rem;
  margin-right: 0.5rem;
  background-color: var(--htb-white);
  border: 2px solid #e0e0e0;
  border-radius: 20px;
  transition: all 0.2s ease;
}

.checkbox-inline:hover, .radio-inline:hover {
  border-color: var(--htb-blue);
  background-color: rgba(64, 180, 229, 0.05);
}

/* Slider styling */
.irs--shiny .irs-bar {
  background: linear-gradient(90deg, var(--htb-blue) 0%, var(--htb-aqua) 100%);
  border: none;
}

.irs--shiny .irs-handle {
  background-color: var(--htb-ocean-blue);
  border: 3px solid var(--htb-white);
  box-shadow: 0 2px 4px rgba(0,0,0,0.2);
}

.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background-color: var(--htb-ocean-blue);
  border-radius: 4px;
}

/* Tab styling */
.nav-tabs {
  border-bottom: 2px solid var(--htb-light-aqua);
}

.nav-tabs .nav-link {
  color: var(--htb-coal-gray);
  border: none;
  border-bottom: 3px solid transparent;
  padding: 0.75rem 1.25rem;
  font-weight: 500;
  transition: all 0.2s ease;
}

.nav-tabs .nav-link:hover {
  color: var(--htb-blue);
  border-bottom-color: var(--htb-light-aqua);
  background-color: transparent;
}

.nav-tabs .nav-link.active {
  color: var(--htb-ocean-blue);
  border-bottom: 3px solid var(--htb-blue);
  background-color: transparent;
  font-weight: 700;
}

/* DataTable Styling */
.dataTables_wrapper {
  padding: 1rem;
}

table.dataTable thead th {
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-ocean-blue) 100%);
  color: var(--htb-white);
  font-weight: 500;
  border: none !important;
}

table.dataTable tbody tr:nth-child(even) {
  background-color: rgba(64, 180, 229, 0.05);
}

table.dataTable tbody tr:hover {
  background-color: rgba(64, 180, 229, 0.1) !important;
}

.dataTables_filter input {
  border: 2px solid #e0e0e0;
  border-radius: 6px;
  padding: 0.4rem 0.75rem;
}

.dataTables_filter input:focus {
  border-color: var(--htb-blue);
  outline: none;
  box-shadow: 0 0 0 3px rgba(64, 180, 229, 0.2);
}

/* Map container */
.leaflet-container {
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.1);
}

/* Welcome/Overview section */
.welcome-section {
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-aqua) 100%);
  color: var(--htb-white);
  padding: 2.5rem;
  border-radius: 12px;
  margin-bottom: 2rem;
  box-shadow: 0 4px 15px rgba(64, 180, 229, 0.3);
}

.welcome-section h2 {
  color: var(--htb-white);
  border-bottom: none;
  margin-bottom: 1rem;
}

.feature-card {
  background-color: var(--htb-white);
  border-radius: 8px;
  padding: 1.5rem;
  margin-bottom: 1rem;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  transition: transform 0.2s ease, box-shadow 0.2s ease;
  border-left: 4px solid var(--htb-blue);
}

.feature-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.12);
}

.feature-card h4 {
  color: var(--htb-ocean-blue);
  margin-bottom: 0.5rem;
}

.feature-card p {
  color: var(--htb-coal-gray);
  margin-bottom: 0;
}

.feature-icon {
  width: 48px;
  height: 48px;
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-aqua) 100%);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-bottom: 1rem;
  color: var(--htb-white);
  font-size: 1.25rem;
}

/* Footer */
.footer {
  background-color: var(--htb-coal-gray);
  color: var(--htb-white);
  padding: 1.5rem;
  margin-top: 2rem;
  border-radius: 8px 8px 0 0;
  text-align: center;
}

/* Download button */
.btn-download {
  background: linear-gradient(135deg, var(--htb-algae) 0%, #7aa832 100%);
  border: none;
  color: var(--htb-white);
  padding: 0.6rem 1.5rem;
  border-radius: 6px;
  font-weight: 500;
  transition: all 0.2s ease;
}

.btn-download:hover {
  background: linear-gradient(135deg, #7aa832 0%, #6a9528 100%);
  transform: translateY(-1px);
}

/* Legend styling for map */
.legend-title {
  font-family: 'Roboto Slab', Georgia, serif;
  font-weight: 700;
  color: var(--htb-coal-gray);
  margin-bottom: 0.5rem;
}

/* Responsive adjustments */
@media (max-width: 768px) {
  .navbar-brand span {
    font-size: 14px !important;
  }
  
  .welcome-section {
    padding: 1.5rem;
  }
  
  .feature-card {
    padding: 1rem;
  }
}

/* Plot styling */
.shiny-plot-output {
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}

/* Info boxes */
.info-box {
  background-color: var(--htb-light-aqua);
  border-left: 4px solid var(--htb-blue);
  padding: 1rem 1.5rem;
  border-radius: 0 8px 8px 0;
  margin-bottom: 1rem;
}

.info-box p {
  margin-bottom: 0;
  color: var(--htb-coal-gray);
}

/* Accent colors for different sections */
.accent-algae {
  border-left-color: var(--htb-algae);
}

.accent-sunset {
  border-left-color: var(--htb-sunset-pink);
}

.accent-sunshine {
  border-left-color: var(--htb-sunshine);
}

/* Map Controls Panel */
.map-controls-panel {
  max-height: calc(100vh - 200px);
  overflow-y: auto;
}

.map-controls-panel h4 {
  font-size: 14px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.map-controls-panel .checkbox label {
  display: flex;
  align-items: center;
  padding: 8px 12px;
  margin: 4px 0;
  background-color: #f8f9fa;
  border-radius: 6px;
  transition: all 0.2s ease;
  cursor: pointer;
}

.map-controls-panel .checkbox label:hover {
  background-color: rgba(64, 180, 229, 0.1);
}

.map-controls-panel .checkbox input:checked + span {
  font-weight: 500;
  color: var(--htb-ocean-blue);
}

/* Map Legend */
.map-legend {
  padding: 0.5rem 0;
}

/* Leaflet customizations */
.leaflet-container {
  font-family: 'Roboto', sans-serif;
}

.leaflet-popup-content-wrapper {
  border-radius: 8px;
  box-shadow: 0 3px 14px rgba(0,0,0,0.15);
}

.leaflet-popup-content {
  margin: 12px 16px;
  font-family: 'Roboto', sans-serif;
}

.leaflet-popup-tip {
  box-shadow: 0 3px 14px rgba(0,0,0,0.15);
}

.leaflet-control-layers {
  border-radius: 8px !important;
  box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
  border: none !important;
}

.leaflet-control-layers-toggle {
  width: 36px !important;
  height: 36px !important;
}

.leaflet-control-scale-line {
  border-color: var(--htb-coal-gray) !important;
  background: rgba(255,255,255,0.8) !important;
}

.leaflet-bar a {
  border-radius: 4px !important;
  border: none !important;
  box-shadow: 0 1px 4px rgba(0,0,0,0.15) !important;
  color: var(--htb-coal-gray) !important;
}

.leaflet-bar a:hover {
  background-color: var(--htb-light-aqua) !important;
}

/* Select all / Clear all links */
.map-controls-panel a {
  color: var(--htb-blue);
  text-decoration: none;
}

.map-controls-panel a:hover {
  color: var(--htb-ocean-blue);
  text-decoration: underline;
}

/* Selectize dropdown styling */
.selectize-dropdown {
  border: 2px solid var(--htb-blue) !important;
  border-radius: 0 0 6px 6px !important;
  box-shadow: 0 4px 12px rgba(0,0,0,0.1) !important;
}

.selectize-dropdown .active {
  background-color: rgba(64, 180, 229, 0.15) !important;
  color: var(--htb-ocean-blue) !important;
}

.selectize-input.focus {
  border-color: var(--htb-blue) !important;
  box-shadow: 0 0 0 3px rgba(64, 180, 229, 0.2) !important;
}

.selectize-input .item {
  background: linear-gradient(135deg, var(--htb-blue) 0%, var(--htb-aqua) 100%) !important;
  color: white !important;
  border: none !important;
  border-radius: 4px !important;
  padding: 2px 8px !important;
}

.selectize-input .item .remove {
  color: rgba(255,255,255,0.8) !important;
  border-left-color: rgba(255,255,255,0.3) !important;
}

.selectize-input .item .remove:hover {
  color: white !important;
  background: rgba(0,0,0,0.1) !important;
}
"

# --- Theme using bslib ---
app_theme <- bs_theme(
  
  version = 5,
  bg = "#f8f9fa",
  fg = "#263746",
  primary = "#40B4E5",
  secondary = "#00B6B6",
  success = "#90B83E",
  info = "#8ACFCF",
  warning = "#FCC755",
  danger = "#F26859",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Slab"),
  font_scale = 1.05
)

# --- UI ---
ui <- navbarPage(
  theme = app_theme,
  title = div(
    style = "display:flex; align-items:center;",
    img(src = "healthebay.png", height = "45px", style = "margin-right:12px;"),
    div(
      span("The Water (e)Quality", style = "font-size:18px; font-weight:700; color:white;"),
      br(),
      span("UCSB Bren School of Environmental Science & Management", 
           style = "font-size:11px; color:rgba(255,255,255,0.85);")
    )
  ),
  header = tags$head(
    tags$style(HTML(htb_css))
  ),
  
  # Overview Tab
  tabPanel("Overview",
           fluidPage(
             div(class = "welcome-section",
                 h2("Welcome to the LA Water Quality Explorer", 
                    style = "font-size: 28px; font-weight: 700;"),
                 p("An interactive dashboard providing insights into water quality monitoring 
           and stormwater capture projects across Los Angeles County.",
                   style = "font-size: 16px; opacity: 0.95; max-width: 700px;")
             ),
             
             fluidRow(
               column(4,
                      div(class = "feature-card",
                          div(class = "feature-icon", icon("map-marked-alt")),
                          h4("Interactive Map"),
                          p("Explore project locations, monitoring sites, watershed boundaries, 
               and precipitation stations across LA County.")
                      )
               ),
               column(4,
                      div(class = "feature-card accent-algae",
                          div(class = "feature-icon", 
                              style = "background: linear-gradient(135deg, #90B83E 0%, #7aa832 100%);",
                              icon("chart-line")),
                          h4("Trend Analysis"),
                          p("Analyze water quality parameters over time with interactive 
               time series and seasonal pattern visualizations.")
                      )
               ),
               column(4,
                      div(class = "feature-card accent-sunset",
                          div(class = "feature-icon",
                              style = "background: linear-gradient(135deg, #F26859 0%, #d94d3d 100%);",
                              icon("table")),
                          h4("Data Explorer"),
                          p("Search, filter, and export raw water quality data for 
               your own analysis and research needs.")
                      )
               )
             ),
             
             div(class = "info-box", style = "margin-top: 1.5rem;",
                 p(HTML("<strong>About this project:</strong> This dashboard was developed 
               in partnership with Heal the Bay to support their mission of making 
               coastal waters and watersheds of Greater Los Angeles safe, healthy, 
               and clean."))
             )
           )
  ),
  
  # Map Tab
  tabPanel("Map",
           fluidPage(
             h2("Map View: Stormwater Projects & Monitoring Sites"),
             div(class = "info-box", style = "margin-bottom: 1.5rem;",
                 p("Explore stormwater capture projects and water quality monitoring sites 
           across Los Angeles County. Use the filters to customize your view.")
             ),
             fluidRow(
               # Map controls sidebar
               column(3,
                      div(class = "map-controls-panel",
                          style = "background: white; padding: 1.25rem; border-radius: 8px; 
                       box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
                          
                          # Layer toggles
                          h4("Map Layers", style = "color: #005CB9; margin-bottom: 1rem; 
                font-family: 'Roboto Slab', serif;"),
                          checkboxGroupInput("map_layers", NULL,
                                             choices = c(
                                               "Stormwater Projects",
                                               "Disadvantaged Communities",
                                               "Beach Monitoring Sites",
                                               "River Monitoring Sites",
                                               "LA County Precip Stations",
                                               "Watershed Boundaries"
                                             ),
                                             selected = c(
                                               "Stormwater Projects",
                                               "Disadvantaged Communities",
                                               "Watershed Boundaries"
                                             )),
                          
                          hr(style = "border-color: #e0e0e0; margin: 1rem 0;"),
                          
                          # DAC Filter
                          h4("DAC Filter (SB 535)", style = "color: #005CB9; margin-bottom: 0.75rem;
                font-family: 'Roboto Slab', serif;"),
                          p("Filter disadvantaged communities by CES percentile:", 
                            style = "font-size: 12px; color: #666; margin-bottom: 0.5rem;"),
                          checkboxGroupInput("dac_percentile_filter", NULL,
                                             choices = c(
                                               "95-100% (Highest)",
                                               "90-95%",
                                               "85-90%",
                                               "80-85%",
                                               "75-80%"
                                             ),
                                             selected = c(
                                               "95-100% (Highest)",
                                               "90-95%",
                                               "85-90%",
                                               "80-85%",
                                               "75-80%"
                                             )),
                          div(style = "margin-top: 0.5rem;",
                              actionLink("select_all_dac", "Select All", 
                                         style = "font-size: 12px; margin-right: 10px;"),
                              actionLink("clear_all_dac", "Clear All", 
                                         style = "font-size: 12px;")
                          ),
                          
                          hr(style = "border-color: #e0e0e0; margin: 1rem 0;"),
                          
                          # Project Type Filter
                          h4("Project Type Filter", style = "color: #005CB9; margin-bottom: 0.75rem;
                font-family: 'Roboto Slab', serif;"),
                          p("Filter stormwater projects by type:", 
                            style = "font-size: 12px; color: #666; margin-bottom: 0.5rem;"),
                          checkboxGroupInput("project_type_filter", NULL,
                                             choices = c(
                                               "Green Street",
                                               "LID/Bioretention", 
                                               "Infiltration Well",
                                               "Regional Infiltration",
                                               "Treatment Facility",
                                               "Biofiltration",
                                               "Diversion",
                                               "Detention",
                                               "Other"
                                             ),
                                             selected = c(
                                               "Green Street",
                                               "LID/Bioretention", 
                                               "Infiltration Well",
                                               "Regional Infiltration",
                                               "Treatment Facility",
                                               "Biofiltration",
                                               "Diversion",
                                               "Detention",
                                               "Other"
                                             )),
                          div(style = "margin-top: 0.5rem;",
                              actionLink("select_all_types", "Select All", 
                                         style = "font-size: 12px; margin-right: 10px;"),
                              actionLink("clear_all_types", "Clear All", 
                                         style = "font-size: 12px;")
                          ),
                          
                          hr(style = "border-color: #e0e0e0; margin: 1rem 0;"),
                          
                          # Watershed Filter
                          h4("Watershed Filter", style = "color: #005CB9; margin-bottom: 0.75rem;
                font-family: 'Roboto Slab', serif;"),
                          uiOutput("watershed_filter_ui")
                      ),
                      
                      # Legend in separate card below
                      div(style = "background: white; padding: 1.25rem; border-radius: 8px; 
                       box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin-top: 1rem;",
                          h4("Legend", style = "color: #005CB9; margin-bottom: 1rem;
                font-family: 'Roboto Slab', serif;"),
                          div(class = "map-legend",
                              tags$table(style = "width: 100%; font-size: 12px;",
                                         tags$tr(
                                           tags$td(style = "padding: 4px 0;",
                                                   span(style = paste0("width: 12px; height: 12px; border-radius: 50%; 
                          background-color: ", htb_colors$algae, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 2px rgba(0,0,0,0.2);")),
                                                   "Stormwater"
                                           ),
                                           tags$td(style = "padding: 4px 0;",
                                                   span(style = paste0("width: 12px; height: 12px; border-radius: 50%; 
                          background-color: ", htb_colors$htb_blue, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 2px rgba(0,0,0,0.2);")),
                                                   "Beach Sites"
                                           )
                                         ),
                                         tags$tr(
                                           tags$td(style = "padding: 4px 0;",
                                                   span(style = paste0("width: 12px; height: 12px; border-radius: 50%; 
                          background-color: ", htb_colors$sunset_pink, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 2px rgba(0,0,0,0.2);")),
                                                   "River Sites"
                                           ),
                                           tags$td(style = "padding: 4px 0;",
                                                   span(style = paste0("width: 12px; height: 12px; border-radius: 50%; 
                          background-color: ", htb_colors$sunshine, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 2px rgba(0,0,0,0.2);")),
                                                   "Precip Stations"
                                           )
                                         ),
                                         tags$tr(
                                           tags$td(style = "padding: 4px 0;",
                                                   span(style = "width: 12px; height: 12px; border-radius: 50%; 
                          background: linear-gradient(135deg, #d73027 0%, #fc8d59 100%); 
                          display: inline-block; margin-right: 6px; border: 2px solid white; 
                          box-shadow: 0 1px 2px rgba(0,0,0,0.2);"),
                                                   "DAC Tracts"
                                           ),
                                           tags$td(style = "padding: 4px 0;",
                                                   span(style = paste0("width: 12px; height: 12px; border: 2px solid ", 
                                                                       htb_colors$ocean_blue, "; background-color: ", htb_colors$light_aqua, 
                                                                       "; opacity: 0.7; display: inline-block; margin-right: 6px;")),
                                                   "Watersheds"
                                           )
                                         )
                              )
                          ),
                          # DAC color scale explanation
                          div(style = "margin-top: 0.75rem; padding-top: 0.75rem; border-top: 1px solid #e0e0e0;",
                              p("DAC Color Scale (CES Percentile):", 
                                style = "font-size: 11px; color: #666; margin-bottom: 0.25rem;"),
                              div(style = "display: flex; align-items: center; font-size: 10px;",
                                  span("75%", style = "margin-right: 4px;"),
                                  div(style = "flex: 1; height: 8px; border-radius: 4px;
                            background: linear-gradient(to right, #fc8d59, #d73027);"),
                                  span("100%", style = "margin-left: 4px;")
                              )
                          )
                      )
               ),
               
               # Map display
               column(9,
                      div(style = "background: white; padding: 0.5rem; border-radius: 8px; 
                       box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
                          leafletOutput("map", height = "650px")
                      ),
                      # Map stats bar
                      div(style = "background: linear-gradient(135deg, #40B4E5 0%, #00B6B6 100%); 
                       padding: 1rem 1.5rem; border-radius: 8px; margin-top: 1rem;
                       display: flex; justify-content: space-around; color: white;",
                          div(style = "text-align: center;",
                              uiOutput("stat_projects")
                          ),
                          div(style = "text-align: center;",
                              uiOutput("stat_dac")
                          ),
                          div(style = "text-align: center;",
                              uiOutput("stat_monitoring")
                          ),
                          div(style = "text-align: center;",
                              uiOutput("stat_volume")
                          )
                      )
               )
             )
           )
  ),
  
  # Trends Tab
  tabPanel("Trends",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               style = "background-color: white; border-radius: 8px; 
                 box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
               h4("Filter Options", style = "color: #005CB9; margin-bottom: 1.25rem;"),
               selectInput("site_select", "Monitoring Site:", 
                           choices = unique(fib_long$location_name)),
               selectInput("param_select", "Parameter:",
                           choices = c("Total Coliform" = "total_val", 
                                       "Fecal Coliform" = "fecal_val", 
                                       "Enterococcus" = "entero_val")),
               sliderInput("year_range", "Year Range:",
                           min = year(min(fib_long$date)), 
                           max = year(max(fib_long$date)),
                           value = c(year(min(fib_long$date)), year(max(fib_long$date))), 
                           sep = ""),
               div(class = "info-box accent-algae", style = "margin-top: 1.5rem;",
                   p(HTML("<strong>Tip:</strong> Use the filters above to customize your 
                  view of water quality trends."))
               )
             ),
             mainPanel(
               width = 9,
               h2("Water Quality Trends"),
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Time Series", 
                          div(style = "padding: 1rem;",
                              plotOutput("trend_plot", height = "450px")
                          )
                 ),
                 tabPanel("Seasonal Patterns", 
                          div(style = "padding: 1rem;",
                              plotOutput("seasonal_plot", height = "450px")
                          )
                 )
               )
             )
           )
  ),
  
  # Data Table Tab
  tabPanel("Data Table",
           fluidPage(
             h2("Raw Data Viewer"),
             div(class = "info-box accent-sunshine", style = "margin-bottom: 1.5rem;",
                 p("Search and filter the complete dataset. Use the download button 
           to export data for your own analysis.")
             ),
             div(style = "background: white; padding: 1.5rem; border-radius: 8px; 
                   box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
                 DTOutput("data_table"),
                 br(),
                 downloadButton("download_data", "Download CSV", class = "btn-download")
             )
           )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Reactive value to store watershed names (handles NULL case)
  watershed_choices <- reactive({
    if (is.null(fixed_ws)) return(character(0))
    sort(unique(fixed_ws$LABEL))
  })
  
  # Reactive filtered stormwater projects
  filtered_stormwater <- reactive({
    selected_types <- input$project_type_filter
    if (is.null(selected_types) || length(selected_types) == 0) {
      return(stormwater_pts[0, ])  # Return empty sf
    }
    stormwater_pts %>%
      filter(project_type_clean %in% selected_types)
  })
  
  # Reactive filtered DAC tracts
  filtered_dac <- reactive({
    selected_pctls <- input$dac_percentile_filter
    if (is.null(selected_pctls) || length(selected_pctls) == 0) {
      return(dac_pts[0, ])  # Return empty sf
    }
    dac_pts %>%
      filter(percentile_bin %in% selected_pctls)
  })
  
  # dynamic watershed filter UI with checkboxes instead of selectize
  output$watershed_filter_ui <- renderUI({
    ws_names <- watershed_choices()
    if (length(ws_names) == 0) {
      return(div(
        style = "color: #888; font-style: italic; padding: 0.5rem;",
        "No watershed data available"
      ))
    }
    
    tagList(
      div(style = "max-height: 200px; overflow-y: auto; padding: 0.5rem; 
                   background: #f8f9fa; border-radius: 6px;",
          checkboxGroupInput("selected_watersheds", NULL,
                             choices = ws_names, 
                             selected = ws_names)
      ),
      div(style = "margin-top: 0.5rem;",
          actionLink("select_all_ws", "Select All", 
                     style = "font-size: 12px; margin-right: 10px;"),
          actionLink("clear_all_ws", "Clear All", 
                     style = "font-size: 12px;")
      )
    )
  })
  
  # Select/Clear all watersheds
  observeEvent(input$select_all_ws, {
    updateCheckboxGroupInput(session, "selected_watersheds", 
                             selected = watershed_choices())
  })
  
  observeEvent(input$clear_all_ws, {
    updateCheckboxGroupInput(session, "selected_watersheds", 
                             selected = character(0))
  })
  
  # Select/Clear all project types
  observeEvent(input$select_all_types, {
    updateCheckboxGroupInput(session, "project_type_filter", 
                             selected = c("Green Street", "LID/Bioretention", "Infiltration Well",
                                          "Regional Infiltration", "Treatment Facility", 
                                          "Biofiltration", "Diversion", "Detention", "Other"))
  })
  
  observeEvent(input$clear_all_types, {
    updateCheckboxGroupInput(session, "project_type_filter", 
                             selected = character(0))
  })
  
  # Select/Clear all DAC percentiles
  observeEvent(input$select_all_dac, {
    updateCheckboxGroupInput(session, "dac_percentile_filter", 
                             selected = c("95-100% (Highest)", "90-95%", "85-90%", "80-85%", "75-80%"))
  })
  
  observeEvent(input$clear_all_dac, {
    updateCheckboxGroupInput(session, "dac_percentile_filter", 
                             selected = character(0))
  })
  
  # Stats outputs for the map - now reactive to filters
  output$stat_projects <- renderUI({
    n_proj <- nrow(filtered_stormwater())
    div(
      div(n_proj, style = "font-size: 24px; font-weight: bold;"),
      div("Stormwater Projects", style = "font-size: 12px; opacity: 0.9;")
    )
  })
  
  output$stat_dac <- renderUI({
    n_dac <- nrow(filtered_dac())
    total_dac <- nrow(dac_pts)
    div(
      div(paste0(n_dac, "/", total_dac), style = "font-size: 24px; font-weight: bold;"),
      div("DAC Tracts", style = "font-size: 12px; opacity: 0.9;")
    )
  })
  
  output$stat_monitoring <- renderUI({
    n_mon <- nrow(monitoring_sites_pts) + nrow(testing_sites_pts)
    div(
      div(n_mon, style = "font-size: 24px; font-weight: bold;"),
      div("Monitoring Sites", style = "font-size: 12px; opacity: 0.9;")
    )
  })
  
  output$stat_volume <- renderUI({
    proj_data <- filtered_stormwater()
    total_vol <- sum(proj_data$volume_addressed, na.rm = TRUE)
    div(
      div(paste0(format(round(total_vol, 1), big.mark = ","), " ac-ft"), 
          style = "font-size: 24px; font-weight: bold;"),
      div("Volume Addressed", style = "font-size: 12px; opacity: 0.9;")
    )
  })
  
  # main map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street") %>%
      addPolygons(data = la_county_geo,
                  color = htb_colors$coal_gray, weight = 2.5, fillOpacity = 0,
                  group = "LA County", label = ~NAME,
                  options = pathOptions(interactive = FALSE)) %>%
      setView(lng = -118.25, lat = 34.05, zoom = 10) %>%
      addLayersControl(
        baseGroups = c("Light", "Satellite", "Street"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "miles",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmiles"
      )
  })
  
  # observer for dynamic map layer toggling
  observe({
    # Get current layer selections
    layers <- input$map_layers
    
    proxy <- leafletProxy("map")
    
    # Clear all dynamic groups
    proxy %>%
      clearGroup("Stormwater Projects") %>%
      clearGroup("Disadvantaged Communities") %>%
      clearGroup("Beach Monitoring Sites") %>%
      clearGroup("River Monitoring Sites") %>%
      clearGroup("LA County Precip Stations")
    
    show <- function(x) x %in% layers
    
    # Disadvantaged Communities - render first (underneath other layers)
    if (show("Disadvantaged Communities")) {
      dac_filtered <- filtered_dac()
      
      if (nrow(dac_filtered) > 0) {
        # Create color palette based on CES percentile
        dac_pal <- colorNumeric(
          palette = c("#fc8d59", "#d73027"),
          domain = c(75, 100)
        )
        
        proxy <- proxy %>%
          addCircleMarkers(
            data = dac_filtered, 
            lng = ~lon, lat = ~lat,
            radius = 6, 
            color = "#FFFFFF",
            fillColor = ~dac_pal(ces_percentile), 
            fillOpacity = 0.7, 
            weight = 1,
            popup = ~paste0(
              "<div style='font-family: Roboto, sans-serif; min-width: 220px;'>",
              "<div style='background: linear-gradient(135deg, #d73027 0%, #fc8d59 100%); ",
              "color: white; padding: 8px; margin: -12px -16px 8px -16px; border-radius: 8px 8px 0 0;'>",
              "<strong>Disadvantaged Community</strong></div>",
              "<table style='font-size: 12px; width: 100%;'>",
              "<tr><td style='color: #666;'>Location:</td><td style='text-align: right;'><b>", location, "</b></td></tr>",
              "<tr><td style='color: #666;'>Census Tract:</td><td style='text-align: right;'>", tract_id, "</td></tr>",
              "<tr><td style='color: #666;'>ZIP Code:</td><td style='text-align: right;'>", zip, "</td></tr>",
              "<tr><td style='color: #666;'>Population:</td><td style='text-align: right;'>", population_fmt, "</td></tr>",
              "<tr><td colspan='2' style='padding-top: 8px; border-top: 1px solid #e0e0e0;'></td></tr>",
              "<tr><td style='color: #666;'>CES 4.0 Score:</td><td style='text-align: right;'><b>", ces_score_fmt, "</b></td></tr>",
              "<tr><td style='color: #666;'>CES Percentile:</td><td style='text-align: right;'><b>", round(ces_percentile, 1), "%</b></td></tr>",
              "<tr><td style='color: #666;'>Category:</td><td style='text-align: right;'>", dac_category_short, "</td></tr>",
              "</table>",
              "</div>"
            ),
            group = "Disadvantaged Communities"
          )
      }
    }
    
    # Stormwater Projects - using new combined filtered data
    if (show("Stormwater Projects")) {
      proj_data <- filtered_stormwater()
      
      if (nrow(proj_data) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data = proj_data, 
            lng = ~lon, lat = ~lat,
            radius = 7, 
            color = "#FFFFFF",
            fillColor = htb_colors$algae, 
            fillOpacity = 0.85, 
            weight = 2,
            popup = ~paste0(
              "<div style='font-family: Roboto, sans-serif; min-width: 200px;'>",
              "<strong style='color: #005CB9; font-size: 14px;'>", name, "</strong>",
              "<hr style='margin: 8px 0; border-color: #e0e0e0;'>",
              "<table style='font-size: 12px; width: 100%;'>",
              "<tr><td style='color: #666;'>Type:</td><td style='text-align: right;'><b>", project_type_clean, "</b></td></tr>",
              "<tr><td style='color: #666;'>Volume:</td><td style='text-align: right;'>", volume_fmt, "</td></tr>",
              "<tr><td style='color: #666;'>Capital Cost:</td><td style='text-align: right;'>", capital_cost_fmt, "</td></tr>",
              ifelse(!is.na(completion_date), 
                     paste0("<tr><td style='color: #666;'>Completed:</td><td style='text-align: right;'>", completion_date, "</td></tr>"),
                     ""),
              "</table>",
              "</div>"
            ),
            group = "Stormwater Projects"
          )
      }
    }
    
    # Beach Monitoring Sites
    if (show("Beach Monitoring Sites")) {
      beach_sites <- tryCatch({
        monitoring_sites_pts[la_county_geo, op = st_within]
      }, error = function(e) monitoring_sites_pts)
      
      if (nrow(beach_sites) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data = beach_sites, 
            lng = ~lon, lat = ~lat,
            radius = 8, 
            color = "#FFFFFF",
            fillColor = htb_colors$htb_blue, 
            fillOpacity = 0.85, 
            weight = 2,
            popup = ~paste0(
              "<div style='font-family: Roboto, sans-serif;'>",
              "<strong style='color: #005CB9;'>", location_name, "</strong>",
              "</div>"
            ),
            group = "Beach Monitoring Sites"
          )
      }
    }
    
    # River Monitoring Sites
    if (show("River Monitoring Sites")) {
      if (nrow(testing_sites_pts) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data = testing_sites_pts, 
            lng = ~lon, lat = ~lat,
            radius = 8, 
            color = "#FFFFFF",
            fillColor = htb_colors$sunset_pink, 
            fillOpacity = 0.85, 
            weight = 2,
            popup = ~paste0(
              "<div style='font-family: Roboto, sans-serif;'>",
              "<strong style='color: #005CB9;'>", name, "</strong><br>",
              "<span style='font-size: 11px;'>Order in watershed: ", order_in_ws, "</span>",
              "</div>"
            ),
            group = "River Monitoring Sites"
          )
      }
    }
    
    # LA County Precipitation Stations
    if (show("LA County Precip Stations")) {
      if (nrow(la_prec_stations_pts) > 0) {
        proxy <- proxy %>%
          addCircleMarkers(
            data = la_prec_stations_pts, 
            lng = ~lon, lat = ~lat,
            radius = 8, 
            color = "#FFFFFF",
            fillColor = htb_colors$sunshine, 
            fillOpacity = 0.85, 
            weight = 2,
            popup = ~paste0(
              "<div style='font-family: Roboto, sans-serif;'>",
              "<strong style='color: #005CB9;'>", name, "</strong>",
              "</div>"
            ),
            group = "LA County Precip Stations"
          )
      }
    }
  })
  
  # Separate observer for watershed boundaries (handles the filter independently)
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Watershed Boundaries")
    
    # Check if watersheds should be shown
    if (!("Watershed Boundaries" %in% input$map_layers)) return()
    if (is.null(fixed_ws)) return()
    
    # Get selected watersheds (use all if none selected yet)
    selected_ws <- input$selected_watersheds
    if (is.null(selected_ws) || length(selected_ws) == 0) {
      # On initial load, show all watersheds
      selected_ws <- unique(fixed_ws$LABEL)
    }
    
    # Filter watershed data
    filtered_ws <- fixed_ws %>%
      filter(LABEL %in% selected_ws)
    
    if (nrow(filtered_ws) > 0) {
      proxy %>%
        addPolygons(
          data = filtered_ws,
          weight = 2,
          color = htb_colors$ocean_blue,
          fillColor = htb_colors$light_aqua,
          fillOpacity = 0.3,
          smoothFactor = 0.5,
          label = ~LABEL,
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Roboto, sans-serif",
              "font-weight" = "bold",
              "color" = "#005CB9",
              "padding" = "4px 8px"
            )
          ),
          group = "Watershed Boundaries",
          highlightOptions = highlightOptions(
            weight = 4,
            color = htb_colors$coal_gray,
            fillOpacity = 0.5,
            bringToFront = FALSE
          ),
          popup = ~paste0(
            "<div style='font-family: Roboto, sans-serif;'>",
            "<strong style='color: #005CB9; font-size: 14px;'>", LABEL, "</strong>",
            "</div>"
          )
        )
    }
  })
  
  # Trend plot with Heal the Bay styling
  output$trend_plot <- renderPlot({
    df <- fib_long %>%
      filter(location_name == input$site_select,
             parameter == input$param_select,
             between(year(date), input$year_range[1], input$year_range[2]))
    req(nrow(df) > 0)
    
    ggplot(df, aes(date, result)) +
      geom_line(color = htb_colors$htb_blue, linewidth = 1) + 
      geom_point(color = htb_colors$ocean_blue, size = 2.5) +
      labs(x = "Date", y = "Result (MPN/100mL)", 
           title = paste("Water Quality at", input$site_select)) + 
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "sans"),
        plot.title = element_text(color = htb_colors$ocean_blue, 
                                  face = "bold", size = 16),
        axis.title = element_text(color = htb_colors$coal_gray, face = "bold"),
        axis.text = element_text(color = htb_colors$coal_gray),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e0e0e0")
      )
  })
  
  # Seasonal plot with Heal the Bay colors
  output$seasonal_plot <- renderPlot({
    df <- fib_long %>%
      filter(location_name == input$site_select,
             parameter == input$param_select,
             between(year(date), input$year_range[1], input$year_range[2]))
    req(nrow(df) > 0)
    
    seasonal_colors <- c(
      "Winter" = htb_colors$ocean_blue,
      "Spring" = htb_colors$algae,
      "Summer" = htb_colors$sunshine,
      "Fall" = htb_colors$sunset_pink
    )
    
    df %>%
      mutate(season = factor(case_when(
        month(date) %in% c(12,1,2) ~ "Winter",
        month(date) %in% c(3,4,5) ~ "Spring",
        month(date) %in% c(6,7,8) ~ "Summer",
        TRUE ~ "Fall"
      ), levels = c("Winter", "Spring", "Summer", "Fall"))) %>%
      group_by(season) %>%
      summarise(avg = mean(as.numeric(result), na.rm = TRUE)) %>%
      ggplot(aes(season, avg, fill = season)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      scale_fill_manual(values = seasonal_colors) +
      labs(x = "Season", y = "Average Result (MPN/100mL)",
           title = "Seasonal Water Quality Patterns") +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "sans"),
        plot.title = element_text(color = htb_colors$ocean_blue, 
                                  face = "bold", size = 16),
        axis.title = element_text(color = htb_colors$coal_gray, face = "bold"),
        axis.text = element_text(color = htb_colors$coal_gray),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#e0e0e0")
      )
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(fib_data, 
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                searching = TRUE
              ),
              class = 'stripe hover',
              rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("htb_fib_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(fib_data, file)
    }
  )
}

# --- Run App ---
shinyApp(ui, server)
