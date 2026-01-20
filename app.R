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

# --- Custom CSS for Heal the Bay Theme - CLEAN SOLID COLORS ---
htb_css <- "
/* Heal the Bay Custom Theme - Clean Solid Colors */
@import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&family=Source+Sans+Pro:wght@400;600&display=swap');

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
  --htb-deep-sea: #0E4C90;
  --htb-sand: #F3DAAB;
  --htb-kelp: #546122;
  --htb-garibaldi: #F47E48;
}

body {
  font-family: 'Source Sans Pro', 'Helvetica Neue', Helvetica, Arial, sans-serif;
  background-color: #f5f7f9;
  color: var(--htb-coal-gray);
}

/* Navbar - Solid Color - COMPREHENSIVE FIX */
.navbar, 
.navbar-default, 
.navbar-static-top, 
.navbar.bg-primary,
.navbar.navbar-expand-md,
.navbar.navbar-light,
.navbar.navbar-dark,
nav.navbar {
  background-color: #0E4C90 !important;
  background: #0E4C90 !important;
  border: none !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.12);
  padding: 0.5rem 1rem;
}

.bg-primary, .navbar .bg-primary {
  background-color: #0E4C90 !important;
  background: #0E4C90 !important;
}

.navbar > .container-fluid {
  background-color: #0E4C90 !important;
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
  font-weight: 600;
  font-family: 'Montserrat', sans-serif;
  padding: 0.75rem 1.25rem !important;
  transition: all 0.2s ease;
  border-radius: 6px;
  margin: 0 4px;
  text-transform: uppercase;
  font-size: 0.85rem;
  letter-spacing: 0.5px;
}

.navbar .navbar-nav .nav-link:hover {
  color: var(--htb-white) !important;
  background-color: rgba(255,255,255,0.15);
}

.navbar .navbar-nav .nav-link.active {
  color: var(--htb-coal-gray) !important;
  background-color: var(--htb-sunshine);
  font-weight: 700;
}

/* Page Headers */
h1, h2, h3, h4, h5, h6 {
  font-family: 'Montserrat', Georgia, serif;
  color: var(--htb-coal-gray);
}

h2 {
  color: var(--htb-deep-sea);
  border-bottom: 4px solid var(--htb-aqua);
  padding-bottom: 0.75rem;
  margin-bottom: 1.5rem;
  font-weight: 700;
}

/* Cards and Panels */
.card, .well, .panel {
  border: none;
  border-radius: 12px;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  background-color: var(--htb-white);
  overflow: hidden;
}

.card-header {
  background-color: var(--htb-aqua);
  color: var(--htb-white);
  font-family: 'Montserrat', Georgia, serif;
  font-weight: 700;
  border-radius: 12px 12px 0 0 !important;
}

/* Sidebar Styling */
.sidebar {
  background-color: var(--htb-white);
  border-radius: 12px;
  padding: 1.5rem;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  border-left: 4px solid var(--htb-algae);
}

/* Form Controls */
.form-control, .selectize-input, .shiny-input-container select {
  border: 2px solid #e0e0e0;
  border-radius: 8px;
  padding: 0.6rem 0.85rem;
  transition: all 0.2s ease;
  background-color: #fafbfc;
}

.form-control:focus, .selectize-input.focus {
  border-color: var(--htb-aqua);
  box-shadow: 0 0 0 3px rgba(0, 182, 182, 0.12);
  outline: none;
  background-color: var(--htb-white);
}

.shiny-input-container label {
  font-weight: 600;
  color: var(--htb-coal-gray);
  margin-bottom: 0.5rem;
  font-family: 'Montserrat', sans-serif;
  font-size: 0.9rem;
}

/* Buttons */
.btn-primary {
  background-color: var(--htb-aqua);
  border: none;
  border-radius: 8px;
  padding: 0.7rem 1.75rem;
  font-weight: 600;
  font-family: 'Montserrat', sans-serif;
  transition: all 0.2s ease;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  font-size: 0.85rem;
}

.btn-primary:hover {
  background-color: var(--htb-ocean-blue);
}

.btn-default, .btn-secondary {
  background-color: var(--htb-white);
  border: 2px solid var(--htb-aqua);
  color: var(--htb-aqua);
  border-radius: 8px;
  font-weight: 600;
  transition: all 0.2s ease;
}

.btn-default:hover, .btn-secondary:hover {
  background-color: var(--htb-aqua);
  color: var(--htb-white);
}

/* Checkbox and Radio Inputs */
.checkbox-inline, .radio-inline {
  padding: 0.6rem 1.1rem;
  margin-right: 0.5rem;
  background-color: var(--htb-white);
  border: 2px solid #e8eaed;
  border-radius: 25px;
  transition: all 0.2s ease;
  font-weight: 500;
}

.checkbox-inline:hover, .radio-inline:hover {
  border-color: var(--htb-aqua);
  background-color: rgba(0, 182, 182, 0.05);
}

/* Slider styling */
.irs--shiny .irs-bar {
  background-color: var(--htb-sunset-pink);
  border: none;
  height: 8px;
}

.irs--shiny .irs-line {
  background: #e8eaed;
  height: 8px;
  border-radius: 4px;
}

.irs--shiny .irs-handle {
  background-color: var(--htb-sunset-pink);
  border: 3px solid var(--htb-white);
  box-shadow: 0 2px 6px rgba(242, 104, 89, 0.3);
  width: 22px;
  height: 22px;
  top: 22px;
}

.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background-color: var(--htb-sunset-pink);
  border-radius: 6px;
  padding: 3px 8px;
  font-weight: 600;
}

/* Tab styling */
.nav-tabs {
  border-bottom: 3px solid #e8eaed;
}

.nav-tabs .nav-link {
  color: var(--htb-coal-gray);
  border: none;
  border-bottom: 4px solid transparent;
  padding: 0.85rem 1.5rem;
  font-weight: 600;
  font-family: 'Montserrat', sans-serif;
  transition: all 0.2s ease;
  margin-bottom: -3px;
}

.nav-tabs .nav-link:hover {
  color: var(--htb-aqua);
  border-bottom-color: var(--htb-light-aqua);
  background-color: transparent;
}

.nav-tabs .nav-link.active {
  color: var(--htb-kelp);
  border-bottom: 4px solid var(--htb-algae);
  background-color: transparent;
  font-weight: 700;
}

/* DataTable Styling */
.dataTables_wrapper {
  padding: 1.25rem;
}

table.dataTable thead th {
  background-color: var(--htb-algae);
  color: var(--htb-white);
  font-weight: 600;
  font-family: 'Montserrat', sans-serif;
  border: none !important;
  padding: 14px 12px;
  text-transform: uppercase;
  font-size: 0.8rem;
  letter-spacing: 0.5px;
}

table.dataTable tbody tr:nth-child(even) {
  background-color: rgba(144, 184, 62, 0.06);
}

table.dataTable tbody tr:hover {
  background-color: rgba(144, 184, 62, 0.12) !important;
}

.dataTables_filter input {
  border: 2px solid #e0e0e0;
  border-radius: 8px;
  padding: 0.5rem 0.85rem;
}

.dataTables_filter input:focus {
  border-color: var(--htb-algae);
  outline: none;
  box-shadow: 0 0 0 3px rgba(144, 184, 62, 0.12);
}

/* Map container */
.leaflet-container {
  border-radius: 12px;
  box-shadow: 0 2px 15px rgba(0,0,0,0.1);
}

/* Welcome/Overview section */
.welcome-section {
  background-color: var(--htb-deep-sea);
  color: var(--htb-white);
  padding: 3rem;
  border-radius: 16px;
  margin-bottom: 2rem;
  box-shadow: 0 4px 20px rgba(14, 76, 144, 0.25);
}

.welcome-section h2 {
  color: var(--htb-white);
  border-bottom: none;
  margin-bottom: 1rem;
}

/* Feature Cards */
.feature-card {
  background-color: var(--htb-white);
  border-radius: 12px;
  padding: 1.75rem;
  margin-bottom: 1rem;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  transition: all 0.2s ease;
  border-top: 5px solid var(--htb-aqua);
}

.feature-card:hover {
  transform: translateY(-3px);
  box-shadow: 0 6px 20px rgba(0,0,0,0.1);
}

.feature-card.accent-algae {
  border-top-color: var(--htb-algae);
}

.feature-card.accent-sunset {
  border-top-color: var(--htb-sunset-pink);
}

.feature-card h4 {
  color: var(--htb-deep-sea);
  margin-bottom: 0.75rem;
  font-weight: 700;
}

.feature-card p {
  color: #5a6a7a;
  margin-bottom: 0;
  line-height: 1.6;
}

/* Feature Icons */
.feature-icon {
  width: 56px;
  height: 56px;
  background-color: var(--htb-aqua);
  border-radius: 14px;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-bottom: 1.25rem;
  color: var(--htb-white);
  font-size: 1.4rem;
}

.feature-icon.icon-algae {
  background-color: var(--htb-algae);
}

.feature-icon.icon-sunset {
  background-color: var(--htb-sunset-pink);
}

/* Footer */
.footer {
  background-color: var(--htb-coal-gray);
  color: var(--htb-white);
  padding: 2rem;
  margin-top: 2rem;
  border-radius: 12px 12px 0 0;
  text-align: center;
}

/* Download button */
.btn-download {
  background-color: var(--htb-sunshine);
  border: none;
  color: var(--htb-coal-gray);
  padding: 0.7rem 1.75rem;
  border-radius: 8px;
  font-weight: 700;
  font-family: 'Montserrat', sans-serif;
  transition: all 0.2s ease;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.btn-download:hover {
  background-color: var(--htb-garibaldi);
  color: var(--htb-white);
}

/* Responsive adjustments */
@media (max-width: 768px) {
  .navbar-brand span {
    font-size: 14px !important;
  }
  
  .welcome-section {
    padding: 1.75rem;
  }
  
  .feature-card {
    padding: 1.25rem;
  }
}

/* Plot styling */
.shiny-plot-output {
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  background: var(--htb-white);
  padding: 1rem;
}

/* Info boxes */
.info-box {
  background-color: rgba(0, 182, 182, 0.08);
  border-left: 5px solid var(--htb-aqua);
  padding: 1.25rem 1.75rem;
  border-radius: 0 12px 12px 0;
  margin-bottom: 1rem;
}

.info-box p {
  margin-bottom: 0;
  color: var(--htb-coal-gray);
  line-height: 1.6;
}

.info-box.accent-algae {
  background-color: rgba(144, 184, 62, 0.08);
  border-left-color: var(--htb-algae);
}

.info-box.accent-sunset {
  background-color: rgba(242, 104, 89, 0.08);
  border-left-color: var(--htb-sunset-pink);
}

.info-box.accent-sunshine {
  background-color: rgba(252, 199, 85, 0.1);
  border-left-color: var(--htb-sunshine);
}

/* Map Controls Panel */
.map-controls-panel {
  max-height: calc(100vh - 200px);
  overflow-y: auto;
  background-color: var(--htb-white);
}

.map-controls-panel h4 {
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-family: 'Montserrat', sans-serif;
  color: var(--htb-deep-sea);
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 2px solid var(--htb-light-aqua);
}

.map-controls-panel .checkbox label {
  display: flex;
  align-items: center;
  padding: 10px 14px;
  margin: 5px 0;
  background-color: #f8f9fa;
  border-radius: 8px;
  transition: all 0.2s ease;
  cursor: pointer;
  border: 2px solid transparent;
}

.map-controls-panel .checkbox label:hover {
  background-color: rgba(0, 182, 182, 0.08);
  border-color: var(--htb-light-aqua);
}

.map-controls-panel .checkbox input:checked + span {
  font-weight: 600;
  color: var(--htb-deep-sea);
}

/* Map Legend */
.map-legend {
  padding: 0.5rem 0;
}

/* Leaflet customizations */
.leaflet-container {
  font-family: 'Source Sans Pro', sans-serif;
}

.leaflet-popup-content-wrapper {
  border-radius: 12px;
  box-shadow: 0 4px 16px rgba(0,0,0,0.15);
  border-top: 4px solid var(--htb-aqua);
}

.leaflet-popup-content {
  margin: 14px 18px;
  font-family: 'Source Sans Pro', sans-serif;
}

.leaflet-popup-tip {
  box-shadow: 0 4px 16px rgba(0,0,0,0.15);
}

.leaflet-control-layers {
  border-radius: 10px !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.1) !important;
  border: none !important;
}

.leaflet-control-layers-toggle {
  width: 40px !important;
  height: 40px !important;
}

.leaflet-control-scale-line {
  border-color: var(--htb-coal-gray) !important;
  background: rgba(255,255,255,0.9) !important;
}

.leaflet-bar a {
  border-radius: 6px !important;
  border: none !important;
  box-shadow: 0 2px 6px rgba(0,0,0,0.12) !important;
  color: var(--htb-coal-gray) !important;
}

.leaflet-bar a:hover {
  background-color: var(--htb-light-algae) !important;
}

/* Select all / Clear all links */
.map-controls-panel a {
  color: var(--htb-aqua);
  text-decoration: none;
  font-weight: 600;
  transition: color 0.2s ease;
}

.map-controls-panel a:hover {
  color: var(--htb-algae);
  text-decoration: underline;
}

/* Selectize dropdown styling */
.selectize-dropdown {
  border: 2px solid var(--htb-aqua) !important;
  border-radius: 0 0 8px 8px !important;
  box-shadow: 0 4px 16px rgba(0,0,0,0.1) !important;
}

.selectize-dropdown .active {
  background-color: rgba(144, 184, 62, 0.12) !important;
  color: var(--htb-kelp) !important;
}

.selectize-input.focus {
  border-color: var(--htb-aqua) !important;
  box-shadow: 0 0 0 3px rgba(0, 182, 182, 0.12) !important;
}

.selectize-input .item {
  background-color: var(--htb-aqua) !important;
  color: white !important;
  border: none !important;
  border-radius: 6px !important;
  padding: 3px 10px !important;
  font-weight: 500;
}

.selectize-input .item .remove {
  color: rgba(255,255,255,0.8) !important;
  border-left-color: rgba(255,255,255,0.3) !important;
}

.selectize-input .item .remove:hover {
  color: white !important;
  background: rgba(0,0,0,0.1) !important;
}

/* Stats bar */
.stats-bar {
  display: flex;
  justify-content: space-around;
  padding: 1.25rem 1.5rem;
  border-radius: 12px;
  margin-top: 1rem;
  background: var(--htb-white);
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  border-top: 4px solid var(--htb-aqua);
}

.stat-item {
  text-align: center;
  padding: 0.5rem 1rem;
  position: relative;
}

.stat-item:not(:last-child)::after {
  content: '';
  position: absolute;
  right: 0;
  top: 20%;
  height: 60%;
  width: 1px;
  background-color: #e0e0e0;
}

.stat-value {
  font-size: 1.75rem;
  font-weight: 700;
  font-family: 'Montserrat', sans-serif;
  color: var(--htb-deep-sea);
}

.stat-item:nth-child(2) .stat-value {
  color: var(--htb-sunset-pink);
}

.stat-item:nth-child(3) .stat-value {
  color: var(--htb-algae);
}

.stat-item:nth-child(4) .stat-value {
  color: var(--htb-garibaldi);
}

.stat-label {
  font-size: 0.75rem;
  color: #6a7a8a;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  font-weight: 600;
  margin-top: 0.25rem;
}

/* Legend card styling */
.legend-card {
  background-color: var(--htb-white);
  border-top: 4px solid var(--htb-sunshine);
}

.legend-card h4 {
  border-bottom-color: var(--htb-light-sunshine) !important;
}
"

# --- Theme using bslib ---
app_theme <- bs_theme(
  version = 5,
  bg = "#f5f7f9",
  fg = "#263746",
  primary = "#0E4C90",
  secondary = "#90B83E",
  success = "#90B83E",
  info = "#40B4E5",
  warning = "#FCC755",
  danger = "#F26859",
  base_font = font_google("Source Sans Pro"),
  heading_font = font_google("Montserrat"),
  font_scale = 1.05,
  "navbar-bg" = "#0E4C90",
  "navbar-light-bg" = "#0E4C90",
  "navbar-dark-bg" = "#0E4C90"
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
                          div(class = "feature-icon icon-algae", icon("chart-line")),
                          h4("Trend Analysis"),
                          p("Analyze water quality parameters over time with interactive 
               time series and seasonal pattern visualizations.")
                      )
               ),
               column(4,
                      div(class = "feature-card accent-sunset",
                          div(class = "feature-icon icon-sunset", icon("table")),
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
             # Header row
             fluidRow(
               column(12,
                      h2("Map View: Stormwater Projects & Monitoring Sites"),
                      div(class = "info-box accent-algae", style = "margin-bottom: 1.25rem;",
                          p("Explore stormwater capture projects and water quality monitoring sites 
                 across Los Angeles County. Use the filters to customize your view.")
                      )
               )
             ),
             
             fluidRow(
               # Map controls sidebar
               column(3,
                      div(class = "map-controls-panel",
                          style = "background: white; padding: 1.25rem; border-radius: 12px; 
                       box-shadow: 0 4px 15px rgba(0,0,0,0.08);",
                          
                          # Layer toggles
                          h4("Map Layers", style = "color: #0E4C90; margin-bottom: 1rem; 
                font-family: 'Montserrat', serif;"),
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
                          h4("DAC Filter (SB 535)", style = "color: #0E4C90; margin-bottom: 0.75rem;
                font-family: 'Montserrat', serif;"),
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
                          h4("Project Type Filter", style = "color: #0E4C90; margin-bottom: 0.75rem;
                font-family: 'Montserrat', serif;"),
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
                          h4("Watershed Filter", style = "color: #0E4C90; margin-bottom: 0.75rem;
                font-family: 'Montserrat', serif;"),
                          uiOutput("watershed_filter_ui")
                      )
               ),
               
               # Map display column
               column(9,
                      # Legend bar above the map
                      div(class = "legend-bar",
                          style = "background: white; padding: 0.85rem 1.25rem; border-radius: 12px 12px 0 0; 
                       box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                       border-top: 4px solid #FCC755;
                       display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap;",
                          
                          # Legend title
                          div(style = "display: flex; align-items: center; margin-right: 1.5rem;",
                              tags$span(class = "legend-title", 
                                        style = "font-family: 'Montserrat', sans-serif; font-weight: 700; 
                                 color: #0E4C90; font-size: 13px; text-transform: uppercase; 
                                 letter-spacing: 0.5px;",
                                        icon("map-marker-alt", style = "margin-right: 6px;"), "Legend")
                          ),
                          
                          # Legend items container
                          div(style = "display: flex; align-items: center; flex-wrap: wrap; gap: 1rem;",
                              # Stormwater
                              div(style = "display: flex; align-items: center;",
                                  span(style = paste0("width: 14px; height: 14px; border-radius: 50%; 
                          background-color: ", htb_colors$algae, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.25);")),
                                  span("Stormwater", style = "font-size: 12px; font-weight: 500; color: #444;")
                              ),
                              # Beach Sites
                              div(style = "display: flex; align-items: center;",
                                  span(style = paste0("width: 14px; height: 14px; border-radius: 50%; 
                          background-color: ", htb_colors$htb_blue, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.25);")),
                                  span("Beach Sites", style = "font-size: 12px; font-weight: 500; color: #444;")
                              ),
                              # River Sites
                              div(style = "display: flex; align-items: center;",
                                  span(style = paste0("width: 14px; height: 14px; border-radius: 50%; 
                          background-color: ", htb_colors$deep_sea, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.25);")),
                                  span("River Sites", style = "font-size: 12px; font-weight: 500; color: #444;")
                              ),
                              # Precip Stations
                              div(style = "display: flex; align-items: center;",
                                  span(style = paste0("width: 14px; height: 14px; border-radius: 50%; 
                          background-color: ", htb_colors$sunshine, "; display: inline-block; 
                          margin-right: 6px; border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.25);")),
                                  span("Precip", style = "font-size: 12px; font-weight: 500; color: #444;")
                              ),
                              # DAC Tracts with solid indicator
                              div(style = "display: flex; align-items: center;",
                                  span(style = "width: 14px; height: 14px; border-radius: 50%; 
                          background-color: #d73027; 
                          display: inline-block; margin-right: 6px; border: 2px solid white; 
                          box-shadow: 0 1px 3px rgba(0,0,0,0.25);"),
                                  span("DAC", style = "font-size: 12px; font-weight: 500; color: #444;")
                              ),
                              # Watersheds
                              div(style = "display: flex; align-items: center;",
                                  span(style = paste0("width: 14px; height: 14px; border: 2px solid ", 
                                                      htb_colors$ocean_blue, "; background-color: ", htb_colors$light_aqua, 
                                                      "; opacity: 0.9; display: inline-block; margin-right: 6px; border-radius: 3px;")),
                                  span("Watersheds", style = "font-size: 12px; font-weight: 500; color: #444;")
                              ),
                              # DAC percentile indicator
                              div(style = "display: flex; align-items: center; padding-left: 0.75rem; 
                                   border-left: 1px solid #e0e0e0; margin-left: 0.5rem;",
                                  span("DAC Percentile:", style = "font-size: 10px; color: #666; margin-right: 6px;"),
                                  span("75%", style = "font-size: 10px; color: #fc8d59; font-weight: 600;"),
                                  span(" → ", style = "font-size: 10px; color: #888;"),
                                  span("100%", style = "font-size: 10px; color: #d73027; font-weight: 600;")
                              )
                          )
                      ),
                      
                      # Map container
                      div(style = "background: white; padding: 0.5rem; border-radius: 0 0 12px 12px; 
                       box-shadow: 0 4px 20px rgba(0,0,0,0.1);",
                          leafletOutput("map", height = "580px")
                      ),
                      
                      # Map stats bar
                      div(class = "stats-bar",
                          div(class = "stat-item",
                              uiOutput("stat_projects")
                          ),
                          div(class = "stat-item",
                              uiOutput("stat_dac")
                          ),
                          div(class = "stat-item",
                              uiOutput("stat_monitoring")
                          ),
                          div(class = "stat-item",
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
               style = "background-color: white; border-radius: 12px; 
                 box-shadow: 0 4px 15px rgba(0,0,0,0.08); border-left: 4px solid #F26859;",
               h4("Filter Options", style = "color: #0E4C90; margin-bottom: 1.25rem;"),
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
               div(class = "info-box accent-sunset", style = "margin-top: 1.5rem;",
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
             div(style = "background: white; padding: 1.5rem; border-radius: 12px; 
                   box-shadow: 0 4px 15px rgba(0,0,0,0.08);",
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
                   background: #f8f9fa; border-radius: 8px;",
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
  
  # Stats outputs for the map - redesigned with new classes
  output$stat_projects <- renderUI({
    n_proj <- nrow(filtered_stormwater())
    div(
      div(class = "stat-value", n_proj),
      div(class = "stat-label", "Stormwater Projects")
    )
  })
  
  output$stat_dac <- renderUI({
    n_dac <- nrow(filtered_dac())
    total_dac <- nrow(dac_pts)
    div(
      div(class = "stat-value", paste0(n_dac, "/", total_dac)),
      div(class = "stat-label", "DAC Tracts")
    )
  })
  
  output$stat_monitoring <- renderUI({
    n_mon <- nrow(monitoring_sites_pts) + nrow(testing_sites_pts)
    div(
      div(class = "stat-value", n_mon),
      div(class = "stat-label", "Monitoring Sites")
    )
  })
  
  output$stat_volume <- renderUI({
    proj_data <- filtered_stormwater()
    total_vol <- sum(proj_data$volume_addressed, na.rm = TRUE)
    div(
      div(class = "stat-value", paste0(format(round(total_vol, 1), big.mark = ","))),
      div(class = "stat-label", "Acre-ft Addressed")
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
              "<div style='font-family: Source Sans Pro, sans-serif; min-width: 220px;'>",
              "<div style='background-color: #d73027; ",
              "color: white; padding: 10px; margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
              "<strong style='font-family: Montserrat, sans-serif;'>Disadvantaged Community</strong></div>",
              "<table style='font-size: 12px; width: 100%;'>",
              "<tr><td style='color: #666; padding: 3px 0;'>Location:</td><td style='text-align: right;'><b>", location, "</b></td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>Census Tract:</td><td style='text-align: right;'>", tract_id, "</td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>ZIP Code:</td><td style='text-align: right;'>", zip, "</td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>Population:</td><td style='text-align: right;'>", population_fmt, "</td></tr>",
              "<tr><td colspan='2' style='padding-top: 10px; border-top: 1px solid #e0e0e0;'></td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>CES 4.0 Score:</td><td style='text-align: right;'><b>", ces_score_fmt, "</b></td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>CES Percentile:</td><td style='text-align: right;'><b>", round(ces_percentile, 1), "%</b></td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>Category:</td><td style='text-align: right;'>", dac_category_short, "</td></tr>",
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
              "<div style='font-family: Source Sans Pro, sans-serif; min-width: 200px;'>",
              "<div style='background-color: #90B83E; ",
              "color: white; padding: 10px; margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
              "<strong style='font-family: Montserrat, sans-serif;'>", name, "</strong></div>",
              "<table style='font-size: 12px; width: 100%;'>",
              "<tr><td style='color: #666; padding: 3px 0;'>Type:</td><td style='text-align: right;'><b>", project_type_clean, "</b></td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>Volume:</td><td style='text-align: right;'>", volume_fmt, "</td></tr>",
              "<tr><td style='color: #666; padding: 3px 0;'>Capital Cost:</td><td style='text-align: right;'>", capital_cost_fmt, "</td></tr>",
              ifelse(!is.na(completion_date), 
                     paste0("<tr><td style='color: #666; padding: 3px 0;'>Completed:</td><td style='text-align: right;'>", completion_date, "</td></tr>"),
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
              "<div style='font-family: Source Sans Pro, sans-serif;'>",
              "<div style='background-color: #40B4E5; ",
              "color: white; padding: 10px; margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
              "<strong style='font-family: Montserrat, sans-serif;'>", location_name, "</strong></div>",
              "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Beach Monitoring Site</p>",
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
            fillColor = htb_colors$deep_sea, 
            fillOpacity = 0.85, 
            weight = 2,
            popup = ~paste0(
              "<div style='font-family: Source Sans Pro, sans-serif;'>",
              "<div style='background-color: #0E4C90; ",
              "color: white; padding: 10px; margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
              "<strong style='font-family: Montserrat, sans-serif;'>", name, "</strong></div>",
              "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Order in watershed: ", order_in_ws, "</p>",
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
              "<div style='font-family: Source Sans Pro, sans-serif;'>",
              "<div style='background-color: #FCC755; ",
              "color: #263746; padding: 10px; margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
              "<strong style='font-family: Montserrat, sans-serif;'>", name, "</strong></div>",
              "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Precipitation Station</p>",
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
              "font-family" = "Montserrat, sans-serif",
              "font-weight" = "bold",
              "color" = "#0E4C90",
              "padding" = "6px 10px"
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
            "<div style='font-family: Source Sans Pro, sans-serif;'>",
            "<div style='background-color: #005CB9; ",
            "color: white; padding: 10px; margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
            "<strong style='font-family: Montserrat, sans-serif;'>", LABEL, "</strong></div>",
            "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Watershed Boundary</p>",
            "</div>"
          )
        )
    }
  })
  
  # Trend plot with varied brand colors
  output$trend_plot <- renderPlot({
    df <- fib_long %>%
      filter(location_name == input$site_select,
             parameter == input$param_select,
             between(year(date), input$year_range[1], input$year_range[2]))
    req(nrow(df) > 0)
    
    ggplot(df, aes(date, result)) +
      geom_line(color = htb_colors$aqua, linewidth = 1.2) + 
      geom_point(color = htb_colors$deep_sea, size = 3, alpha = 0.8) +
      labs(x = "Date", y = "Result (MPN/100mL)", 
           title = paste("Water Quality at", input$site_select)) + 
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "sans"),
        plot.title = element_text(color = htb_colors$deep_sea, 
                                  face = "bold", size = 18),
        axis.title = element_text(color = htb_colors$coal_gray, face = "bold"),
        axis.text = element_text(color = htb_colors$coal_gray),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8eaed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # Seasonal plot with full brand color palette
  output$seasonal_plot <- renderPlot({
    df <- fib_long %>%
      filter(location_name == input$site_select,
             parameter == input$param_select,
             between(year(date), input$year_range[1], input$year_range[2]))
    req(nrow(df) > 0)
    
    # Using full brand palette for seasons
    seasonal_colors <- c(
      "Winter" = htb_colors$deep_sea,
      "Spring" = htb_colors$algae,
      "Summer" = htb_colors$sunshine,
      "Fall" = htb_colors$garibaldi
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
        plot.title = element_text(color = htb_colors$deep_sea, 
                                  face = "bold", size = 18),
        axis.title = element_text(color = htb_colors$coal_gray, face = "bold"),
        axis.text = element_text(color = htb_colors$coal_gray),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#e8eaed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
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