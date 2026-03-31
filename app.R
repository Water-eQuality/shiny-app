# --- Libraries ---
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(bslib)
library(lubridate)
library(DT)
library(scales)

# Helpful while debugging crashes:
options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)

# --- Helper functions (define ONCE, near the top) ---
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

validate_latlon <- function(df, lat = "lat", lon = "lon") {
  dplyr::filter(
    df,
    !is.na(.data[[lat]]), !is.na(.data[[lon]]),
    is.finite(.data[[lat]]), is.finite(.data[[lon]]),
    dplyr::between(.data[[lat]], -90, 90),
    dplyr::between(.data[[lon]], -180, 180)
  )
}


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
  validate_latlon(lat = "lat", lon = "lon") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)


# Get unique DAC categories for filtering
dac_categories <- sort(unique(dac_data$dac_category_short))

# --- Load LAUSD School Parcels (MAP-SAFE: cropped + simplified; geometry only is fine) ---
lausd_parcels <- tryCatch({
  gpkg_path <- "data/lausd_school_parcels/lausd_fixed.gpkg"
  
  if (!file.exists(gpkg_path)) {
    sf::gdal_utils(
      "vectortranslate",
      source         = "data/lausd_school_parcels/lausd_school_parcels.shp",
      destination    = gpkg_path,
      options        = c("-f", "GPKG"),
      config_options = c(SHAPE_RESTORE_SHX = "YES")
    )
  }
  
  x <- sf::read_sf(gpkg_path)
  
  # If CRS missing, set BEFORE transform (LAUSD is typically EPSG:3310)
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 3310
  
  # Clean geometry - do NOT cast yet, preserve original types
  x <- x %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
    sf::st_transform(4326) %>%
    sf::st_zm(drop = TRUE, what = "ZM")
  
  # Drop all columns except LABEL early
  x <- x %>% dplyr::select(LABEL)
  
  # Crop to LA County bbox
  if (!is.null(la_county_geo) && nrow(la_county_geo) > 0) {
    bb <- sf::st_bbox(la_county_geo)
    bb <- bb + c(xmin = -0.05, ymin = -0.05, xmax = 0.05, ymax = 0.05)
    x <- sf::st_crop(x, bb)
  }
  
  # Simplify
  x <- sf::st_simplify(x, dTolerance = 0.0003, preserveTopology = TRUE)
  
  # Make valid again after simplify, then extract polygons
  x <- x %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(sf::st_geometry(.)))
  
  # Only cast rows that are already polygon types - skip collections
  geom_types <- sf::st_geometry_type(x)
  x <- x[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]
  
  if (nrow(x) > 0) {
    x <- sf::st_cast(x, "MULTIPOLYGON", warn = FALSE)
  }
  
  # Explode geometry collections, keep only polygon types, recast
  x
}, error = function(e) {
  message("LAUSD parcels load failed: ", e$message)
  NULL
})


# --- Load Park Polygons ---
park_polygons <- tryCatch({
  
  Sys.setenv(SHAPE_RESTORE_SHX = "YES")
  
  x <- sf::read_sf("data/park_polygons/park_polygons.shp")
  
  # Assign CA Albers (EPSG:3310, meters) — no CRS in file
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 3310
  
  x <- x %>%
    # Fix validity BEFORE any casting or transforming
    sf::st_buffer(0) %>%                    
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
    sf::st_transform(4326) %>%
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    # Fix again after transform
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(sf::st_geometry(.))) %>%
    sf::st_simplify(dTolerance = 0.0003, preserveTopology = TRUE) %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(sf::st_geometry(.)))
  
  # Safe cast: only touch polygon types, skip anything else
  geom_types <- sf::st_geometry_type(x)
  x <- x[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]
  
  # Cast row by row to avoid one bad geometry killing the whole layer
  x <- tryCatch(
    sf::st_cast(x, "MULTIPOLYGON", warn = FALSE),
    error = function(e) {
      message("st_cast failed, returning as-is: ", e$message)
      x
    }
  )
  
  message("Park polygons loaded: ", nrow(x), " features")
  x
  
}, error = function(e) {
  message("Park polygons load failed: ", e$message)
  NULL
})

# --- Spatial point data ---

# NEW Combined stormwater projects as sf object
stormwater_pts <- combined_projects %>%
  validate_latlon(lat = "lat", lon = "lon") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Get unique project types for filtering
project_types <- sort(unique(combined_projects$project_type_clean))

# Legacy project points (keeping for backwards compatibility if needed)
projects_pts <- projects_data %>%
  rename(lat = Latitude, lon = Longitude, name = `Project Name`) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%   # ensure numeric
  validate_latlon(lat = "lat", lon = "lon") %>%              # stronger than filter(!is.na)
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# WRAMPS project points
wramps_pts <- read_csv("data/wramps_clean.csv", show_col_types = FALSE) %>%
  rename(lat = Latitude, lon = Longitude, name = project) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  validate_latlon(lat = "lat", lon = "lon") %>%              # replaces filter(!is.na)
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Monitoring sites points
monitoring_sites_pts <- site_info %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%   # safe guard
  validate_latlon(lat = "lat", lon = "lon") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Testing sites points
testing_sites_pts <- read_csv("data/testing_site_locations.csv", show_col_types = FALSE) %>%
  transmute(
    name = `Site Name`,
    lat = as.numeric(Latitude),
    lon = as.numeric(Longitude),
    order_in_ws = `Order in Watershed`
  ) %>%
  validate_latlon(lat = "lat", lon = "lon") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Precip stations points
la_prec_stations_pts <- {
  df <- read_csv("data/la_county_prec_stations.csv", show_col_types = FALSE)
  
  name_col <- pick_col(df, c("name", "station", "station name", "site", "site name")) %||% {
    df$name_tmp <- paste0("Station ", seq_len(nrow(df)))
    "name_tmp"
  }
  lat_col  <- pick_col(df, c("latitude", "lat", "y"))
  lon_col  <- pick_col(df, c("longitude", "long", "lon", "x"))
  
  df %>%
    transmute(
      name = .data[[name_col]],
      lat  = suppressWarnings(as.numeric(.data[[lat_col]])),
      lon  = suppressWarnings(as.numeric(.data[[lon_col]]))
    ) %>%
    validate_latlon(lat = "lat", lon = "lon") %>%            # replaces filter(!is.na)
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

.stat-item:nth-child(5) .stat-value {
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

/* Data Sources Page Styling */
.data-source-card {
  background-color: var(--htb-white);
  border-radius: 12px;
  padding: 2rem;
  margin-bottom: 1.5rem;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  transition: all 0.2s ease;
}

.data-source-card:hover {
  box-shadow: 0 6px 20px rgba(0,0,0,0.1);
}

.data-source-card h3 {
  color: var(--htb-deep-sea);
  font-weight: 700;
}

.citation-box code {
  background: transparent;
  color: inherit;
  font-size: inherit;
}

/* Collapsible filter panels */
details > summary {
  user-select: none;
}

details > summary::-webkit-details-marker {
  display: none;
}

details[open] > summary span {
  transform: rotate(180deg);
  display: inline-block;
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
  # ✅ use plain font family strings (works on older bslib)
  base_font = "Source Sans Pro",
  heading_font = "Montserrat",
  font_scale = 1.05,
  "navbar-bg" = "#0E4C90",
  "navbar-light-bg" = "#0E4C90",
  "navbar-dark-bg" = "#0E4C90"
)

# --- <- ---
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
                          
                          h4("Map Layers", style = "color: #0E4C90; margin-bottom: 1rem; 
          font-family: 'Montserrat', serif;"),
                          checkboxGroupInput("map_layers", NULL,
                                             choices = c(
                                               "Stormwater Projects",
                                               "Park Polygons",
                                               "LAUSD School Parcels",
                                               "Disadvantaged Communities",
                                               "Beach Monitoring Sites",
                                               "River Monitoring Sites",
                                               "LA County Precip Stations",
                                               "Watershed Boundaries"
                                             ),
                                             selected = c(
                                               "Stormwater Projects",
                                               "Park Polygons",
                                               "LAUSD School Parcels",
                                               "Disadvantaged Communities",
                                               "Watershed Boundaries"
                                             )),
                          
                          hr(style = "border-color: #e0e0e0; margin: 1rem 0;"),
                          
                          # --- DAC Filter (collapsible) ---
                          tags$details(
                            tags$summary(
                              style = "cursor: pointer; font-size: 13px; font-weight: 700;
                   text-transform: uppercase; letter-spacing: 1px;
                   font-family: 'Montserrat', sans-serif; color: #0E4C90;
                   padding: 0.5rem 0; list-style: none;
                   display: flex; align-items: center; justify-content: space-between;",
                              "DAC Filter (SB 535)",
                              tags$span(style = "font-size: 16px; color: #00B6B6;", HTML("&#9660;"))
                            ),
                            div(style = "padding-top: 0.75rem;",
                                div(class = "info-box accent-sunset", style = "margin-bottom: 0.85rem; padding: 0.85rem 1rem;",
                                    p(HTML("<strong>About this filter:</strong> Disadvantaged communities are identified 
                        using <strong>CalEnviroScreen 4.0</strong> scores, developed by the California 
                        Office of Environmental Health Hazard Assessment (OEHHA). Under <strong>SB 535</strong>, 
                        CalEPA designates these communities based on geographic, socioeconomic, public 
                        health, and environmental hazard criteria. Higher percentiles indicate greater 
                        cumulative pollution burden."),
                                      style = "margin: 0; font-size: 11px; line-height: 1.5;")
                                ),
                                p("Filter by CES 4.0 percentile:", 
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
                                div(style = "margin-top: 0.25rem;",
                                    actionLink("select_all_dac", "Select All", 
                                               style = "font-size: 12px; margin-right: 10px;"),
                                    actionLink("clear_all_dac", "Clear All", 
                                               style = "font-size: 12px;")
                                )
                            )
                          ),
                          
                          hr(style = "border-color: #e0e0e0; margin: 1rem 0;"),
                          
                          # --- Project Type Filter (collapsible) ---
                          tags$details(
                            tags$summary(
                              style = "cursor: pointer; font-size: 13px; font-weight: 700;
                   text-transform: uppercase; letter-spacing: 1px;
                   font-family: 'Montserrat', sans-serif; color: #0E4C90;
                   padding: 0.5rem 0; list-style: none;
                   display: flex; align-items: center; justify-content: space-between;",
                              "Project Type Filter",
                              tags$span(style = "font-size: 16px; color: #00B6B6;", HTML("&#9660;"))
                            ),
                            div(style = "padding-top: 0.75rem;",
                                div(class = "info-box accent-algae", style = "margin-bottom: 0.85rem; padding: 0.85rem 1rem;",
                                    p(HTML("<strong>About this filter:</strong> Stormwater capture projects are categorized 
                        by infrastructure type. Projects range from small-scale <strong>green streets</strong> 
                        and <strong>bioretention</strong> cells to large <strong>regional infiltration</strong> 
                        facilities and <strong>treatment systems</strong>. Each type captures, treats, or 
                        diverts stormwater runoff to reduce pollution and replenish groundwater."),
                                      style = "margin: 0; font-size: 11px; line-height: 1.5;")
                                ),
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
                                div(style = "margin-top: 0.25rem;",
                                    actionLink("select_all_types", "Select All", 
                                               style = "font-size: 12px; margin-right: 10px;"),
                                    actionLink("clear_all_types", "Clear All", 
                                               style = "font-size: 12px;")
                                )
                            )
                          ),
                          
                          hr(style = "border-color: #e0e0e0; margin: 1rem 0;"),
                          
                          # --- Watershed Filter (collapsible) ---
                          tags$details(
                            tags$summary(
                              style = "cursor: pointer; font-size: 13px; font-weight: 700;
                   text-transform: uppercase; letter-spacing: 1px;
                   font-family: 'Montserrat', sans-serif; color: #0E4C90;
                   padding: 0.5rem 0; list-style: none;
                   display: flex; align-items: center; justify-content: space-between;",
                              "Watershed Filter",
                              tags$span(style = "font-size: 16px; color: #00B6B6;", HTML("&#9660;"))
                            ),
                            div(style = "padding-top: 0.75rem;",
                                div(class = "info-box", style = "margin-bottom: 0.85rem; padding: 0.85rem 1rem;",
                                    p(HTML("<strong>About this filter:</strong> Watersheds define the land area that drains 
                        to a common outlet. LA County spans multiple major watersheds including the 
                        <strong>LA River</strong>, <strong>Ballona Creek</strong>, and <strong>San Gabriel River</strong> 
                        systems. Filtering by watershed helps identify which stormwater projects and 
                        monitoring sites fall within a given drainage area."),
                                      style = "margin: 0; font-size: 11px; line-height: 1.5;")
                                ),
                                uiOutput("watershed_filter_ui")
                            )
                          )
                      )
               ),
               
               # Map display column
               column(9,
                      # Legend bar above the map
                      div(
                        style = "background: white; padding: 1rem 1.5rem; border-radius: 12px 12px 0 0;
           box-shadow: 0 2px 10px rgba(0,0,0,0.06); border-top: 4px solid #FCC755;",
                        
                        div(style = "font-family: 'Montserrat', sans-serif; font-weight: 700; color: #0E4C90;
               font-size: 12px; text-transform: uppercase; letter-spacing: 0.5px;
               margin-bottom: 0.75rem;",
                            icon("map-marker-alt", style = "margin-right: 6px;"), "Legend"
                        ),
                        
                        div(style = "display: flex; flex-wrap: wrap; gap: 0.6rem;",
                            
                            # Point layers
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = paste0("width: 16px; height: 16px; border-radius: 50%;
               background-color: ", htb_colors$algae, "; display: inline-block;
               border: 2px solid white; box-shadow: 0 1px 4px rgba(0,0,0,0.3); flex-shrink: 0;")),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "Stormwater"),
                                  div(style = "font-size: 10px; color: #888;", "Capture projects")
                                )
                            ),
                            
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = "width: 16px; height: 16px; border-radius: 3px;
               background-color: #F47E48; display: inline-block;
               border: 2px solid white; box-shadow: 0 1px 4px rgba(0,0,0,0.3); flex-shrink: 0;"),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "LAUSD"),
                                  div(style = "font-size: 10px; color: #888;", "School parcels")
                                )
                            ),
                            
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = paste0("width: 16px; height: 16px; border-radius: 50%;
               background-color: ", htb_colors$htb_blue, "; display: inline-block;
               border: 2px solid white; box-shadow: 0 1px 4px rgba(0,0,0,0.3); flex-shrink: 0;")),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "Beach Sites"),
                                  div(style = "font-size: 10px; color: #888;", "FIB monitoring")
                                )
                            ),
                            
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = paste0("width: 16px; height: 16px; border-radius: 50%;
               background-color: ", htb_colors$deep_sea, "; display: inline-block;
               border: 2px solid white; box-shadow: 0 1px 4px rgba(0,0,0,0.3); flex-shrink: 0;")),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "River Sites"),
                                  div(style = "font-size: 10px; color: #888;", "FIB monitoring")
                                )
                            ),
                            
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = paste0("width: 16px; height: 16px; border-radius: 50%;
               background-color: ", htb_colors$sunshine, "; display: inline-block;
               border: 2px solid white; box-shadow: 0 1px 4px rgba(0,0,0,0.3); flex-shrink: 0;")),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "Precip"),
                                  div(style = "font-size: 10px; color: #888;", "Rain stations")
                                )
                            ),
                            
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = "width: 16px; height: 16px; border-radius: 3px;
               background-color: #2ca25f; display: inline-block;
               border: 2px solid white; box-shadow: 0 1px 4px rgba(0,0,0,0.3); flex-shrink: 0;"),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "Parks"),
                                  div(style = "font-size: 10px; color: #888;", "LA County parks")
                                )
                            ),
                            
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = paste0("width: 16px; height: 16px; border: 2px solid ",
                                                    htb_colors$ocean_blue, "; background-color: ", htb_colors$light_aqua,
                                                    "; display: inline-block; border-radius: 3px; flex-shrink: 0;")),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "Watersheds"),
                                  div(style = "font-size: 10px; color: #888;", "Drainage boundaries")
                                )
                            ),
                            
                            # DAC gradient pill
                            div(style = "display: flex; align-items: center; background: #f8f9fa;
                   border-radius: 8px; padding: 0.4rem 0.75rem; gap: 8px;",
                                span(style = "width: 32px; height: 16px; border-radius: 4px; flex-shrink: 0;
               background: linear-gradient(to right, #fc8d59, #d73027);
               border: 1px solid #ccc; display: inline-block;"),
                                div(
                                  div(style = "font-size: 12px; font-weight: 600; color: #263746; line-height: 1.2;", "DAC"),
                                  div(style = "font-size: 10px; color: #888;", "75% to 100% CES")
                                )
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
                              uiOutput("stat_lausd")
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
  
  # Data Sources Tab
  tabPanel("Data Sources",
           fluidPage(
             h2("Data Sources & Citations"),
             div(class = "info-box", style = "margin-bottom: 1.5rem;",
                 p(HTML("This dashboard integrates data from multiple authoritative sources. 
                        Below you'll find citation information, data descriptions, and links 
                        to the original sources for transparency and reproducibility."))
             ),
             
             # Beach Monitoring Data Citation Card
             fluidRow(
               column(12,
                      div(class = "feature-card", 
                          style = "border-top: 5px solid #40B4E5;",
                          
                          div(style = "display: flex; align-items: center; margin-bottom: 1rem;",
                              div(class = "feature-icon", 
                                  style = "margin-bottom: 0; margin-right: 1rem;",
                                  icon("water")),
                              div(
                                h3("Beach Water Quality Monitoring Data", 
                                   style = "margin: 0; color: #0E4C90; font-size: 1.4rem;"),
                                p("Fecal Indicator Bacteria (FIB) Measurements", 
                                  style = "margin: 0; color: #666; font-size: 0.9rem;")
                              )
                          ),
                          
                          hr(style = "border-color: #e8eaed; margin: 1rem 0;"),
                          
                          h4("Description", style = "color: #263746; font-size: 1rem; margin-bottom: 0.5rem;"),
                          p("This dataset contains fecal indicator bacteria (FIB) monitoring results 
                            from beaches across Los Angeles County. The data includes measurements of 
                            Total Coliform, Fecal Coliform, and Enterococcus -- key indicators used to 
                            assess recreational water quality and protect public health.",
                            style = "color: #555; line-height: 1.6;"),
                          
                          div(style = "background: #f8f9fa; padding: 1rem; border-radius: 8px; margin: 1rem 0;",
                              h4("Dataset Details", style = "color: #263746; font-size: 1rem; margin-bottom: 0.75rem;"),
                              tags$table(style = "width: 100%; font-size: 0.9rem;",
                                         tags$tr(
                                           tags$td(style = "padding: 0.4rem 0; color: #666; width: 140px;", "Parameters:"),
                                           tags$td(style = "padding: 0.4rem 0;", "Total Coliform, Fecal Coliform, Enterococcus (MPN/100mL)")
                                         ),
                                         tags$tr(
                                           tags$td(style = "padding: 0.4rem 0; color: #666;", "Geographic Scope:"),
                                           tags$td(style = "padding: 0.4rem 0;", "Los Angeles County coastal beaches")
                                         ),
                                         tags$tr(
                                           tags$td(style = "padding: 0.4rem 0; color: #666;", "Update Frequency:"),
                                           tags$td(style = "padding: 0.4rem 0;", "Weekly during beach season")
                                         ),
                                         tags$tr(
                                           tags$td(style = "padding: 0.4rem 0; color: #666;", "Data Provider:"),
                                           tags$td(style = "padding: 0.4rem 0;", "California State Water Resources Control Board")
                                         )
                              )
                          ),
                          
                          div(class = "info-box accent-algae", style = "margin: 1rem 0;",
                              h4("Suggested Citation", style = "color: #263746; font-size: 0.95rem; margin-bottom: 0.5rem;"),
                              p(HTML("California State Water Resources Control Board. <em>Beach Water Quality 
                                     Monitoring Data.</em> Retrieved from California Beach Monitoring Program. 
                                     Available at: <a href='https://www.waterboards.ca.gov/water_issues/programs/beaches/search_beach_mon.html' 
                                     target='_blank' style='color: #005CB9;'>https://www.waterboards.ca.gov/water_issues/programs/beaches/search_beach_mon.html</a>"),
                                style = "margin: 0; font-size: 0.9rem; line-height: 1.6;")
                          ),
                          
                          div(style = "display: flex; gap: 1rem; margin-top: 1.5rem; flex-wrap: wrap;",
                              tags$a(href = "https://www.waterboards.ca.gov/water_issues/programs/beaches/search_beach_mon.html",
                                     target = "_blank",
                                     class = "btn btn-primary",
                                     style = "display: inline-flex; align-items: center;",
                                     icon("external-link-alt", style = "margin-right: 8px;"),
                                     "Visit Data Source"),
                              downloadButton("download_fib_data", "Download Dataset", 
                                             class = "btn-download",
                                             style = "display: inline-flex; align-items: center;")
                          )
                      )
               )
             ),
             
             # Placeholder for future data sources
             fluidRow(
               column(12,
                      div(style = "margin-top: 2rem; padding: 2rem; background: #f8f9fa; 
                                  border-radius: 12px; border: 2px dashed #d0d0d0; text-align: center;",
                          icon("plus-circle", style = "font-size: 2rem; color: #aaa; margin-bottom: 0.5rem;"),
                          h4("Additional Data Sources Coming Soon", 
                             style = "color: #888; font-weight: 500; margin-bottom: 0.25rem;"),
                          p("Citations for stormwater projects, DAC data, LAUSD parcels, and watershed boundaries 
                            will be added here.",
                            style = "color: #aaa; font-size: 0.9rem; margin: 0;")
                      )
               )
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
      return(stormwater_pts[0, ])
    }
    stormwater_pts %>%
      filter(project_type_clean %in% selected_types)
  })
  
  # Reactive filtered DAC tracts
  filtered_dac <- reactive({
    selected_pctls <- input$dac_percentile_filter
    if (is.null(selected_pctls) || length(selected_pctls) == 0) {
      return(dac_pts[0, ])
    }
    dac_pts %>%
      filter(percentile_bin %in% selected_pctls)
  })
  
  # Reactive LAUSD parcels (no filters - show all loaded parcels)
  filtered_lausd <- reactive({
    lausd_parcels  # return as-is; NULL handled downstream
  })
  
  # --- Watershed-aware stat reactives ---
  stormwater_in_ws <- reactive({
    proj <- filtered_stormwater()
    if (is.null(fixed_ws) || nrow(fixed_ws) == 0) return(proj)
    
    selected_ws <- input$selected_watersheds
    if (is.null(selected_ws) || length(selected_ws) == 0) return(proj[0, ])
    
    ws_filtered <- fixed_ws %>% filter(LABEL %in% selected_ws)
    if (nrow(ws_filtered) == 0) return(proj[0, ])
    
    tryCatch(
      proj[st_within(proj, st_union(ws_filtered), sparse = FALSE)[, 1], ],
      error = function(e) proj
    )
  })
  
  dac_in_ws <- reactive({
    d <- filtered_dac()
    if (is.null(fixed_ws) || nrow(fixed_ws) == 0) return(d)
    
    selected_ws <- input$selected_watersheds
    if (is.null(selected_ws) || length(selected_ws) == 0) return(d[0, ])
    
    ws_filtered <- fixed_ws %>% filter(LABEL %in% selected_ws)
    if (nrow(ws_filtered) == 0) return(d[0, ])
    
    tryCatch(
      d[st_within(d, st_union(ws_filtered), sparse = FALSE)[, 1], ],
      error = function(e) d
    )
  })
  
  monitoring_in_ws <- reactive({
    beach <- monitoring_sites_pts
    river <- testing_sites_pts
    if (is.null(fixed_ws) || nrow(fixed_ws) == 0) {
      return(list(beach = beach, river = river))
    }
    
    selected_ws <- input$selected_watersheds
    if (is.null(selected_ws) || length(selected_ws) == 0) {
      return(list(beach = beach[0, ], river = river[0, ]))
    }
    
    ws_filtered <- fixed_ws %>% filter(LABEL %in% selected_ws)
    ws_union <- st_union(ws_filtered)
    
    beach_filtered <- tryCatch(
      beach[st_within(beach, ws_union, sparse = FALSE)[, 1], ],
      error = function(e) beach
    )
    river_filtered <- tryCatch(
      river[st_within(river, ws_union, sparse = FALSE)[, 1], ],
      error = function(e) river
    )
    list(beach = beach_filtered, river = river_filtered)
  })
  
  # --- Stats outputs (watershed-aware) ---
  output$stat_projects <- renderUI({
    n_proj <- nrow(stormwater_in_ws())
    div(
      div(class = "stat-value", n_proj),
      div(class = "stat-label", "Stormwater Projects")
    )
  })
  
  output$stat_lausd <- renderUI({
    n_lausd <- if (!is.null(filtered_lausd())) nrow(filtered_lausd()) else 0
    div(
      div(class = "stat-value", n_lausd),
      div(class = "stat-label", "LAUSD Parcels")
    )
  })
  
  output$stat_dac <- renderUI({
    n_dac <- nrow(dac_in_ws())
    total_dac <- nrow(dac_pts)
    div(
      div(class = "stat-value", paste0(n_dac, "/", total_dac)),
      div(class = "stat-label", "DAC Tracts")
    )
  })
  
  output$stat_monitoring <- renderUI({
    mon <- monitoring_in_ws()
    n_mon <- nrow(mon$beach) + nrow(mon$river)
    div(
      div(class = "stat-value", n_mon),
      div(class = "stat-label", "Monitoring Sites")
    )
  })
  
  output$stat_volume <- renderUI({
    proj_data <- stormwater_in_ws()
    total_vol <- sum(proj_data$volume_addressed, na.rm = TRUE)
    div(
      div(class = "stat-value", paste0(format(round(total_vol, 1), big.mark = ","))),
      div(class = "stat-label", "Acre-ft Addressed")
    )
  })
  
  # dynamic watershed filter UI
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
  
  # Select/Clear observers - Watersheds
  observeEvent(input$select_all_ws, {
    updateCheckboxGroupInput(session, "selected_watersheds", 
                             selected = watershed_choices())
  })
  observeEvent(input$clear_all_ws, {
    updateCheckboxGroupInput(session, "selected_watersheds", 
                             selected = character(0))
  })
  
  # Select/Clear observers - Project Types
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
  
  # Select/Clear observers - DAC
  observeEvent(input$select_all_dac, {
    updateCheckboxGroupInput(session, "dac_percentile_filter", 
                             selected = c("95-100% (Highest)", "90-95%", "85-90%", "80-85%", "75-80%"))
  })
  observeEvent(input$clear_all_dac, {
    updateCheckboxGroupInput(session, "dac_percentile_filter", 
                             selected = character(0))
  })
  
  # Select/Clear observers - LAUSD
  observeEvent(input$select_all_lausd, {
    updateCheckboxGroupInput(session, "lausd_priority_filter",
                             selected = lausd_priority_tiers)
    updateCheckboxGroupInput(session, "lausd_type_filter",
                             selected = lausd_school_types)
  })
  observeEvent(input$clear_all_lausd, {
    updateCheckboxGroupInput(session, "lausd_priority_filter",
                             selected = character(0))
    updateCheckboxGroupInput(session, "lausd_type_filter",
                             selected = character(0))
  })
  

  output$map <- renderLeaflet({
    # If anything in here errors, you still get a basic map instead of a blank panel
    tryCatch({
      m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "Street") %>%
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
      
      # Add LA County outline only if it exists + is non-empty
      if (!is.null(la_county_geo) && nrow(la_county_geo) > 0) {
        m <- m %>%
          addPolygons(
            data = la_county_geo,
            color = htb_colors$coal_gray, weight = 2.5, fillOpacity = 0,
            group = "LA County", label = ~NAME,
            options = pathOptions(interactive = FALSE)
          )
      } else {
        message("la_county_geo is NULL or empty; skipping outline.")
      }
      
      # Draw watersheds here so they sit at the bottom of the layer stack
      if (!is.null(fixed_ws) && nrow(fixed_ws) > 0) {
        m <- m %>%
          addPolygons(
            data = fixed_ws,
            weight = 2,
            color = htb_colors$ocean_blue,
            fillColor = htb_colors$light_aqua,
            fillOpacity = 0.15,
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
              "<div style='background-color: #005CB9; color: white; padding: 10px;
               margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
              "<strong style='font-family: Montserrat, sans-serif;'>", LABEL, "</strong></div>",
              "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Watershed Boundary</p>",
              "</div>"
            )
          )
      }
      
      m                          # <-- still the last line
    }, error = function(e) {
      message("renderLeaflet() failed: ", e$message)
      
      # Fallback simple map so you can see something even if providers/geo fail
      leaflet() %>%
        addTiles() %>%
        setView(lng = -118.25, lat = 34.05, zoom = 10)
    })
  })
  
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  # Observer for dynamic map layer toggling (SAFE: waits for map to exist)
  observeEvent(
    list(
      input$map_bounds,
      input$map_layers,
      input$project_type_filter,
      input$dac_percentile_filter,
      input$selected_watersheds
    ),
    {
      req(input$map_bounds)
      
      # Build active watershed union for spatial clipping
      ws_union <- NULL
      if (!is.null(fixed_ws) && nrow(fixed_ws) > 0) {
        selected_ws <- input$selected_watersheds
        if (!is.null(selected_ws) && length(selected_ws) > 0) {
          ws_filtered <- fixed_ws %>% filter(LABEL %in% selected_ws)
          if (nrow(ws_filtered) > 0) {
            ws_union <- tryCatch(st_union(ws_filtered), error = function(e) NULL)
          }
        }
      }
      
      clip_to_ws <- function(sf_obj) {
        if (is.null(ws_union) || is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj[0, ])
        tryCatch(
          sf_obj[st_within(sf_obj, ws_union, sparse = FALSE)[, 1], ],
          error = function(e) sf_obj
        )
      }
      
      clip_poly_to_ws <- function(sf_obj) {
        if (is.null(ws_union) || is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj[0, ])
        tryCatch(
          sf_obj[st_intersects(sf_obj, ws_union, sparse = FALSE)[, 1], ],
          error = function(e) sf_obj
        )
      }
      
      layers <- input$map_layers
      if (is.null(layers)) layers <- character(0)
      
      proxy <- leafletProxy("map")
      
      proxy %>%
        clearGroup("Stormwater Projects") %>%
        clearGroup("Park Polygons") %>%
        clearGroup("LAUSD School Parcels") %>%
        clearGroup("Disadvantaged Communities") %>%
        clearGroup("Beach Monitoring Sites") %>%
        clearGroup("River Monitoring Sites") %>%
        clearGroup("LA County Precip Stations")
      
      show <- function(x) isTRUE(x %in% layers)
      
      # ---- DAC ----
      if (show("Disadvantaged Communities")) {
        dac_filtered <- clip_to_ws(filtered_dac())
        if (nrow(dac_filtered) > 0) {
          dac_pal <- colorNumeric(
            palette = c("#fc8d59", "#d73027"),
            domain = c(75, 100)
          )
          proxy %>%
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
                "<div style='background-color: #d73027; color: white; padding: 10px;
                 margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
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
                "</table></div>"
              ),
              group = "Disadvantaged Communities"
            )
        }
      }
        
     
      # ---- Park Polygons ----
      if (show("Park Polygons") && !is.null(park_polygons) && nrow(park_polygons) > 0) {
        park_clipped <- clip_poly_to_ws(park_polygons)
        if (nrow(park_clipped) > 0) {
          proxy %>%
            addPolygons(
              data = park_clipped,
              color = "#1a7340",
              weight = 1,
              opacity = 0.9,
              fillColor = "#2ca25f",
              fillOpacity = 0.35,
              smoothFactor = 0.5,
              group = "Park Polygons",
              popup = "<div style='font-family: Source Sans Pro, sans-serif; min-width: 160px;'>
                       <div style='background-color: #2ca25f; color: white; padding: 10px;
                         margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>
                         <strong style='font-family: Montserrat, sans-serif;'>Park</strong>
                       </div>
                       <p style='margin: 0; padding-top: 4px; color: #263746; font-size: 13px;'>
                         LA County Park Polygon</p>
                     </div>",
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#263746",
                fillOpacity = 0.6,
                bringToFront = TRUE
              )
            )
        }
      }
      
      # ---- LAUSD Parcels ----
      if (show("LAUSD School Parcels") && !is.null(lausd_parcels) && nrow(lausd_parcels) > 0) {
        lausd_data <- clip_poly_to_ws(filtered_lausd())
        if (is.null(lausd_data) || nrow(lausd_data) == 0) {
          message("LAUSD layer selected but filtered_lausd() returned 0 rows (skipping draw).")
        } else {
          proxy %>%
            addPolygons(
              data = lausd_data,
              color = "#F47E48",
              weight = 1,
              opacity = 0.9,
              fillOpacity = 0.35,
              smoothFactor = 0.5,
              group = "LAUSD School Parcels",
              popup = ~paste0(
                "<div style='font-family: Source Sans Pro, sans-serif; min-width: 180px;'>",
                "<div style='background-color: #F47E48; color: white; padding: 10px;
                 margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
                "<strong style='font-family: Montserrat, sans-serif;'>LAUSD School Parcel</strong></div>",
                "<p style='margin: 0; padding-top: 4px; color: #263746; font-size: 13px;'>",
                "<b>", LABEL, "</b></p>",
                "</div>"
              ),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#263746",
                fillOpacity = 0.6,
                bringToFront = TRUE
              )
            )
        }
      }
      
      # ---- Stormwater projects ----
      if (show("Stormwater Projects")) {
        proj_data <- clip_to_ws(filtered_stormwater())
        if (nrow(proj_data) > 0) {
          proxy %>%
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
                "<div style='background-color: #90B83E; color: white; padding: 10px;
                 margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
                "<strong style='font-family: Montserrat, sans-serif;'>", name, "</strong></div>",
                "<table style='font-size: 12px; width: 100%;'>",
                "<tr><td style='color: #666; padding: 3px 0;'>Type:</td><td style='text-align: right;'><b>", project_type_clean, "</b></td></tr>",
                "<tr><td style='color: #666; padding: 3px 0;'>Volume:</td><td style='text-align: right;'>", volume_fmt, "</td></tr>",
                "<tr><td style='color: #666; padding: 3px 0;'>Capital Cost:</td><td style='text-align: right;'>", capital_cost_fmt, "</td></tr>",
                ifelse(!is.na(completion_date),
                       paste0("<tr><td style='color: #666; padding: 3px 0;'>Completed:</td><td style='text-align: right;'>", completion_date, "</td></tr>"),
                       ""),
                "</table></div>"
              ),
              group = "Stormwater Projects"
            )
        }
      }
      
      # ---- Beach sites ----
      if (show("Beach Monitoring Sites")) {
        beach_sites <- clip_to_ws(monitoring_sites_pts)
        if (nrow(beach_sites) > 0) {
          proxy %>%
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
                "<div style='background-color: #40B4E5; color: white; padding: 10px;
                 margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
                "<strong style='font-family: Montserrat, sans-serif;'>", location_name, "</strong></div>",
                "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Beach Monitoring Site</p>",
                "</div>"
              ),
              group = "Beach Monitoring Sites"
            )
        }
      }
      
      # ---- River sites ----
      if (show("River Monitoring Sites")) {
        river_sites <- clip_to_ws(testing_sites_pts)
        if (nrow(river_sites) > 0) {
          proxy %>%
            addCircleMarkers(
              data = river_sites,
              lng = ~lon, lat = ~lat,
              radius = 8,
              color = "#FFFFFF",
              fillColor = htb_colors$deep_sea,
              fillOpacity = 0.85,
              weight = 2,
              popup = ~paste0(
                "<div style='font-family: Source Sans Pro, sans-serif;'>",
                "<div style='background-color: #0E4C90; color: white; padding: 10px;
                 margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
                "<strong style='font-family: Montserrat, sans-serif;'>", name, "</strong></div>",
                "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Order in watershed: ", order_in_ws, "</p>",
                "</div>"
              ),
              group = "River Monitoring Sites"
            )
        }
      }
      
      # ---- Precip stations ----
      if (show("LA County Precip Stations")) {
        precip_sites <- clip_to_ws(la_prec_stations_pts)
        if (nrow(precip_sites) > 0) {
          proxy %>%
            addCircleMarkers(
              data = precip_sites,
              lng = ~lon, lat = ~lat,
              radius = 8,
              color = "#FFFFFF",
              fillColor = htb_colors$sunshine,
              fillOpacity = 0.85,
              weight = 2,
              popup = ~paste0(
                "<div style='font-family: Source Sans Pro, sans-serif;'>",
                "<div style='background-color: #FCC755; color: #263746; padding: 10px;
                 margin: -14px -18px 10px -18px; border-radius: 12px 12px 0 0;'>",
                "<strong style='font-family: Montserrat, sans-serif;'>", name, "</strong></div>",
                "<p style='margin: 0; padding-top: 5px; color: #666; font-size: 12px;'>Precipitation Station</p>",
                "</div>"
              ),
              group = "LA County Precip Stations"
            )
        }
      }
      
    },
    ignoreNULL = FALSE
  )
  
  # Separate observer for watershed boundaries (SAFE: waits for map to exist)
  observeEvent(
    list(input$map_bounds, input$map_layers, input$selected_watersheds),
    {
      req(input$map_bounds)
      proxy <- leafletProxy("map")
      
      if ("Watershed Boundaries" %in% input$map_layers) {
        proxy %>% showGroup("Watershed Boundaries")
      } else {
        proxy %>% hideGroup("Watershed Boundaries")
      }
    },
    ignoreNULL = FALSE
  )
  
  # Trend plot
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
  
  # Seasonal plot
  output$seasonal_plot <- renderPlot({
    df <- fib_long %>%
      filter(location_name == input$site_select,
             parameter == input$param_select,
             between(year(date), input$year_range[1], input$year_range[2]))
    req(nrow(df) > 0)
    
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
  
  # Download handler for FIB data
  output$download_fib_data <- downloadHandler(
    filename = function() paste0("htb_beach_monitoring_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(fib_data, file)
    }
  )
}

# --- Run App ---
shinyApp(ui, server)