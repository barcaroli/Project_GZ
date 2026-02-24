# Name: plot_merged_data.R
# Function: Render an interactive mapview map from merged grid/UNOSAT objects saved in RData.
# Input datasets:
# - out/merged_data.RData (expects merged_dt, grid_sf, and optionally damage_sf/damage_dt)
# Processing Logic:
# - Load merged objects from RData.
# - Build grid map in WGS84 and color by damage_points_total.
# - Optionally overlay UNOSAT points if available.
# - Save map HTML to out/ and print in interactive sessions.
# Output datasets:
# - out/gaza_grid_100m_osm_damaged_viirs_map.html
# - out/gaza_grid_100m_osm_damaged_viirs_map_files/

suppressPackageStartupMessages({
  library(sf)
})

# --------------------------
# 1) Configuration
# --------------------------
base_dir <- "D:/Google Drive/Societa geografica/Palestina/Workflow2"
in_rdata <- file.path(base_dir, "out", "merged_data.RData")
out_map_html <- file.path(base_dir, "out", "gaza_grid_100m_osm_damaged_viirs_map.html")

# --------------------------
# 2) Validate Inputs
# --------------------------
if (!file.exists(in_rdata)) {
  stop("Missing input RData: ", in_rdata)
}

# Load into dedicated environment to avoid clobbering local config variables.
e <- new.env(parent = emptyenv())
load(in_rdata, envir = e)

required_objects <- c("merged_dt", "grid_sf")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), envir = e)]
if (length(missing_objects) > 0) {
  stop("RData is missing required object(s): ", paste(missing_objects, collapse = ", "))
}

# --------------------------
# 3) Build Map Layers
# --------------------------
if (!requireNamespace("mapview", quietly = TRUE)) {
  stop("Package 'mapview' is not installed.")
}
if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  stop("Package 'htmlwidgets' is not installed.")
}

library(mapview)

# Reconstruct sf from merged attributes and saved grid geometry.
crs_utm <- if (exists("crs_utm", envir = e)) get("crs_utm", envir = e) else 32636
grid_map <- st_sf(get("merged_dt", envir = e), geometry = st_geometry(get("grid_sf", envir = e)), crs = crs_utm)
grid_map <- st_transform(grid_map, 4326)

# Helper to add numeric choropleth layers only when the column exists.
add_numeric_layer <- function(m, sf_obj, col_name, layer_name, alpha = 0.7) {
  if (!(col_name %in% names(sf_obj))) return(m)
  lyr <- mapview(
    sf_obj,
    zcol = col_name,
    layer.name = layer_name,
    alpha.regions = alpha
  )
  if (is.null(m)) lyr else m + lyr
}

# Build a multi-layer map so the HTML has all key thematic layers.
m <- NULL
m <- add_numeric_layer(m, grid_map, "damage_points_total", "Damage points total")
m <- add_numeric_layer(m, grid_map, "destroyed_n", "Destroyed points per cell")
m <- add_numeric_layer(m, grid_map, "bldg_count_osm", "OSM building count per cell")
m <- add_numeric_layer(m, grid_map, "mean_avg_rad", "VIIRS mean radiance")

# Add damage points layer if available.
if (exists("damage_sf", envir = e)) {
  dmg_pts_map <- st_transform(get("damage_sf", envir = e), 4326)
  max_points <- 10000L
  if (nrow(dmg_pts_map) > max_points) {
    set.seed(42)
    keep <- sample(seq_len(nrow(dmg_pts_map)), max_points)
    dmg_pts_map <- dmg_pts_map[keep, ]
    message("UNOSAT point layer sampled to ", max_points, " points for HTML performance.")
  }
  m <- m + mapview(
    dmg_pts_map,
    zcol = "damage_level",
    layer.name = "UNOSAT damage points",
    cex = 3
  )
} else if (exists("damage_dt", envir = e)) {
  damage_sf <- st_as_sf(get("damage_dt", envir = e), coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  max_points <- 10000L
  if (nrow(damage_sf) > max_points) {
    set.seed(42)
    keep <- sample(seq_len(nrow(damage_sf)), max_points)
    damage_sf <- damage_sf[keep, ]
    message("UNOSAT point layer sampled to ", max_points, " points for HTML performance.")
  }
  m <- m + mapview(
    damage_sf,
    zcol = "damage_level",
    layer.name = "UNOSAT damage points",
    cex = 3
  )
}

# --------------------------
# 4) Save And Print
# --------------------------
dir.create(dirname(out_map_html), showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(m@map, out_map_html, selfcontained = FALSE)
message("Saved interactive map to: ", out_map_html)

if (interactive()) {
  print(m)
}
