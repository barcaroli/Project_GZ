#!/usr/bin/env Rscript
# Name: 05_merge_grid_osm_unosat_viirs.R
# Function: Merge grid-level OSM metrics, UNOSAT damage counts, and VIIRS lights into one 100 m cell dataset.
# Input datasets:
# - out/gaza_grid_100m_with_osm.csv
# - out/unosat_damage.csv
# - out/gaza_viirs_cells_2025-10.csv
# Processing Logic:
# - Merge grid and VIIRS tables by cell_id.
# - Rebuild grid polygons from bounds and spatially assign UNOSAT points to cells.
# - Aggregate damage counts per cell and append to merged table.
# - Export final CSV and interactive mapview visualization.
# Output datasets:
# - out/gaza_grid_100m_osm_damaged_viirs.csv
# - out/gaza_grid_100m_osm_damaged_viirs_map.html

# --------------------------
# 0) Libraries And Runtime
# --------------------------
setwd("D:/Google Drive/Societa geografica/Palestina/Workflow2")

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})

sf::sf_use_s2(FALSE)

# --------------------------
# 1) Configuration
# --------------------------
in_grid_csv   <- "out/gaza_grid_100m_with_osm.csv"
in_damage_csv <- "out/unosat_damage.csv"
in_viirs_csv  <- "out/gaza_viirs_cells_2025-10.csv"

out_csv       <- "out/gaza_grid_100m_osm_damaged_viirs.csv"
out_map_html  <- "out/gaza_grid_100m_osm_damaged_viirs_map.html"

crs_utm <- 32636  # WGS84 / UTM zone 36N (same as grid workflow)

# --------------------------
# 2) Helper Functions
# --------------------------
stop_if_missing_cols <- function(dt, cols, label) {
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0) {
    stop(sprintf("%s missing required columns: %s", label, paste(miss, collapse = ", ")))
  }
}

make_grid_sf_from_bounds <- function(grid_dt, crs = 32636) {
  stop_if_missing_cols(
    grid_dt,
    c("cell_id", "xmin_m", "ymin_m", "xmax_m", "ymax_m"),
    "Grid CSV"
  )
  wkt <- sprintf(
    "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
    grid_dt$xmin_m, grid_dt$ymin_m,
    grid_dt$xmax_m, grid_dt$ymin_m,
    grid_dt$xmax_m, grid_dt$ymax_m,
    grid_dt$xmin_m, grid_dt$ymax_m,
    grid_dt$xmin_m, grid_dt$ymin_m
  )
  geom <- st_as_sfc(wkt, crs = crs)
  st_sf(grid_dt, geometry = geom)
}

save_mapview_html <- function(map_obj, out_html) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    message("Package 'mapview' not installed. Skipping map output.")
    return(invisible(NULL))
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("Package 'htmlwidgets' not installed. Skipping map output.")
    return(invisible(NULL))
  }
  dir.create(dirname(out_html), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(map_obj@map, out_html, selfcontained = FALSE)
  message("Saved interactive map to: ", out_html)
}

# --------------------------
# 3) Read Inputs And Validate Schema
# --------------------------
if (!file.exists(in_grid_csv)) stop("Missing input: ", in_grid_csv)
if (!file.exists(in_damage_csv)) stop("Missing input: ", in_damage_csv)
if (!file.exists(in_viirs_csv)) stop("Missing input: ", in_viirs_csv)

grid_dt   <- fread(in_grid_csv, encoding = "UTF-8")
damage_dt <- fread(in_damage_csv, encoding = "UTF-8")
viirs_dt  <- fread(in_viirs_csv, encoding = "UTF-8")

stop_if_missing_cols(grid_dt, c("cell_id"), "Grid CSV")
stop_if_missing_cols(viirs_dt, c("cell_id"), "VIIRS CSV")
stop_if_missing_cols(damage_dt, c("lon", "lat", "damage_level"), "UNOSAT CSV")

setDT(grid_dt)
setDT(viirs_dt)
setDT(damage_dt)

# --------------------------
# 4) Merge Grid And VIIRS By Cell ID
# --------------------------
merged_dt <- merge(
  grid_dt,
  viirs_dt,
  by = "cell_id",
  all.x = TRUE,
  sort = FALSE
)

# --------------------------
# 5) Aggregate UNOSAT Points To Grid Cells
# --------------------------
grid_sf <- make_grid_sf_from_bounds(merged_dt, crs = crs_utm)

damage_sf <- st_as_sf(
  damage_dt,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)
damage_sf <- st_transform(damage_sf, crs_utm)

damage_in_grid <- st_join(
  damage_sf,
  grid_sf[, "cell_id", drop = FALSE],
  join = st_within,
  left = FALSE
)

damage_cell_dt <- as.data.table(st_drop_geometry(damage_in_grid))[
  ,
  .(
    damage_points_total = .N,
    destroyed_n = sum(damage_level == "destroyed", na.rm = TRUE),
    severely_damaged_n = sum(damage_level == "severely_damaged", na.rm = TRUE),
    moderately_damaged_n = sum(damage_level == "moderately_damaged", na.rm = TRUE),
    possibly_damaged_n = sum(damage_level == "possibly_damaged", na.rm = TRUE),
    no_visible_or_unknown_n = sum(damage_level == "no_visible_damage_or_unknown", na.rm = TRUE),
    unknown_n = sum(damage_level == "unknown", na.rm = TRUE)
  ),
  by = cell_id
]

merged_dt <- merge(merged_dt, damage_cell_dt, by = "cell_id", all.x = TRUE, sort = FALSE)

count_cols <- c(
  "damage_points_total",
  "destroyed_n",
  "severely_damaged_n",
  "moderately_damaged_n",
  "possibly_damaged_n",
  "no_visible_or_unknown_n",
  "unknown_n"
)
for (col in count_cols) {
  if (!col %in% names(merged_dt)) merged_dt[, (col) := 0L]
  merged_dt[is.na(get(col)), (col) := 0L]
}

# --------------------------
# 6) Write Output CSV
# --------------------------
dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
fwrite(merged_dt, out_csv)
message("Saved merged CSV to: ", out_csv)

save.image(file="merged_data.RData")

# --------------------------
# 7) Mapview Output
# --------------------------
# if (requireNamespace("mapview", quietly = TRUE)) {
#   library(mapview)
#   grid_map <- st_sf(merged_dt, geometry = st_geometry(grid_sf), crs = crs_utm)
#   grid_map <- st_transform(grid_map, 4326)
# 
#   m <- mapview(
#     grid_map,
#     zcol = "damage_points_total",
#     layer.name = "Damage points per 100m cell",
#     alpha.regions = 0.7
#   )
# 
#   if (nrow(damage_dt) > 0) {
#     dmg_pts_map <- st_transform(damage_sf, 4326)
#     m <- m + mapview(
#       dmg_pts_map,
#       zcol = "damage_level",
#       layer.name = "UNOSAT damage points",
#       cex = 3
#     )
#   }
#   save_mapview_html(m, out_map_html)
# } else {
#   message("mapview not installed -> skipping map output")
# }

# --------------------------
# 8) Optional Console Sanity Checks
# --------------------------
sum(merged_dt$bldg_count_osm)
# [1] 325170
sum(merged_dt$damage_points_total)
# [1] 198308
sum(merged_dt$destroyed_n)
# [1] 123464
sum(merged_dt$severely_damaged_n)
# [1] 17116
sum(merged_dt$possibly_damaged_n)
# [1] 23836
