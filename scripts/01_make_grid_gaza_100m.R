# Name: 01_make_grid_gaza_100m.R
# Function: Build a 100x100 m analysis grid over Gaza AOI, with stable alignment and cell geometry attributes.
# Input datasets:
# - data/gaza_aoi.geojson (AOI polygon, expected CRS EPSG:4326 or declared CRS)
# Processing Logic:
# - Read and validate AOI geometry.
# - Reproject AOI to EPSG:32636 and generate a snapped 100 m square grid.
# - Keep cells intersecting AOI; compute centroid and corner coordinates per cell.
# - Export grid as GPKG/CSV and render an interactive mapview map.
# Output datasets:
# - out/gaza_grid_100m.gpkg (layer: grid_100m)
# - out/gaza_grid_100m_centroids.csv
# - out/gaza_grid_100m_map.html

# --------------------------
# 0) Libraries And Runtime
# --------------------------
setwd("D:/Google Drive/Societa geografica/Palestina/Workflow2")
suppressPackageStartupMessages({
  library(sf)
})

# --------------------------
# CONFIG
# --------------------------
aoi_path   <- "data/gaza_aoi.geojson"     # Gaza boundary polygon (EPSG:4326)
aoi_layer  <- NULL                        # set if using GPKG with multiple layers

out_gpkg   <- "out/gaza_grid_100m.gpkg"
out_csv    <- "out/gaza_grid_100m_centroids.csv"
out_map_html <- "out/gaza_grid_100m_map.html"  # interactive map (saved if non-interactive)

cellsize_m <- 100
crs_utm    <- 32636  # WGS84 / UTM zone 36N

# --------------------------
# 2) Helper Functions
# --------------------------
read_aoi <- function(path, layer = NULL) {
  if (!file.exists(path)) stop("AOI file not found: ", path)
  if (is.null(layer)) st_read(path, quiet = TRUE) else st_read(path, layer = layer, quiet = TRUE)
}

# compute corners for each grid cell (xmin,ymin,xmax,ymax) in projected CRS
cell_corners <- function(grid_sf) {
  bb <- lapply(st_geometry(grid_sf), st_bbox)
  corners <- do.call(rbind, lapply(bb, function(b) {
    c(xmin = unname(b["xmin"]), ymin = unname(b["ymin"]),
      xmax = unname(b["xmax"]), ymax = unname(b["ymax"]))
  }))
  as.data.frame(corners)
}

# mapview helper: print if interactive, else save to HTML
save_or_print_map <- function(m, out_html) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    message("Package 'mapview' not installed. Skipping interactive plot. Install with: install.packages('mapview')")
    return(invisible(NULL))
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("Package 'htmlwidgets' not installed. Skipping HTML save. Install with: install.packages('htmlwidgets')")
    return(invisible(NULL))
  }
  if (interactive()) {
    print(m)
  } else {
    dir.create(dirname(out_html), showWarnings = FALSE, recursive = TRUE)
    htmlwidgets::saveWidget(m@map, out_html, selfcontained = TRUE)
    message("Saved interactive map to: ", out_html)
  }
}

# --------------------------
# 3) Main Processing
# --------------------------
sf_use_s2(FALSE)  # safer for projected ops

aoi <- read_aoi(aoi_path, aoi_layer)
aoi <- st_make_valid(aoi)

# If AOI has multiple features, dissolve into one polygon
aoi <- st_union(aoi)

# Ensure CRS then project to meters
if (is.na(st_crs(aoi))) {
  stop("AOI has no CRS. Please assign it (likely EPSG:4326) before running.")
}
aoi_utm <- st_transform(aoi, crs_utm)

# Stable grid alignment ("snap" to 100m)
bb <- st_bbox(aoi_utm)
origin_x <- floor(as.numeric(bb["xmin"]) / cellsize_m) * cellsize_m
origin_y <- floor(as.numeric(bb["ymin"]) / cellsize_m) * cellsize_m

# Make grid over bbox area with fixed origin
grid_geom <- st_make_grid(
  aoi_utm,
  cellsize = cellsize_m,
  square = TRUE,
  offset = c(origin_x, origin_y)
)

grid <- st_sf(cell_id = seq_along(grid_geom), geometry = grid_geom)

# Keep only cells intersecting AOI (but keep full squares, not clipped)
sel <- lengths(st_intersects(grid, aoi_utm)) > 0
grid <- grid[sel, ]

# Centroids (in meters, EPSG:32636)
cent <- st_centroid(grid)
xy <- st_coordinates(cent)
grid$x_m <- xy[, 1]
grid$y_m <- xy[, 2]

# Optional: corners
cc <- cell_corners(grid)
grid$xmin_m <- cc$xmin; grid$ymin_m <- cc$ymin
grid$xmax_m <- cc$xmax; grid$ymax_m <- cc$ymax

# Write outputs
dir.create(dirname(out_gpkg), showWarnings = FALSE, recursive = TRUE)
st_write(grid, out_gpkg, layer = "grid_100m", delete_layer = TRUE, quiet = TRUE)

grid_tab <- st_drop_geometry(grid)
write.csv(grid_tab, out_csv, row.names = FALSE)

message("Saved grid to: ", out_gpkg)
message("Saved centroids table to: ", out_csv)

# --------------------------
# 4) Final Mapview Output
# --------------------------
if (requireNamespace("mapview", quietly = TRUE)) {
  library(mapview)
  # plot in WGS84 so basemap aligns nicely
  aoi_wgs  <- st_transform(aoi, 4326)
  grid_wgs <- st_transform(grid, 4326)

  m <- mapview(aoi_wgs, color = "red", lwd = 2, layer.name = "Gaza AOI") +
       mapview(grid_wgs, alpha.regions = 0, lwd = 0.4, layer.name = "100m grid")
  save_or_print_map(m, out_map_html)
} else {
  message("mapview not installed -> skipping final map. Install with install.packages('mapview')")
}
