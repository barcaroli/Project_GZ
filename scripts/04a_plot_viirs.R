# Name: 04a_plot_viirs.R
# Function: Build a static PNG map of monthly VIIRS night-light intensity over Gaza cells.
# Input datasets:
# - out/gaza_viirs_cells_<YYYY-MM>.csv (expects geometry JSON and VIIRS metric columns)
# Processing Logic:
# - Load monthly VIIRS table and normalize geometry column naming.
# - Parse polygon coordinates from JSON strings and rebuild cell polygons.
# - Select one VIIRS metric, apply log transform, and render a choropleth with ggplot2.
# Output datasets:
# - out/gaza_viirs_cells_<YYYY-MM>_map.png

# --------------------------
# 0) Libraries And Runtime
# --------------------------
suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(sf)
  library(ggplot2)
  library(viridisLite)
})

# --------------------------
# 1) Configuration
# --------------------------
year <- "2023"
month <- "09"

# Disabling s2 often speeds up some lon/lat operations.
sf::sf_use_s2(FALSE)

in_csv  <- paste0("D:/Google Drive/Societa geografica/Palestina/Workflow2/out/gaza_viirs_cells_", year, "-", month, ".csv")
out_png <- paste0("D:/Google Drive/Societa geografica/Palestina/Workflow2/out/gaza_viirs_cells_", year, "-", month, "_map.png")

# --------------------------
# 2) Read Input And Validate Schema
# --------------------------
dt <- fread(in_csv, encoding = "UTF-8")

# Rename `.geo` -> `geojson` if exported with original Earth Engine name.
if (".geo" %in% names(dt)) setnames(dt, ".geo", "geojson")

if (!("geojson" %in% names(dt))) stop("Column .geo / geojson not found in CSV.")

# Choose first available VIIRS metric among standard candidates.
metric_candidates <- c("mean_avg_rad", "median_avg_rad", "sum_avg_rad")
metric <- metric_candidates[metric_candidates %in% names(dt)][1]
if (is.na(metric)) stop("No VIIRS metric column found (mean/median/sum).")

# --------------------------
# 3) Rebuild Cell Polygon Geometry
# --------------------------
# Fast extraction of numeric coordinates from cell GeoJSON polygons.
m <- stringi::stri_extract_all_regex(
  dt$geojson,
  pattern = "[-+]?(?:\\d+\\.\\d+|\\d+)(?:[eE][-+]?\\d+)?",
  simplify = TRUE
)

if (ncol(m) < 6) stop("Geometry parsing failed: expected at least 6 numeric coordinates per cell.")

lon_min <- as.numeric(m[, 1])
lat_min <- as.numeric(m[, 2])
lon_max <- as.numeric(m[, 3])
lat_max <- as.numeric(m[, 6])

wkt <- sprintf(
  "POLYGON((%.10f %.10f, %.10f %.10f, %.10f %.10f, %.10f %.10f, %.10f %.10f))",
  lon_min, lat_min,
  lon_max, lat_min,
  lon_max, lat_max,
  lon_min, lat_max,
  lon_min, lat_min
)

geom <- st_as_sfc(wkt, crs = 4326)

# Drop large raw geometry text after conversion to sf geometry.
dt[, geojson := NULL]
gaza_sf <- st_sf(dt, geometry = geom)

# Log scale improves contrast for skewed radiance distributions.
gaza_sf$metric_raw  <- as.numeric(gaza_sf[[metric]])
gaza_sf$metric_plot <- log1p(gaza_sf$metric_raw)

# --------------------------
# 4) Plot And Export
# --------------------------
p <- ggplot(gaza_sf) +
  geom_sf(aes(fill = metric_plot), color = NA) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "transparent",
    name = paste0(metric, "\nlog1p")
  ) +
  labs(
    title = paste0("Night-time lights (VIIRS) - Gaza Strip - Year ", year, " Month ", month),
    subtitle = "100 m grid; log-scaled for visualization"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p)
ggsave(out_png, p, width = 9, height = 7, dpi = 220)
message("Map saved to: ", out_png)
