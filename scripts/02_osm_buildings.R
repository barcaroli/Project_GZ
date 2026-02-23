# Name: 02_osm_buildings.R
# Function: Enrich the Gaza 100 m grid with OSM-derived road/building metrics and export tabular/spatial outputs.
# Input datasets:
# - out/gaza_grid_100m.gpkg (layer: grid_100m)
# - data/gaza_aoi.geojson (AOI polygon used for filtering and Overpass bbox)
# - OpenStreetMap via Overpass API (roads/buildings)
# Processing Logic:
# - Load grid and AOI, harmonize CRS to EPSG:32636.
# - Download OSM roads/buildings, repair/filter geometries to AOI.
# - Compute per-cell nearest-road distance and building count/area metrics.
# - Export enriched grid and building points, plus interactive mapview visualizations.
# Output datasets:
# - out/gaza_grid_100m_with_osm.gpkg (layer: grid_100m_osm)
# - out/gaza_grid_100m_with_osm.csv
# - out/gaza_osm_buildings.csv
# - out/gaza_grid_100m_osm_map.html
# - out/gaza_osm_buildings_map.html

# Suggested install (once):
# install.packages(c("cli", "lwgeom", "progress", "mapview", "htmlwidgets"))

# --------------------------
# 0) Libraries And Runtime
# --------------------------
setwd("D:/Google Drive/Societa geografica/Palestina/Workflow2")
suppressPackageStartupMessages({
  library(sf)
  library(osmdata)
})

# --------------------------
# CONFIG
# --------------------------
grid_gpkg  <- "out/gaza_grid_100m.gpkg"
grid_layer <- "grid_100m"

aoi_path   <- "data/gaza_aoi.geojson"
aoi_layer  <- NULL

crs_utm    <- 32636  # WGS84 / UTM 36N

# Overpass settings
overpass_timeout <- 600  # seconds
# options(osmdata.server = "https://overpass-api.de/api/interpreter") # optional

out_gpkg     <- "out/gaza_grid_100m_with_osm.gpkg"
out_csv      <- "out/gaza_grid_100m_with_osm.csv"
out_map_html <- "out/gaza_grid_100m_osm_map.html"

# --------------------------
# 2) Helper Functions
# --------------------------
read_aoi <- function(path, layer = NULL) {
  if (!file.exists(path)) stop("AOI file not found: ", path)
  if (is.null(layer)) st_read(path, quiet = TRUE) else st_read(path, layer = layer, quiet = TRUE)
}

# Spinner + elapsed time while running an expression (Solution A)
pb_fetch <- function(label, expr) {
  if (requireNamespace("cli", quietly = TRUE)) {
    id <- cli::cli_progress_bar(label, format = "{cli::pb_spin} {label} | {elapsed}")
    on.exit(cli::cli_progress_done(id), add = TRUE)
    res <- eval.parent(substitute(expr))
    return(res)
  } else {
    message(label)
    t0 <- Sys.time()
    res <- eval.parent(substitute(expr))
    message(sprintf("...done in %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    return(res)
  }
}

# geometry repair (works for sf and sfc)
make_valid_any <- function(x) {
  if (!(inherits(x, "sf") || inherits(x, "sfc"))) return(x)
  
  is_sf <- inherits(x, "sf")
  geom  <- if (is_sf) st_geometry(x) else x
  
  # 1) make_valid (lwgeom preferred)
  if (requireNamespace("lwgeom", quietly = TRUE)) {
    geom <- tryCatch(lwgeom::st_make_valid(geom), error = function(e) geom)
  } else {
    geom <- tryCatch(st_make_valid(geom), error = function(e) geom)
  }
  
  # 2) buffer(0) ONLY on polygon geometries, ONLY if projected CRS (never on lines)
  is_ll <- tryCatch(sf::st_is_longlat(geom), error = function(e) FALSE)
  if (!is_ll) {
    gt <- sf::st_geometry_type(geom)
    is_poly <- gt %in% c("POLYGON", "MULTIPOLYGON")
    
    if (any(is_poly)) {
      geom_poly <- geom[is_poly]
      geom_poly <- tryCatch(sf::st_buffer(geom_poly, 0), error = function(e) geom_poly)
      geom[is_poly] <- geom_poly
    }
  }
  
  if (is_sf) {
    st_geometry(x) <- geom
    return(x)
  } else {
    return(geom)
  }
}

drop_bad_geoms <- function(x) {
  if (nrow(x) == 0) return(x)
  x <- x[!st_is_empty(x), ]
  
  gt <- st_geometry_type(x)
  is_poly <- gt %in% c("POLYGON","MULTIPOLYGON")
  
  ok <- rep(TRUE, nrow(x))
  if (any(is_poly)) {
    v <- tryCatch(st_is_valid(x[is_poly, ]), error = function(e) rep(TRUE, sum(is_poly)))
    v <- ifelse(is.na(v), TRUE, v)
    ok[which(is_poly)] <- v
  }
  x[ok, ]
}

# Prefer filter-to-AOI (robust). Clip only if you truly need clipping.
filter_to_aoi <- function(x, aoi_geom) {
  if (nrow(x) == 0) return(x)
  idx <- st_intersects(x, aoi_geom, sparse = FALSE)
  x[idx, ]
}

# Overpass bbox query (then filter locally)
fetch_osm <- function(aoi_wgs84, key, value = NULL, timeout = 600) {
  bb <- st_bbox(aoi_wgs84)
  q <- opq(bbox = c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]), timeout = timeout)
  if (is.null(value)) q <- add_osm_feature(q, key = key) else q <- add_osm_feature(q, key = key, value = value)
  osmdata_sf(q)
}

compute_dist_to_roads <- function(grid_utm, roads_utm) {
  if (nrow(roads_utm) == 0) {
    grid_utm$dist_road_m <- NA_real_
    return(grid_utm)
  }
  cent <- st_centroid(grid_utm)
  
  roads_lines <- roads_utm[st_geometry_type(roads_utm) %in% c("LINESTRING","MULTILINESTRING"), ]
  if (nrow(roads_lines) == 0) {
    grid_utm$dist_road_m <- NA_real_
    return(grid_utm)
  }
  
  idx <- st_nearest_feature(cent, roads_lines)
  d <- st_distance(cent, roads_lines[idx, ], by_element = TRUE)
  grid_utm$dist_road_m <- as.numeric(d)
  grid_utm
}

compute_building_metrics <- function(grid_utm, buildings_utm) {
  n <- nrow(grid_utm)
  grid_utm$bldg_count_osm <- integer(n)
  grid_utm$bldg_area_sum_m2_osm <- numeric(n)
  grid_utm$bldg_area_frac_osm <- numeric(n)
  
  if (nrow(buildings_utm) == 0) return(grid_utm)
  
  bpoly <- buildings_utm[st_geometry_type(buildings_utm) %in% c("POLYGON","MULTIPOLYGON"), ]
  if (nrow(bpoly) == 0) return(grid_utm)
  
  bpoly <- make_valid_any(bpoly)
  bpoly <- st_collection_extract(bpoly, "POLYGON", warn = FALSE)
  bpoly <- drop_bad_geoms(bpoly)
  if (nrow(bpoly) == 0) return(grid_utm)
  
  grid_utm <- make_valid_any(grid_utm)
  
  # Count buildings by assigning one representative point per building
  # to exactly one grid cell (prevents multi-cell double counting).
  bpts <- tryCatch(
    st_point_on_surface(bpoly),
    error = function(e) st_centroid(bpoly)
  )
  pt_hits <- st_intersects(bpts, grid_utm)
  pt_cell_idx <- vapply(pt_hits, function(ix) if (length(ix) > 0) ix[1] else NA_integer_, integer(1))
  
  has_id <- "osm_id" %in% names(bpoly)
  valid_pt <- !is.na(pt_cell_idx)
  if (any(valid_pt)) {
    pt_cell_id <- grid_utm$cell_id[pt_cell_idx[valid_pt]]
    if (has_id) {
      count_df <- unique(data.frame(
        cell_id = pt_cell_id,
        osm_id = bpoly$osm_id[valid_pt],
        stringsAsFactors = FALSE
      ))
      count_tab <- aggregate(osm_id ~ cell_id, data = count_df, FUN = length)
      names(count_tab)[2] <- "bldg_count_osm"
    } else {
      count_tab <- as.data.frame(table(pt_cell_id), stringsAsFactors = FALSE)
      names(count_tab) <- c("cell_id", "bldg_count_osm")
      count_tab$cell_id <- as.integer(as.character(count_tab$cell_id))
      count_tab$bldg_count_osm <- as.integer(count_tab$bldg_count_osm)
    }
  } else {
    count_tab <- data.frame(cell_id = integer(0), bldg_count_osm = integer(0))
  }
  pos_cnt <- match(grid_utm$cell_id, count_tab$cell_id)
  grid_utm$bldg_count_osm <- ifelse(is.na(pos_cnt), 0L, as.integer(count_tab$bldg_count_osm[pos_cnt]))
  
  message("Intersecting buildings with grid (this can take a while)...")
  inter <- tryCatch(
    suppressWarnings(st_intersection(
      bpoly[, intersect(names(bpoly), c("osm_id","name","building")), drop = FALSE],
      grid_utm[, "cell_id", drop = FALSE]
    )),
    error = function(e) {
      message("WARNING: st_intersection failed: ", conditionMessage(e))
      message("Trying again after precision snapping...")
      b2 <- tryCatch(st_set_precision(bpoly, 0.01), error = function(e2) bpoly)
      g2 <- tryCatch(st_set_precision(grid_utm, 0.01), error = function(e2) grid_utm)
      b2 <- make_valid_any(b2); g2 <- make_valid_any(g2)
      suppressWarnings(st_intersection(
        b2[, intersect(names(b2), c("osm_id","name","building")), drop = FALSE],
        g2[, "cell_id", drop = FALSE]
      ))
    }
  )
  
  if (is.null(inter) || nrow(inter) == 0) return(grid_utm)
  
  inter$part_area_m2 <- as.numeric(st_area(inter))
  agg <- split(inter, inter$cell_id)
  
  cell_id <- as.integer(names(agg))
  bldg_area_sum <- vapply(agg, function(x) sum(x$part_area_m2, na.rm = TRUE), numeric(1))
  
  m <- data.frame(cell_id = cell_id,
                  bldg_area_sum_m2_osm = bldg_area_sum)
  
  pos <- match(grid_utm$cell_id, m$cell_id)
  grid_utm$bldg_area_sum_m2_osm <- ifelse(is.na(pos), 0, m$bldg_area_sum_m2_osm[pos])
  grid_utm$bldg_area_frac_osm   <- grid_utm$bldg_area_sum_m2_osm / 10000
  
  grid_utm
}

save_or_print_map <- function(m, out_html) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    message("Package 'mapview' not installed. Skipping interactive plot. Install with: install.packages('mapview')")
    return(invisible(NULL))
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    message("Package 'htmlwidgets' not installed. Skipping HTML save. Install with: install.packages('htmlwidgets')")
    return(invisible(NULL))
  }
  dir.create(dirname(out_html), showWarnings = FALSE, recursive = TRUE)
  widget <- if (inherits(m, "mapview")) m@map else m
  htmlwidgets::saveWidget(widget, out_html, selfcontained = FALSE)
  message("Saved interactive map to: ", out_html, " (assets in ", out_html, "_files)")
  if (interactive()) print(m)
}

# --------------------------
# 3) Main Processing
# --------------------------
sf_use_s2(FALSE)

grid <- st_read(grid_gpkg, layer = grid_layer, quiet = TRUE)
grid <- st_transform(grid, crs_utm)

aoi <- read_aoi(aoi_path, aoi_layer)

# Dissolve/union in UTM (avoid lon/lat planar assumptions)
aoi_utm <- st_transform(aoi, crs_utm)
aoi_utm <- make_valid_any(aoi_utm)
aoi_utm <- st_union(aoi_utm)       # returns sfc
aoi_utm <- make_valid_any(aoi_utm) # safe for sfc now

# WGS84 for Overpass bbox + basemap
aoi_wgs84 <- st_transform(aoi_utm, 4326)

# --- Fetch OSM roads (highway=*)
osm_roads <- pb_fetch("Fetching OSM roads (highway=*)",
                      fetch_osm(aoi_wgs84, key = "highway", timeout = overpass_timeout))

roads <- osm_roads$osm_lines
if (is.null(roads) || nrow(roads) == 0) {
  roads <- osm_roads$osm_multilines
  if (!is.null(roads) && nrow(roads) > 0) roads <- st_cast(roads, "MULTILINESTRING", warn = FALSE)
}
if (is.null(roads)) roads <- st_sf(geometry = st_sfc(), crs = 4326)

roads <- st_transform(roads, crs_utm)
roads <- make_valid_any(roads)
roads <- drop_bad_geoms(roads)
roads <- filter_to_aoi(roads, aoi_utm)

# --- Fetch OSM buildings (building=*)
osm_bldg <- pb_fetch("Fetching OSM buildings (building=*)",
                     fetch_osm(aoi_wgs84, key = "building", timeout = overpass_timeout))

buildings <- osm_bldg$osm_polygons
if (is.null(buildings)) buildings <- st_sf(geometry = st_sfc(), crs = 4326)
# buildings <- buildings[,c("osm_id","name","amenity","building","shop","geometry")]

buildings <- st_transform(buildings, crs_utm)
buildings <- make_valid_any(buildings)
buildings <- st_collection_extract(buildings, "POLYGON", warn = FALSE)
buildings <- drop_bad_geoms(buildings)
buildings <- filter_to_aoi(buildings, aoi_utm)

# --- Add centroid coordinates to each building
if (nrow(buildings) > 0) {
  cent_utm <- st_centroid(buildings)
  xy_utm <- st_coordinates(cent_utm)
  buildings$x_m <- xy_utm[, 1]
  buildings$y_m <- xy_utm[, 2]
  
  cent_wgs <- st_transform(cent_utm, 4326)
  xy_wgs <- st_coordinates(cent_wgs)
  buildings$lon <- xy_wgs[, 1]
  buildings$lat <- xy_wgs[, 2]
}


# --- Compute per-cell metrics
grid2 <- compute_dist_to_roads(grid, roads)
grid2 <- compute_building_metrics(grid2, buildings)
summary(buildings)
builds <- buildings[,c("osm_id","x_m","y_m","lon","lat")]
builds <- st_drop_geometry(builds)
# --- Save outputs
dir.create(dirname(out_gpkg), showWarnings = FALSE, recursive = TRUE)
st_write(grid2, out_gpkg, layer = "grid_100m_osm", delete_layer = TRUE, quiet = TRUE)
write.csv(st_drop_geometry(grid2), out_csv, row.names = FALSE)
write.csv(builds, "out/gaza_osm_buildings.csv", row.names = FALSE)

message("Saved grid + OSM metrics to: ", out_gpkg)
message("Saved table to: ", out_csv)
message("Road features kept (after AOI filter): ", nrow(roads))
message("Building features kept (after AOI filter): ", nrow(buildings))

# --------------------------
# 4) Final Grid Mapview
# --------------------------
if (requireNamespace("mapview", quietly = TRUE)) {
  library(mapview)
  grid_wgs  <- st_transform(grid2, 4326)
  aoi_wgs   <- st_transform(aoi_utm, 4326)
  roads_wgs <- st_transform(roads, 4326)
  bldg_wgs  <- st_transform(buildings, 4326)
  
  m <- mapview(aoi_wgs, color = "red", lwd = 2, layer.name = "Gaza AOI") +
    mapview(grid_wgs, alpha.regions = 0, lwd = 0.3, layer.name = "100m grid") +
    mapview(roads_wgs, col.regions = "black", lwd = 1, layer.name = "OSM roads") +
    mapview(bldg_wgs, alpha.regions = 0.2, layer.name = "OSM buildings") +
    mapview(grid_wgs, zcol = "bldg_area_frac_osm", layer.name = "bldg_area_frac_osm")
  
  save_or_print_map(m, out_map_html)
} else {
  message("mapview not installed -> skipping final map. Install with install.packages('mapview')")
}

# --------------------------
# 5) Final Buildings Mapview
# --------------------------
out_map_html_buildings <- "out/gaza_osm_buildings_map.html"
if (requireNamespace("mapview", quietly = TRUE)) {
  library(mapview)
  aoi_wgs_b <- st_transform(aoi_utm, 4326)
  bldg_wgs_b <- st_transform(buildings, 4326)
  mb <- mapview(aoi_wgs_b, color = "red", lwd = 2, layer.name = "Gaza AOI") +
    mapview(bldg_wgs_b, alpha.regions = 0.25, layer.name = "OSM buildings")
  save_or_print_map(mb, out_map_html_buildings)
} else {
  message("mapview not installed -> skipping buildings map. Install with install.packages('mapview')")
}

# dams <- read.csv("data/GAZA_damaged_buildings.csv")
# dams_sf <- st_as_sf(dams, coords = c("X", "y"), crs = 4326)
# head(dams)
# if (requireNamespace("mapview", quietly = TRUE)) {
#   library(mapview)
#   aoi_wgs_b <- st_transform(aoi_utm, 4326)
#   bldg_wgs_b <- st_transform(builds, 4326)
#   mb <- 
#     # mapview(aoi_wgs_b, color = "red", lwd = 2, layer.name = "Gaza AOI") +
#     mapview(bldg_wgs_b, alpha.regions = 0.25, color = "green",layer.name = "OSM buildings") +
#     mapview(dams_sf, alpha.regions = 0.25, cex=1, color = "red", layer.name = "Damaged buildings")
#   save_or_print_map(mb, out_map_html_buildings)
# } else {
#   message("mapview not installed -> skipping buildings map. Install with install.packages('mapview')")
# }
# 
