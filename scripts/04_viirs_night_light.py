"""
Name: 04_viirs_night_light.py
Function: Extract monthly VIIRS night-light statistics for each 100 m Gaza grid cell.
Input datasets:
- data/gaza_aoi.geojson (AOI for clipping)
- out/gaza_grid_100m_centroids.csv (cell_id and cell geometry support columns)
- NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG (Google Earth Engine ImageCollection)
Processing Logic:
- Authenticate to Google Earth Engine and load AOI geometry.
- Read grid cells and build polygon features in manageable blocks.
- For each month, run reduceRegions with mean/median/sum reducers on avg_rad.
- Download block CSV outputs, merge by month, and write monthly results.
Output datasets:
- out/gaza_viirs_cells_<YYYY-MM>.csv
"""

# -----------------------------
# 0) Imports
# -----------------------------
import os
import json
import ee
import pandas as pd
import requests
import zipfile
from pathlib import Path

# -----------------------------
# 1) Authentication
# -----------------------------
ee.Authenticate(auth_mode="localhost")
ee.Initialize(project="landcovercopernicus")

# -----------------------------
# 2) Load AOI From GeoJSON
# -----------------------------
GEOJSON_PATH = r"D:\Google Drive\Societa geografica\Palestina\Workflow2\data\gaza_aoi.geojson"

with open(GEOJSON_PATH, "r", encoding="utf-8") as f:
    gj = json.load(f)

def geojson_to_aoi_geometry(gj_obj):
    """
    Convert a GeoJSON dict to an ee.Geometry.
    Supports: FeatureCollection, Feature, Geometry (Polygon/MultiPolygon/etc.).
    """
    t = gj_obj.get("type", None)

    if t == "FeatureCollection":
        fc = ee.FeatureCollection(gj_obj)
        geom = fc.geometry()
    elif t == "Feature":
        geom = ee.Feature(gj_obj).geometry()
    else:
        geom = ee.Geometry(gj_obj)

    # Defensive fix
    geom = geom.buffer(0, 1)
    return geom

aoi = geojson_to_aoi_geometry(gj)

# -----------------------------
# 3) Configuration
# -----------------------------
START = "2025-10-01"
END   = "2025-11-01"   # end exclusive for October only (change as needed)
SCALE_M = 500          # VIIRS ~500m

GRID_CSV_PATH = r"D:\Google Drive\Societa geografica\Palestina\Workflow2\out\gaza_grid_100m_centroids.csv"
OUT_DIR       = r"D:\Google Drive\Societa geografica\Palestina\Workflow2\out"
os.makedirs(OUT_DIR, exist_ok=True)

COLL_ID = "NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG"
BAND = "avg_rad"

# Grid CRS (your x_m/y_m are in meters; adjust if different)
GRID_CRS = "EPSG:32636"
CELL_SIZE_M = 100
HALF = CELL_SIZE_M / 2

# download in blocks to avoid size limits
BLOCK_SIZE = 500

# -----------------------------
# 4) Read Grid And Build Processing Blocks
# -----------------------------
df = pd.read_csv(GRID_CSV_PATH)

required_cols = {"cell_id", "x_m", "y_m"}
missing = required_cols - set(df.columns)
if missing:
    raise ValueError(f"Missing required columns in grid CSV: {sorted(missing)}")

n_cells = len(df)
print(f"[OK] Read grid cells: {n_cells}")
if n_cells != 37029:
    print("[WARN] Expected 37029 cells, but got:", n_cells)

df = df.reset_index(drop=True)
df["block"] = (df.index // BLOCK_SIZE) + 1
n_blocks = int(df["block"].max())
print(f"[OK] BLOCK_SIZE={BLOCK_SIZE} -> n_blocks={n_blocks}")

# -----------------------------
# 5) Helper: Build EE FeatureCollection Per Block
# Prefer exact bbox if xmin/ymin/xmax/ymax exist; else buffer+bounds.
# Build one block at a time to avoid EE payload limits.
# -----------------------------
proj = ee.Projection(GRID_CRS)

has_bbox = {"xmin_m", "ymin_m", "xmax_m", "ymax_m"}.issubset(df.columns)
if has_bbox:
    print("[OK] Using exact bbox columns (xmin/ymin/xmax/ymax) from CSV.")
else:
    print("[OK] Bbox columns not found; building 100m square via buffer(50m).")

def build_block_fc(df_block: pd.DataFrame) -> ee.FeatureCollection:
    features = []
    for row in df_block.itertuples(index=False):
        cell_id = int(row.cell_id)

        if has_bbox:
            xmin = float(getattr(row, "xmin_m"))
            ymin = float(getattr(row, "ymin_m"))
            xmax = float(getattr(row, "xmax_m"))
            ymax = float(getattr(row, "ymax_m"))

            # Rectangle in metric CRS, then transform to EPSG:4326
            cell_geom = ee.Geometry.Rectangle([xmin, ymin, xmax, ymax], proj, False)
        else:
            x = float(row.x_m)
            y = float(row.y_m)
            pt = ee.Geometry.Point([x, y], proj)
            # Avoid passing a Projection where EE expects errorMargin
            cell_geom = pt.buffer(HALF).bounds()

        cell_geom = cell_geom.transform("EPSG:4326", 1)
        features.append(ee.Feature(cell_geom, {"cell_id": cell_id}))

    return ee.FeatureCollection(features)

# -----------------------------
# 6) VIIRS Monthly Collection (Clipped To AOI)
# -----------------------------
col = (
    ee.ImageCollection(COLL_ID)
    .filterDate(START, END)
    .select([BAND])
    .map(lambda img: img.clip(aoi))
)

n_months = col.size().getInfo()
print(f"[OK] Monthly images found: {n_months}")

imgs = col.toList(col.size())

# -----------------------------
# 7) Download Helper (EE -> ZIP -> CSV)
# -----------------------------
def download_fc_as_csv(fc: ee.FeatureCollection, out_csv_path: str, timeout_s: int = 900):
    """
    Downloads an ee.FeatureCollection as CSV to out_csv_path.
    Earth Engine usually returns a ZIP containing the CSV -> handled.
    """
    url = fc.getDownloadURL("CSV")

    out_csv_path = str(out_csv_path)
    tmp_zip = str(Path(out_csv_path).with_suffix(".zip"))

    r = requests.get(url, stream=True, timeout=timeout_s)
    r.raise_for_status()

    with open(tmp_zip, "wb") as f:
        for chunk in r.iter_content(chunk_size=1024 * 1024):
            if chunk:
                f.write(chunk)

    # If it's a zip, extract first CSV. Otherwise treat as plain CSV.
    try:
        with zipfile.ZipFile(tmp_zip, "r") as z:
            csv_names = [n for n in z.namelist() if n.lower().endswith(".csv")]
            if not csv_names:
                raise RuntimeError("Downloaded ZIP does not contain a CSV.")
            csv_name = csv_names[0]
            z.extract(csv_name, path=str(Path(out_csv_path).parent))
            extracted = Path(out_csv_path).parent / csv_name

            if Path(out_csv_path).exists():
                Path(out_csv_path).unlink()
            extracted.replace(out_csv_path)
    except zipfile.BadZipFile:
        # Not a zip: rename the downloaded file to CSV
        if Path(out_csv_path).exists():
            Path(out_csv_path).unlink()
        Path(tmp_zip).replace(out_csv_path)
        return

    Path(tmp_zip).unlink(missing_ok=True)

# -----------------------------
# 8) Reducer Definition
# -----------------------------
reducer = (
    ee.Reducer.mean().setOutputs(["mean_avg_rad"])
    .combine(ee.Reducer.median().setOutputs(["median_avg_rad"]), sharedInputs=True)
    .combine(ee.Reducer.sum().setOutputs(["sum_avg_rad"]), sharedInputs=True)
)

# -----------------------------
# 9) Per-Month Processing And Output Merge
# -----------------------------
for i in range(n_months):
    img = ee.Image(imgs.get(i))
    month = ee.Date(img.get("system:time_start")).format("YYYY-MM").getInfo()
    print(f"\n=== Processing month: {month} ===")

    block_csvs = []

    for b in range(1, n_blocks + 1):
        df_block = df[df["block"] == b]
        fc_block = build_block_fc(df_block)

        stats_fc = img.reduceRegions(
            collection=fc_block,
            reducer=reducer,
            scale=SCALE_M,
            tileScale=4
        )

        # add month as property
        stats_fc = stats_fc.map(lambda f: f.set("month", month))

        # keep only requested columns
        stats_fc = stats_fc.select(["cell_id", "month", "mean_avg_rad", "median_avg_rad", "sum_avg_rad"])

        # Debug fetching server-side properties can trigger large payloads.
        # Leave disabled unless needed.

        out_block = os.path.join(OUT_DIR, f"gaza_viirs_cells_{month}_block_{b}.csv")
        download_fc_as_csv(stats_fc, out_block)
        block_csvs.append(out_block)

        print(f"[OK] Saved block {b}/{n_blocks}: {out_block}")

    merged = pd.concat([pd.read_csv(p) for p in block_csvs], ignore_index=True)

    # enforce ordering and quick sanity checks
    merged = merged.sort_values("cell_id").reset_index(drop=True)

    out_month = os.path.join(OUT_DIR, f"gaza_viirs_cells_{month}.csv")
    merged.to_csv(out_month, index=False, encoding="utf-8")

    print(f"[OK] Monthly merged CSV: {out_month}")
    print(f"     Rows: {len(merged)} | unique cell_id: {merged['cell_id'].nunique()}")

    # optional: remove block files
    for p in block_csvs:
        try:
            os.remove(p)
        except Exception:
            pass

print("\n[OK] Done.")
