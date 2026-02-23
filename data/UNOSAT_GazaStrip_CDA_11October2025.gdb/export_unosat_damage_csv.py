#!/usr/bin/env python3
"""Export UNOSAT FileGDB damage points to CSV.

Output columns:
- osm_id (surrogate from FID unless --id-field is provided)
- lon
- lat
- damage_level (mapped text)
- damage_code (raw numeric class)
"""

from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path

import fiona


# Mapping inferred from UNOSAT Gaza CDA 11 Oct 2025 counts.
# Adjust if you have an official codebook for your specific product.
DEFAULT_DAMAGE_MAP = {
    "1": "destroyed",
    "2": "severely_damaged",
    "3": "moderately_damaged",
    "4": "possibly_damaged",
    "11": "possibly_damaged",
    "6": "no_visible_damage_or_unknown",
}


def pick_latest_damage_field(property_names: list[str]) -> str:
    candidates: list[tuple[int, str]] = []
    for name in property_names:
        if name == "Main_Damage_Site_Class":
            candidates.append((1, name))
        elif name.startswith("Main_Damage_Site_Class_"):
            suffix = name.replace("Main_Damage_Site_Class_", "")
            if suffix.isdigit():
                candidates.append((int(suffix), name))
    if not candidates:
        raise ValueError("No Main_Damage_Site_Class field found in layer.")
    return max(candidates, key=lambda x: x[0])[1]


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Export UNOSAT damage points to CSV")
    p.add_argument("gdb_path", help="Path to .gdb folder")
    p.add_argument("output_csv", help="Path to output CSV")
    p.add_argument(
        "--layer",
        default="Damage_Sites_GazaStrip_20251011",
        help="Layer name (default: Damage_Sites_GazaStrip_20251011)",
    )
    p.add_argument(
        "--id-field",
        default=None,
        help="Optional property field to use as osm_id. If omitted, FID is used.",
    )
    p.add_argument(
        "--damage-field",
        default=None,
        help="Optional damage code field. If omitted, latest Main_Damage_Site_Class_* is used.",
    )
    return p.parse_args()


def main() -> int:
    args = parse_args()
    gdb_path = Path(args.gdb_path)
    out_csv = Path(args.output_csv)

    if not gdb_path.exists():
        print(f"ERROR: GDB path not found: {gdb_path}", file=sys.stderr)
        return 1

    with fiona.open(gdb_path, layer=args.layer) as src:
        prop_names = list(src.schema.get("properties", {}).keys())

        damage_field = args.damage_field or pick_latest_damage_field(prop_names)
        if damage_field not in prop_names:
            print(f"ERROR: damage field not found: {damage_field}", file=sys.stderr)
            return 1

        if args.id_field and args.id_field not in prop_names:
            print(f"ERROR: id field not found: {args.id_field}", file=sys.stderr)
            return 1

        out_csv.parent.mkdir(parents=True, exist_ok=True)
        with out_csv.open("w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(
                f, fieldnames=["osm_id", "lon", "lat", "damage_level", "damage_code"]
            )
            writer.writeheader()

            for feat in src:
                geom = feat.get("geometry")
                if not geom or geom.get("type") != "Point":
                    continue

                coords = geom.get("coordinates") or []
                if len(coords) < 2:
                    continue

                x = coords[0]
                y = coords[1]

                props = feat.get("properties") or {}
                raw_code = props.get(damage_field)
                raw_code_str = "" if raw_code is None else str(raw_code)
                damage_level = DEFAULT_DAMAGE_MAP.get(raw_code_str, "unknown")

                osm_id = str(props.get(args.id_field)) if args.id_field else str(feat.get("id"))

                writer.writerow(
                    {
                        "osm_id": osm_id,
                        "lon": f"{x:.6f}",
                        "lat": f"{y:.6f}",
                        "damage_level": damage_level,
                        "damage_code": raw_code_str,
                    }
                )

    print(f"CSV written: {out_csv}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
