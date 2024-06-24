# wotus-wetlands-wetness

## About
This repository contains code (R scripts and ArcGIS tools and notebooks) used to estimate federal jurisdictional status for the conterminous US based on different interpretations of the US Supreme Court's majority opinion in Sackett v. EPA.

These data were produced using the National Wetlands Inventory (NWI), National Hydrography Dataset Plus High Resolution (NHDPlus HR), NWI Difference Product line, and the PAD-US dataset. Wetland polygons from the NWI were filtered to better align with the US Army Corps of Engineer's 3-factor definition of wetlands and intersected with select buffered NHDPlus HR features. Wetland flooding frequency, derived from the NWI's Coward Code Water Regime modifier, was used as a proxy for different interpretations of a "continuous surface connection", such that spatially contiguous wetland polygons that met or exceeded a specific flooding frequency were estimated as jurisdictional if any polygon in a group was estimated jurisdictional. NWI polygons with an NWI difference product impervious surface percentage of 5% or greater were excluded.

## Related products
This code was used in the following products:
- Pre-print: https://essopenarchive.org/doi/full/10.22541/essoar.171052545.55380305/v1
- Forthcoming Dryad repository (contains more info)

## Input data
- [National Wetlands Inventory (NWI)](https://www.fws.gov/program/national-wetlands-inventory/wetlands-data) (downloaded during workflow)
- [NWI Code Definitions Download Package](https://www.fws.gov/program/national-wetlands-inventory/classification-codes)
- [NWI Difference Product Line](https://www.fws.gov/project/national-wetlands-inventory-difference-product-line)
- [National Hydrography Dataset Plus High Resolution](https://www.sciencebase.gov/catalog/item/57645ff2e4b07657d19ba8e8) (NHDPlus HR)
- [State outlines from census bureau](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.2022.html#list-tab-1258746043)
- [Protected Areas Database of the United States US 3.0](https://www.sciencebase.gov/catalog/item/61955a0bd34eb622f6908f88)

## Workflow
Code in this repository should be run in the following order:

### Prep data
1. `setup_workspace` (ArcGIS tool): Creates folders and geodatabases using a particular schema that aligns with rest of tools
2. `nwi_download.R`: Scrapes the NWI download website to download data and organizes metadata
3. `metadata_cleanup` (ArcGIS tool): Organizes metadata to create a national layer

### Merge NWI into national dataset
4. `process_nwi.ipynb` (ArcGIS Notebook): Clips wetlands to state boundaries and repairs any bad geometries
5. `R/combine_nwi.R`: Combines state NWI datasets into a single national dataset

### 3-factor wetland definition filter
6. `three_factor_nwi_filter` (ArcGIS tool): Filters out wetland classes that do not fit the USACE's 3-factor definition of wetlands and removes wetlands with more than 5% of their area covered by impervious surfaces

### Process NHD waters
7. `select_nhd` (ArcGIS tool): Selects specific types of water bodies and assigns a flow_class (i.e., hydrographic classification) to flowlines based on their `FCode` or `FType` attributes.
8. `correct_flowline_class_and_buffer` (ArcGIS tool): Modifies flow_class values for select flowlines and then buffers them.
9. `create_waters` (ArcGIS tool): Overlays water bodies with buffered flowlines to assign flow_class to the water bodies, then buffers the water bodies, then combines them with the buffered flowlines to create a single "waters" layer.

### Overlay wetlands and estimate jurisdictional status with different interpretations
10. `overlay_wetlands_and_waters` (ArcGIS tool): Assign flow_class to wetland polygons by overlaying the waters layer
11. `wetlands_water_regime_propagation` (ArcGIS tool): Group wetland polygons by if they meet/exceed a range of water regime number thresholds. Overlay with PAD-US dataset and re-calculate polygon areas
12. `R/wetland_water_regime_propagation.R`: Propagate minimum flow_class to contiguous groups of wetland polygons for each scenario modeled
13. `wetlands_water_regime_join` (ArcGIS tool): Join back the propagated flow_class values to the final wetlands layer.

### Other tools used within workflow tools
- `assign_nhd_flowline_class` (ArcGIS tool)
- `iterate_feature_classes_in_gdb` (ArcGIS tool)

## Summarize results
- `wetland_summary.R`: Create figures and tables from the manuscript using either data from the Dryad repository (`wetlands_for_dryad.gdb`) or table produced by running code in this repo.
