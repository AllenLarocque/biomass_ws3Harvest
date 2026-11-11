# buildWs3Inventory(): translate cohortData into WS3 dev type area table
#
# Note: site quality binning (maxANPP/species$maxANPP → low/med/high) is performed
# by the calling module event (via binSiteQuality() from siteQuality.R) BEFORE
# this function is called. cohortData must already have a `site_quality` column.
# This separation keeps the pure-R aggregation logic unit-testable without SpaDES.

# dominantCohort(): one row per pixelGroup, the cohort with the most biomass. ws3 holds each
# pixel in one development type and age, so the aspatial inventory (buildWs3Inventory) and the
# raster inventory (buildInventoryRaster) must both use this, or the schedule cannot be allocated.
dominantCohort <- function(cohortData) {
  cohortData[cohortData[, .I[which.max(B)], by = pixelGroup]$V1]
}

# ws3AgeClass(): stand age for ws3, in years, floored to a multiple of the period length.
# ws3 ages stands by period_length per period, and its yield curves and operability limits are
# in years. Binning keeps one age per period step, as the model only visits those.
ws3AgeClass <- function(age, periodLength) {
  as.integer(floor(age / periodLength) * periodLength)
}

buildWs3Inventory <- function(cohortData, pixelArea, periodLength,
                               minHarvestAge = 40L) {
  # cohortData must have columns: pixelGroup, speciesCode, ecoregionGroup,
  #   age, B, site_quality (pre-binned by binSiteQuality)
  # pixelArea: data.table(pixelGroup, area_ha)
  # periodLength: integer, years per WS3 period (for age class binning)
  # minHarvestAge: minimum age, in years, for harvestable flag

  stopifnot(
    is.data.table(cohortData),
    is.data.table(pixelArea),
    all(c("pixelGroup", "speciesCode", "ecoregionGroup", "age", "site_quality", "B") %in% names(cohortData)),
    all(c("pixelGroup", "area_ha") %in% names(pixelArea)),
    length(periodLength) == 1L, periodLength > 0,
    length(minHarvestAge) == 1L, minHarvestAge >= 0
  )

  # Each pixel group counts once, under its dominant cohort, so areas sum to the forested area
  cd <- dominantCohort(cohortData)

  # Build dev type key for each cohort (scalar devTypeTupleKey called row-by-row)
  cd[, devTypeKey  := mapply(devTypeTupleKey, speciesCode, site_quality, ecoregionGroup)]
  cd[, age_class   := ws3AgeClass(age, periodLength)]
  cd[, harvestable := age >= minHarvestAge]

  cd <- merge(cd, pixelArea, by = "pixelGroup", all.x = TRUE)

  n_missing_area <- sum(is.na(cd$area_ha))
  if (n_missing_area > 0) {
    warning("buildWs3Inventory: ", n_missing_area,
            " cohort(s) have no matching pixelArea entry — area will be treated as NA")
  }

  inv <- cd[, .(area_ha = sum(area_ha, na.rm = TRUE),
                harvestable = any(harvestable)),
            by = .(devTypeKey, age_class)]
  inv[]
}

# loadWs3Inventory(): replace the inventory held by a ws3 ForestModel with `inv`.
# Always period 0: ForestModel.reset() starts the plan from period 0's areas and discards the
# others, so loading any other period leaves ws3 replanning from the first inventory it saw.
# inv: data.table(devTypeKey, age_class, area_ha)
loadWs3Inventory <- function(fm, inv) {
  reticulate::py_run_string("
def _ws3_load_inventory(fm, keys, ages, areas):
    for dt in fm.dtypes.values():
        dt.reset_areas(0)
    missing = 0.
    for key, age, area in zip(keys, ages, areas):
        dtk = tuple(key.split('|'))
        if dtk in fm.dtypes:
            fm.dt(dtk).area(0, int(age), float(area))
        else:
            missing += float(area)
    return missing
")
  load_fn <- reticulate::py_eval("_ws3_load_inventory")
  missing <- reticulate::py_to_r(load_fn(fm, as.list(inv$devTypeKey), as.list(inv$age_class),
                                         as.list(inv$area_ha)))
  if (missing > 0) {
    warning("loadWs3Inventory: ", round(missing, 2), " ha left out of the plan: ",
            "its development types have no yield curve")
  }
  invisible(fm)
}
