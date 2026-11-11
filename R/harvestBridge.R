# applyHarvestSchedule(): apply WS3 harvest schedule to LandR cohortData and pixelGroupMap

# applyWs3Solution(): make a solved ws3 problem the model's schedule. add_problem() resets the
# model for each stand type it enumerates, so after solve() applied_actions holds only the last
# one built; the solution has to be compiled and applied before anything reads it.
# Done in Python: through R, the schedule's development type tuples would become lists.
applyWs3Solution <- function(fm, problem) {
  reticulate::py_run_string("
def _ws3_apply_solution(fm, problem):
    return fm.apply_schedule(fm.compile_schedule(problem), force_integral_area=False,
                             fuzzy_age=False, recourse_enabled=False, compile_c_ycomps=True)
")
  missing <- reticulate::py_to_r(reticulate::py_eval("_ws3_apply_solution")(fm, problem))
  if (isTRUE(missing > 0)) {
    warning("applyWs3Solution: ", round(missing, 2), " ha of the solved schedule could not be applied")
  }
  invisible(fm)
}

applyClearcut <- function(harvestRast, cohortData, pixelGroupMap) {
  stopifnot(
    inherits(harvestRast, "SpatRaster"),
    inherits(pixelGroupMap, "SpatRaster"),
    is.data.table(cohortData),
    "pixelGroup" %in% names(cohortData)
  )

  # Identify pixel groups where any pixel was harvested
  harvestVals  <- terra::values(harvestRast, mat = FALSE)
  pgVals       <- terra::values(pixelGroupMap, mat = FALSE)
  # Note: !is.na() is explicit for clarity; NA == 1L is already NA and excluded
  harvestedPGs <- unique(pgVals[!is.na(harvestVals) & harvestVals == 1L])

  # Clearcut semantics: if ANY pixel in a pixel group was scheduled for harvest,
  # the entire pixel group's cohorts are removed (consistent with pixel-group abstraction).
  cd <- copy(cohortData)
  # Set mortality = B before zeroing (two separate := calls — data.table evaluates left-to-right)
  cd[pixelGroup %in% harvestedPGs, mortality := B]
  cd[pixelGroup %in% harvestedPGs, `:=`(B = 0L, aNPPAct = 0L)]
  cd[]
}

applyHarvestSchedule <- function(schedule, cohortData, pixelGroupMap,
                                  outputPath, baseYear) {
  # schedule: data.table with columns year, acode, area (from WS3 solution)
  # Reads GeoTIFFs written by ForestRaster.allocate_schedule()
  for (yr in unique(schedule$year)) {
    yearSchedule <- schedule[year == yr]
    for (ac in unique(yearSchedule$acode)) {
      tifPath <- file.path(outputPath, sprintf("%s_%d.tif", ac, yr))
      if (!file.exists(tifPath)) {
        message("applyHarvestSchedule: GeoTIFF not found, skipping: ", tifPath)
        next
      }
      harvestRast <- terra::rast(tifPath)
      cohortData  <- applyHarvestAction(ac, harvestRast, cohortData, pixelGroupMap)
    }
  }
  cohortData
}
