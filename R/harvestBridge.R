# Apply the WS3 harvest schedule to LandR cohortData and pixelGroupMap

# ws3FlowConstraints(): the cflw_e argument of ForestModel.add_problem(), holding the output
# within `epsilon` of its period-1 value. Returned unconverted: through R, the integer period
# keys become strings, ws3 finds none of its periods in them, and adds no constraint.
ws3FlowConstraints <- function(name, horizon, epsilon) {
  reticulate::py_run_string("
def _ws3_cflw(name, horizon, epsilon):
    return {name: ({t: epsilon for t in range(1, int(horizon) + 1)}, 1)}
")
  reticulate::py_eval("_ws3_cflw", convert = FALSE)(name, as.integer(horizon), epsilon)
}

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

# applyClearcut(): remove the forest from the cut cells only. Cut cells get pixelGroup 0, LandR's
# code for forest with no cohorts (seed dispersal can recolonise them); cohorts of pixel groups
# left with no cells are dropped. Pixel groups are not contiguous, and regenerated stands share
# one across thousands of pixels, so clearing whole groups removed far more than ws3 allocated.
# Returns list(cohortData, pixelGroupMap).
applyClearcut <- function(harvestRast, cohortData, pixelGroupMap) {
  stopifnot(
    inherits(harvestRast, "SpatRaster"),
    inherits(pixelGroupMap, "SpatRaster"),
    is.data.table(cohortData),
    "pixelGroup" %in% names(cohortData)
  )
  harvestVals <- terra::values(harvestRast, mat = FALSE)
  pgVals      <- terra::values(pixelGroupMap, mat = FALSE)
  cut <- which(!is.na(harvestVals) & harvestVals == 1L & !is.na(pgVals) & pgVals != 0L)
  if (length(cut) == 0L) return(list(cohortData = cohortData, pixelGroupMap = pixelGroupMap))

  pgVals[cut] <- 0L
  pixelGroupMap <- terra::setValues(pixelGroupMap, pgVals)   # a new raster: the input is unchanged
  list(cohortData = cohortData[pixelGroup %in% pgVals], pixelGroupMap = pixelGroupMap)
}

# applyHarvestYear(): apply the clearcut raster ws3 allocated to `year`, if there is one.
# Returns list(cohortData, pixelGroupMap, harvestRast), or NULL when there is no raster.
applyHarvestYear <- function(year, harvestPath, cohortData, pixelGroupMap) {
  tifPath <- file.path(harvestPath, sprintf("clearcut_%d.tif", as.integer(year)))
  if (!file.exists(tifPath)) return(NULL)
  harvestRast <- terra::rast(tifPath)
  out <- applyHarvestAction("clearcut", harvestRast, cohortData, pixelGroupMap)
  out$harvestRast <- harvestRast
  out
}
