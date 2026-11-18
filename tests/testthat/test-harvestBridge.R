# Tests for applyClearcut (harvestBridge.R) and applyHarvestAction (actionDispatch.R)
# Both are tested here because applyHarvestAction is a thin dispatcher over applyClearcut.

# ── applyClearcut ─────────────────────────────────────────────────────────────
# mock_pixelGroupMap(): cells 1-2 → pixelGroup 1, cells 3-4 → 2, cells 5-6 → 3, 7-9 NA

test_that("applyClearcut empties only the cut cells", {
  ## Pixel groups are not contiguous and regenerated stands share them across thousands of
  ## pixels: clearing the whole group removed up to 54% more forest than ws3 allocated
  cd     <- mock_cohortData_binned()
  result <- applyClearcut(mock_harvest_raster(harvested_cells = 2L), cd, mock_pixelGroupMap())
  pgv    <- terra::values(result$pixelGroupMap, mat = FALSE)
  expect_equal(pgv[1:6], c(1L, 0L, 2L, 2L, 3L, 3L))   # 0 = forest with no cohorts (LandR)
  expect_equal(result$cohortData, cd)                 # pixel group 1 still has cell 1
})

test_that("applyClearcut drops the cohorts of pixel groups left with no cells", {
  cd     <- mock_cohortData_binned()
  result <- applyClearcut(mock_harvest_raster(harvested_cells = 1:2), cd, mock_pixelGroupMap())
  expect_false(1L %in% result$cohortData$pixelGroup)
  expect_equal(result$cohortData, cd[pixelGroup != 1L])
})

test_that("applyClearcut leaves NA cells NA and does not modify its inputs", {
  pgm    <- mock_pixelGroupMap()
  result <- applyClearcut(mock_harvest_raster(harvested_cells = c(1L, 8L)), mock_cohortData_binned(), pgm)
  expect_true(is.na(terra::values(result$pixelGroupMap, mat = FALSE)[8]))
  expect_equal(terra::values(pgm, mat = FALSE)[1], 1L)
})

test_that("applyClearcut with no harvested cells changes nothing", {
  cd     <- mock_cohortData_binned()
  pgm    <- mock_pixelGroupMap()
  result <- applyClearcut(mock_harvest_raster(harvested_cells = integer(0)), cd, pgm)
  expect_equal(result$cohortData, cd)
  expect_equal(terra::values(result$pixelGroupMap, mat = FALSE), terra::values(pgm, mat = FALSE))
})

# ── applyHarvestAction ────────────────────────────────────────────────────────

test_that("applyHarvestAction dispatches 'clearcut' to applyClearcut", {
  result <- applyHarvestAction("clearcut", mock_harvest_raster(1:2), mock_cohortData_binned(),
                               mock_pixelGroupMap())
  expect_false(1L %in% result$cohortData$pixelGroup)
})

test_that("applyHarvestAction warns and changes nothing for an unknown action", {
  cd <- mock_cohortData_binned()
  expect_warning(
    result <- applyHarvestAction("partial_cut", mock_harvest_raster(1L), cd, mock_pixelGroupMap()),
    regexp = "unknown action"
  )
  expect_equal(result$cohortData, cd)
})

# ── applyHarvestYear ──────────────────────────────────────────────────────────

test_that("applyHarvestYear applies that year's clearcut raster only", {
  ## ws3 allocates each year of a period separately; applying all ten at the period start
  ## removed stands up to 9 years before ws3 planned to cut them, while LandR grows yearly
  dir <- file.path(tempdir(), "harvestYear"); dir.create(dir, showWarnings = FALSE)
  terra::writeRaster(mock_harvest_raster(1:2), file.path(dir, "clearcut_2015.tif"), overwrite = TRUE)
  terra::writeRaster(mock_harvest_raster(3:4), file.path(dir, "clearcut_2016.tif"), overwrite = TRUE)
  result <- applyHarvestYear(2015, dir, mock_cohortData_binned(), mock_pixelGroupMap())
  expect_false(1L %in% result$cohortData$pixelGroup)
  expect_true(2L %in% result$cohortData$pixelGroup)
  expect_s4_class(result$harvestRast, "SpatRaster")
})

test_that("applyHarvestYear returns NULL when there is no raster for the year", {
  expect_null(applyHarvestYear(1999, tempdir(), mock_cohortData_binned(), mock_pixelGroupMap()))
})

mock_ws3_model <- function(missing) {
  reticulate::py_run_string(sprintf("
class _MockFm:
    applied = None
    def compile_schedule(self, problem):
        return [(('A', 'med', 'E'), 50, 1.0, 'clearcut', 1, '_existing')]
    def apply_schedule(self, schedule, **kwargs):
        self.applied = schedule
        return %s
", missing))
  reticulate::py_eval("_MockFm()")
}

test_that("applyWs3Solution applies the solved schedule, with its tuples intact", {
  ## add_problem() leaves only the last tree it built in applied_actions; solve() applies nothing.
  ## The schedule must stay in Python: through R, its dev type tuples become unhashable lists.
  skip_if_no_ws3()
  fm <- mock_ws3_model(0)
  applyWs3Solution(fm, "p")
  expect_true(reticulate::py_to_r(reticulate::py_eval("lambda fm: isinstance(fm.applied[0][0], tuple)")(fm)))
})

test_that("applyWs3Solution warns when the schedule cannot be fully applied", {
  skip_if_no_ws3()
  expect_warning(applyWs3Solution(mock_ws3_model(12.5), "p"), regexp = "12.5 ha")
})

test_that("ws3FlowConstraints keeps integer period keys and the tuple ws3 expects", {
  ## Through R the keys become strings: ws3 then finds no period in them and adds no constraint
  skip_if_no_ws3()
  e <- ws3FlowConstraints("cflw_vol", horizon = 3L, epsilon = 0.05)
  check <- reticulate::py_eval(
    "lambda e: isinstance(e['cflw_vol'], tuple) and e['cflw_vol'][0] == {1: 0.05, 2: 0.05, 3: 0.05} and e['cflw_vol'][1] == 1")
  expect_true(reticulate::py_to_r(check(e)))
})
