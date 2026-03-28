# Tests for applyClearcut (harvestBridge.R) and applyHarvestAction (actionDispatch.R)
# Both are tested here because applyHarvestAction is a thin dispatcher over applyClearcut.

# ── applyClearcut ─────────────────────────────────────────────────────────────

test_that("applyClearcut sets B = 0 for harvested pixel groups", {
  cd  <- mock_cohortData_binned()
  pgm <- mock_pixelGroupMap()
  hr  <- mock_harvest_raster(harvested_cells = 1L)   # cell 1 → pixelGroup 1
  result <- applyClearcut(hr, cd, pgm)
  expect_true(all(result[pixelGroup == 1L, B] == 0L))
})

test_that("applyClearcut sets aNPPAct = 0 for harvested pixel groups", {
  cd  <- mock_cohortData_binned()
  result <- applyClearcut(mock_harvest_raster(1L), cd, mock_pixelGroupMap())
  expect_true(all(result[pixelGroup == 1L, aNPPAct] == 0L))
})

test_that("applyClearcut sets mortality = original B before zeroing", {
  cd           <- mock_cohortData_binned()
  original_B   <- cd[pixelGroup == 1L, B]
  result       <- applyClearcut(mock_harvest_raster(1L), cd, mock_pixelGroupMap())
  expect_equal(result[pixelGroup == 1L, mortality], original_B)
})

test_that("applyClearcut leaves unharvested pixel groups unchanged", {
  cd     <- mock_cohortData_binned()
  result <- applyClearcut(mock_harvest_raster(1L), cd, mock_pixelGroupMap())
  expect_equal(result[pixelGroup == 2L, B], cd[pixelGroup == 2L, B])
  expect_equal(result[pixelGroup == 3L, B], cd[pixelGroup == 3L, B])
})

test_that("applyClearcut harvests entire pixel group when any cell is harvested", {
  # cells 1 and 2 both map to pixelGroup 1 — harvesting cell 2 should also clear cell 1 cohorts
  cd     <- mock_cohortData_binned()
  result <- applyClearcut(mock_harvest_raster(harvested_cells = 2L), cd, mock_pixelGroupMap())
  expect_true(all(result[pixelGroup == 1L, B] == 0L))
})

test_that("applyClearcut returns a data.table", {
  result <- applyClearcut(mock_harvest_raster(1L), mock_cohortData_binned(), mock_pixelGroupMap())
  expect_s3_class(result, "data.table")
})

test_that("applyClearcut preserves row count", {
  cd     <- mock_cohortData_binned()
  result <- applyClearcut(mock_harvest_raster(1L), cd, mock_pixelGroupMap())
  expect_equal(nrow(result), nrow(cd))
})

test_that("applyClearcut with no harvested cells leaves cohortData unchanged", {
  cd     <- mock_cohortData_binned()
  hr_empty <- mock_harvest_raster(harvested_cells = integer(0))
  result <- applyClearcut(hr_empty, cd, mock_pixelGroupMap())
  expect_equal(result$B, cd$B)
})

# ── applyHarvestAction ────────────────────────────────────────────────────────

test_that("applyHarvestAction dispatches 'clearcut' to applyClearcut", {
  cd     <- mock_cohortData_binned()
  result <- applyHarvestAction("clearcut",
                                mock_harvest_raster(1L), cd, mock_pixelGroupMap())
  expect_true(all(result[pixelGroup == 1L, B] == 0L))
})

test_that("applyHarvestAction warns and returns cohortData unchanged for unknown action", {
  cd <- mock_cohortData_binned()
  expect_warning(
    result <- applyHarvestAction("partial_cut",
                                  mock_harvest_raster(1L), cd, mock_pixelGroupMap()),
    regexp = "unknown action"
  )
  expect_equal(result$B, cd$B)
})
