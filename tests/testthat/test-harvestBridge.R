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
