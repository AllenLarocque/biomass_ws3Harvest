# modules/biomass_ws3Harvest/tests/testthat/test-spatialBridge.R
library(testthat)
library(data.table)
library(terra)

test_that("buildInventoryRaster returns named list with path and hdt_table", {
  skip_if_no_ws3()
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  expect_type(result, "list")
  expect_named(result, c("path", "hdt_table"))
})

test_that("buildInventoryRaster writes a 3-band INT4S GeoTIFF", {
  skip_if_no_ws3()
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  expect_true(file.exists(result$path))
  r <- terra::rast(result$path)
  expect_equal(terra::nlyr(r), 3L)
  expect_equal(terra::datatype(r)[1L], "INT4S")
})

test_that("buildInventoryRaster output path is named inventory_{base_year}.tif", {
  skip_if_no_ws3()
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2025L,
    out_dir       = tmp
  )
  expect_equal(basename(result$path), "inventory_2025.tif")
})

test_that("buildInventoryRaster hdt_table has required columns with correct types", {
  skip_if_no_ws3()
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  ht <- result$hdt_table
  expect_s3_class(ht, "data.table")
  expect_true(all(c("hash_val", "speciesCode", "site_quality", "ecoregionGroup") %in% names(ht)))
  expect_type(ht$hash_val, "integer")   # must be integer, not double
})

test_that("buildInventoryRaster encodes non-forest cells as 0 in all bands", {
  skip_if_no_ws3()
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  r <- terra::rast(result$path)
  # mock_pixelGroupMap() has cells 7-9 as NA (no forest)
  vals_b1 <- terra::values(r[[1L]], mat = FALSE)
  pg_vals <- terra::values(mock_pixelGroupMap(), mat = FALSE)
  na_idx  <- which(is.na(pg_vals))
  ## 0 is the file's nodata value: terra reads it back as NA, rasterio (ws3) as 0
  expect_true(all(is.na(vals_b1[na_idx])))
})

test_that("buildInventoryRaster band 2 is the dominant cohort age in years, as in the inventory", {
  skip_if_no_ws3()
  tmp <- tempdir()
  # mock_cohortData_binned(): pixelGroup 2 dominant cohort has age=80, period_length=10
  # → age class 80, the value buildWs3Inventory gives ws3 for the same stand
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  r      <- terra::rast(result$path)
  b2     <- terra::values(r[[2L]], mat = FALSE)
  pg_map <- terra::values(mock_pixelGroupMap(), mat = FALSE)
  pg2_cells <- which(pg_map == 2L)
  expect_true(all(b2[pg2_cells] == 80L))
})

test_that("buildInventoryRaster band 3 equals pixelGroup ID (block ID)", {
  skip_if_no_ws3()
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  r      <- terra::rast(result$path)
  b3     <- terra::values(r[[3L]], mat = FALSE)
  pg_map <- terra::values(mock_pixelGroupMap(), mat = FALSE)
  forested <- which(!is.na(pg_map))
  expect_equal(b3[forested], pg_map[forested])
})

test_that("buildInventoryRaster selects dominant cohort by max B", {
  skip_if_no_ws3()
  # pixelGroup 1 has two cohorts: Pice_mar B=5000 (dominant) and Pice_gla B=3000
  # band 1 hash for PG 1 must match hash_dt(('Pice_mar','med','ECO_1'))
  tmp <- tempdir()
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = mock_pixelGroupMap(),
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tmp
  )
  ws3c   <- reticulate::import("ws3.common")
  expected_hash <- as.integer(reticulate::py_to_r(
    ws3c$hash_dt(reticulate::tuple("Pice_mar", "med", "ECO_1"))
  ))
  r      <- terra::rast(result$path)
  b1     <- terra::values(r[[1L]], mat = FALSE)
  pg_map <- terra::values(mock_pixelGroupMap(), mat = FALSE)
  pg1_cells <- which(pg_map == 1L)
  expect_true(all(b1[pg1_cells] == expected_hash))
})

test_that("buildInventoryRaster errors on missing required column", {
  cd_bad <- mock_cohortData_binned()[, -"site_quality"]
  expect_error(
    buildInventoryRaster(cd_bad, mock_pixelGroupMap(), 10L, 2011L, tempdir()),
    regexp = "site_quality"
  )
})

test_that("buildInventoryRaster encodes orphan pixelGroup (in map, absent from cohortData) as 0", {
  skip_if_no_ws3()
  # PG 4 is present in map but has no cohortData row — must be encoded as 0 (non-forest)
  r_extra <- terra::rast(nrows = 1L, ncols = 4L,
                         xmin = 0, xmax = 4, ymin = 0, ymax = 1,
                         crs = "EPSG:4326")
  terra::values(r_extra) <- c(1L, 2L, 3L, 4L)
  result <- buildInventoryRaster(
    cohortData    = mock_cohortData_binned(),
    pixelGroupMap = r_extra,
    period_length = 10L,
    base_year     = 2011L,
    out_dir       = tempdir()
  )
  b1 <- terra::values(terra::rast(result$path)[[1L]], mat = FALSE)
  expect_true(is.na(b1[4L]))   # written as 0, the nodata value, which terra reads as NA
})

test_that("ws3ExactAgeAllocation lets ForestRaster take only cells of the scheduled age", {
  ## ws3's _transition_cells accepts cells within one period length of the scheduled age, so an
  ## age-40 prescription could cut age-30 cells, below the minimum harvest age
  skip_if_no_ws3()
  reticulate::py_run_string("
import numpy as _np
class _MockFr:
    def __init__(self):
        self._x = _np.array([[[7, 7, 7]], [[30, 40, 50]], [[1, 2, 3]]], dtype=_np.int32)
    def _transition_cells(self, from_dtk, from_age, to_dtk, to_age, tarea, acode, dy, **kw):
        ages = self._x[1]
        ages[_np.abs(ages - from_age) <= 10] = to_age   # ws3's tolerance, as in spatial.py
        return 0.
")
  fr <- reticulate::py_eval("_MockFr()")
  ws3ExactAgeAllocation(fr)
  fr$`_transition_cells`("k", 40L, "k", 1L, 1, "clearcut", 0L)
  expect_equal(as.integer(reticulate::py_to_r(fr$`_x`)[2, 1, ]), c(30L, 1L, 50L))
})
