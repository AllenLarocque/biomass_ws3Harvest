test_that("buildWs3Inventory returns data.table with required columns", {
  result <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                               periodLength = 10L, minHarvestAge = 40L)
  expect_s3_class(result, "data.table")
  expect_true(all(c("devTypeKey", "age_class", "area_ha", "harvestable") %in% names(result)))
})

test_that("buildWs3Inventory devTypeKey uses pipe-delimited format", {
  result <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                               periodLength = 10L, minHarvestAge = 40L)
  expect_true(all(grepl("^.+\\|.+\\|.+$", result$devTypeKey)))
})

test_that("buildWs3Inventory area_ha values are all positive", {
  result <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                               periodLength = 10L, minHarvestAge = 40L)
  expect_true(all(result$area_ha > 0))
})

test_that("buildWs3Inventory does not double-count pixel area within an age class", {
  # pixelGroup 1 has 2 cohorts but should contribute area_ha only once per devTypeKey+age_class
  cd <- data.table(
    pixelGroup     = c(1L, 1L),
    speciesCode    = c("Pice_mar", "Pice_mar"),
    ecoregionGroup = c("ECO_1",    "ECO_1"),
    age            = c(50L,        55L),          # same age_class (floor(50/10) = floor(55/10) = 5)
    B              = c(5000L,      5500L),
    aNPPAct        = c(100L,       105L),
    mortality      = c(50L,        55L),
    site_quality   = c("med",      "med")
  )
  pa     <- data.table(pixelGroup = 1L, area_ha = 6.25)
  result <- buildWs3Inventory(cd, pa, periodLength = 10L, minHarvestAge = 40L)
  key    <- devTypeTupleKey("Pice_mar", "med", "ECO_1")
  expect_equal(result[devTypeKey == key, area_ha], 6.25)
})

test_that("buildWs3Inventory harvestable is FALSE when age < minHarvestAge", {
  # pixelGroup 3: age 10 < minHarvestAge 40
  result  <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                                periodLength = 10L, minHarvestAge = 40L)
  pg3_key <- devTypeTupleKey("Pice_gla", "low", "ECO_1")
  pg3_row <- result[devTypeKey == pg3_key & age_class == 1L]
  if (nrow(pg3_row) > 0L)
    expect_false(pg3_row$harvestable[1L])
})

test_that("buildWs3Inventory harvestable is TRUE when age >= minHarvestAge", {
  # pixelGroup 1: age 50 >= minHarvestAge 40
  result  <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                                periodLength = 10L, minHarvestAge = 40L)
  pg1_key <- devTypeTupleKey("Pice_mar", "med", "ECO_1")
  pg1_row <- result[devTypeKey == pg1_key & age_class == 5L]
  if (nrow(pg1_row) > 0L)
    expect_true(pg1_row$harvestable[1L])
})

test_that("buildWs3Inventory errors when cohortData missing required column", {
  cd_bad <- mock_cohortData_binned()[, -"site_quality"]
  expect_error(
    buildWs3Inventory(cd_bad, mock_pixelArea(), periodLength = 10L),
    regexp = "site_quality"
  )
})

test_that("buildWs3Inventory errors when pixelArea missing area_ha column", {
  pa_bad <- data.table(pixelGroup = 1L)
  expect_error(
    buildWs3Inventory(mock_cohortData_binned(), pa_bad, periodLength = 10L),
    regexp = "area_ha"
  )
})

test_that("buildWs3Inventory warns when cohort has no pixelArea entry", {
  pa_partial <- mock_pixelArea()[pixelGroup != 3L]   # drop pixelGroup 3
  expect_warning(
    buildWs3Inventory(mock_cohortData_binned(), pa_partial, periodLength = 10L),
    regexp = "no matching pixelArea"
  )
})

test_that("buildWs3Inventory age_class bins follow floor(age / periodLength)", {
  result <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                               periodLength = 10L, minHarvestAge = 40L)
  # age 80 → age_class 8; age 50 → age_class 5; age 30 → age_class 3; age 10 → age_class 1
  expect_true(8L %in% result$age_class)
  expect_true(5L %in% result$age_class)
})
