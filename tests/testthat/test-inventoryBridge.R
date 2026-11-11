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
  pg3_row <- result[devTypeKey == pg3_key & age_class == 10L]
  if (nrow(pg3_row) > 0L)
    expect_false(pg3_row$harvestable[1L])
})

test_that("buildWs3Inventory harvestable is TRUE when age >= minHarvestAge", {
  # pixelGroup 1: age 50 >= minHarvestAge 40
  result  <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(),
                                periodLength = 10L, minHarvestAge = 40L)
  pg1_key <- devTypeTupleKey("Pice_mar", "med", "ECO_1")
  pg1_row <- result[devTypeKey == pg1_key & age_class == 50L]
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

test_that("buildWs3Inventory age_class is in years, floored to the period", {
  ## ws3 ages stands by period_length per period, and its curves and operability are in years
  cd <- mock_cohortData_binned()[pixelGroup == 2L][, age := 87L]
  result <- buildWs3Inventory(cd, mock_pixelArea(), periodLength = 10L, minHarvestAge = 40L)
  expect_equal(result$age_class, 80L)
})

test_that("buildWs3Inventory counts each pixel group's area once, under its dominant cohort", {
  ## pixelGroup 1 holds Pice_mar (B 5000) and Pice_gla (B 3000): only the first gets the area
  result <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(), periodLength = 10L, minHarvestAge = 40L)
  expect_equal(sum(result$area_ha), sum(mock_pixelArea()$area_ha))
  expect_equal(nrow(result[devTypeKey == devTypeTupleKey("Pice_gla", "med", "ECO_1")]), 0L)
})

test_that("buildWs3Inventory age_class is integer, as ws3 keys transitions by integer age", {
  ## A double age_class reached ws3 as a float (e.g. 9.0); ForestRaster.allocate_schedule then
  ## failed with KeyError: ('null', 9.0) looking up dtype transitions keyed by int age
  result <- buildWs3Inventory(mock_cohortData_binned(), mock_pixelArea(), periodLength = 10L, minHarvestAge = 40L)
  expect_type(result$age_class, "integer")
})

test_that("loadWs3Inventory replaces the period-0 inventory, including on a replan", {
  skip_if_no_ws3()
  ws3 <- reticulate::import("ws3")
  fm  <- ws3$forest$ForestModel(model_name = "t", model_path = tempdir(), base_year = 2011L,
                                horizon = 2L, period_length = 10L)
  fm$`_themes` <- reticulate::py_eval("[{}, {}, {}]")
  keys <- list(a = reticulate::tuple("A", "med", "E"), b = reticulate::tuple("B", "med", "E"))
  for (k in keys) fm$create_dtype_fromkey(k)

  first  <- data.table(devTypeKey = c("A|med|E", "B|med|E"), age_class = c(50L, 60L), area_ha = c(10, 20))
  second <- data.table(devTypeKey = "A|med|E", age_class = 70L, area_ha = 5)
  loadWs3Inventory(fm, first)
  loadWs3Inventory(fm, second)
  fm$reset()   # as .ws3Plan does: copies period 0 into period 1

  expect_equal(reticulate::py_to_r(fm$dt(keys$a)$area(1L)), 5)
  expect_equal(reticulate::py_to_r(fm$dt(keys$b)$area(1L)), 0)
})

test_that("loadWs3Inventory warns about area with no development type", {
  skip_if_no_ws3()
  ws3 <- reticulate::import("ws3")
  fm  <- ws3$forest$ForestModel(model_name = "t", model_path = tempdir(), base_year = 2011L,
                                horizon = 2L, period_length = 10L)
  fm$`_themes` <- reticulate::py_eval("[{}, {}, {}]")
  expect_warning(loadWs3Inventory(fm, data.table(devTypeKey = "A|med|E", age_class = 70L, area_ha = 5)),
                 regexp = "5 ha")
})
