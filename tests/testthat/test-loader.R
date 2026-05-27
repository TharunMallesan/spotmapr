test_that("detect_lat_lon finds named columns", {
  df <- data.frame(latitude = c(28.6, 19.0), longitude = c(77.2, 72.8),
                   outcome = c("case", "control"))
  result <- detect_lat_lon(df)
  expect_equal(result$lat, "latitude")
  expect_equal(result$lon, "longitude")
})

test_that("detect_lat_lon with explicit columns", {
  df <- data.frame(y = c(28.6, 19.0), x = c(77.2, 72.8))
  result <- detect_lat_lon(df, lat_col = "y", lon_col = "x")
  expect_equal(result$lat, "y")
  expect_equal(result$lon, "x")
})

test_that("detect_lat_lon errors on missing explicit columns", {
  df <- data.frame(a = 1, b = 2)
  expect_error(detect_lat_lon(df, lat_col = "lat", lon_col = "lon"),
               "not found")
})

test_that("detect_outcome finds standard column names", {
  df <- data.frame(lat = 1, lon = 2, outcome = c("case", "control", "case"))
  result <- detect_outcome(df)
  expect_equal(result$outcome_col, "outcome")
  expect_equal(result$case_value, "case")
})

test_that("detect_outcome uses explicit values", {
  df <- data.frame(status = c("pos", "neg", "pos"))
  result <- detect_outcome(df, outcome_col = "status", case_value = "pos")
  expect_equal(result$outcome_col, "status")
  expect_equal(result$case_value, "pos")
})

test_that("detect_outcome errors on missing column", {
  df <- data.frame(a = 1)
  expect_error(detect_outcome(df, outcome_col = "missing"), "not found")
})
