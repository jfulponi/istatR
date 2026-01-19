test_that("make_url_key works correctly", {
  # Single values
  filters1 <- list(FREQ = "M", TIPO_DATO = "ISAV")
  expect_equal(make_url_key(filters1), "M.ISAV")

  # Multiple values for a dimension
  filters2 <- list(FREQ = "M", TIPO_DATO = c("ISAV", "ESAV"))
  expect_equal(make_url_key(filters2), "M.ISAV+ESAV")

  # All values (dot notation)
  filters3 <- list(FREQ = ".", TIPO_DATO = "ISAV")
  expect_equal(make_url_key(filters3), "..ISAV")

  # Empty filters
  expect_equal(make_url_key(list()), "")
})

test_that("parse_time_period handles various formats", {
  # Annual format
  expect_equal(parse_time_period("2020"), as.Date("2020-01-01"))

  # Monthly format
  expect_equal(parse_time_period("2020-06"), as.Date("2020-06-01"))

  # Quarterly format
  expect_equal(parse_time_period("2020-Q1"), as.Date("2020-01-01"))
  expect_equal(parse_time_period("2020-Q2"), as.Date("2020-04-01"))
  expect_equal(parse_time_period("2020-Q3"), as.Date("2020-07-01"))
  expect_equal(parse_time_period("2020-Q4"), as.Date("2020-10-01"))

  # Semester format
  expect_equal(parse_time_period("2020-S1"), as.Date("2020-01-01"))
  expect_equal(parse_time_period("2020-S2"), as.Date("2020-07-01"))
})
