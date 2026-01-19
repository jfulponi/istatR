test_that("all_available returns a tibble", {
  skip_if_offline()
  skip_on_cran()

  datasets <- all_available()

  expect_s3_class(datasets, "tbl_df")
  expect_true(nrow(datasets) > 0)
  expect_true(all(c("df_id", "version", "df_description", "df_structure_id") %in% names(datasets)))
})

test_that("search_dataset filters by keyword", {
  skip_if_offline()
  skip_on_cran()

  results <- search_dataset("population")

  expect_s3_class(results, "tbl_df")
  expect_true(all(grepl("population", results$df_description, ignore.case = TRUE)))
})

test_that("search_dataset warns when no results found", {
  skip_if_offline()
  skip_on_cran()

  expect_warning(
    search_dataset("xyznonexistentkeyword123"),
    "No datasets found"
  )
})

test_that("istat_dataset creates valid dataset object", {
  skip_if_offline()
  skip_on_cran()

  # First get a valid dataset ID
  datasets <- all_available()
  df_id <- datasets$df_id[1]

  ds <- istat_dataset(df_id)

  expect_s3_class(ds, "istat_dataset")
  expect_equal(ds$df_id, df_id)
  expect_true(length(ds$dimensions) > 0)
  expect_true(length(ds$filters) > 0)
})

test_that("set_filters modifies dataset filters", {
  skip_if_offline()
  skip_on_cran()

  datasets <- all_available()
  df_id <- datasets$df_id[1]
  ds <- istat_dataset(df_id)

  # Get first dimension name
  dim_name <- names(ds$dimensions)[1]

  # Set filter using first dimension
  ds <- set_filters(ds, setNames(list("TEST"), dim_name))

  expect_equal(ds$filters[[dim_name]], "TEST")
})
