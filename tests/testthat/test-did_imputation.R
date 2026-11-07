## test-did_imputation.R -------------------------------------------------------
## Kyle Butts, CU Boulder Economics
##
## Test did_imputation function

data(df_hom, package = "didimputation")

test_that("estimation runs", {
  # Static, no formula
  expect_error(
    did_imputation(
      data = df_hom,
      yname = "dep_var",
      gname = "g",
      tname = "year",
      idname = "unit"
    ),
    NA
  )
  # Event Study
  expect_error(
    did_imputation(
      data = df_hom,
      yname = "dep_var",
      gname = "g",
      tname = "year",
      idname = "unit",
      horizon = T,
      pretrends = c(-2, -1)
    ),
    NA
  )
  # Weighted
  expect_error(
    did_imputation(
      data = df_hom,
      yname = "dep_var",
      gname = "g",
      tname = "year",
      idname = "unit",
      horizon = T,
      pretrends = c(-2, -1),
      wname = "weight"
    ),
    NA
  )
})
