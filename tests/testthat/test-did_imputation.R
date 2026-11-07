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

test_that("leaveout keeps estimates and inflates standard errors", {
  set.seed(20261107)
  units = data.frame(unit = 1:109, g = c(rep(0, 100), rep(2005, 5), rep(2008, 4)))
  df = merge(units, data.frame(year = 2001:2010))
  df$dep_var = df$unit / 100 + df$year / 10 + (df$g > 0 & df$year >= df$g) + rnorm(nrow(df))

  est = did_imputation(df, yname = "dep_var", gname = "g", tname = "year",
                       idname = "unit", horizon = 0:2)
  est_lo = did_imputation(df, yname = "dep_var", gname = "g", tname = "year",
                          idname = "unit", horizon = 0:2, leaveout = TRUE)
  expect_equal(est_lo$estimate, est$estimate)
  expect_true(all(est_lo$std.error > est$std.error))

  # A cohort with one unit has no leave-out mean
  one_unit = subset(df, !(g == 2008 & unit != 106))
  expect_error(
    did_imputation(one_unit, yname = "dep_var", gname = "g", tname = "year",
                   idname = "unit", horizon = 0:2, leaveout = TRUE),
    "only cluster"
  )
})
