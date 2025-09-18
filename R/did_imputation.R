#' Borusyak, Jaravel, and Spiess (2021) Estimator
#'
#' @description
#' Treatment effect estimation and pre-trend testing in staggered adoption
#'   diff-in-diff designs with an imputation approach of Borusyak, Jaravel, and
#'   Spiess (2021)
#'
#' @details
#' The imputation-based estimator is a method of calculating treatment effects
#'   in a difference-in-differences framework. The method estimates a model for
#'   Y(0) using untreated/not-yet-treated observations and predicts Y(0) for the
#'   treated observations hat(Y_it(0)). The difference between treated and
#'   predicted untreated outcomes Y_it(1) - hat(Y_it(0)) serves as an estimate
#'   for the treatment effect for unit i in period t. These are then averaged to
#'   form average treatment effects for groups of (i, t).
#'
#' @import fixest
#' @import data.table
#'
#' @param data A `data.frame`
#' @param yname String. Variable name for outcome. Use `fixest` c() syntax
#'   for multiple lhs, e.g. `"c(y1, y2)"`.
#' @param idname String. Variable name for unique unit id.
#' @param gname String. Variable name for unit-specific date of treatment
#'   (never-treated should be zero or `NA`).
#' @param tname String. Variable name for calendar period.
#' @param first_stage Formula for Y(0).
#'   Formula following \code{\link[fixest:feols]{fixest::feols}}.
#'   Fixed effects specified after "`|`".
#'   If not specified, then just unit and time fixed effects will be used.
#' @param wname String. Variable name for estimation weights of observations.
#'   This is used in estimating Y(0) and also augments treatment effect weights.
#' @param wtr Character vector of treatment weight names
#'   (see horizon for standard static and event-study weights)
#' @param horizon Integer vector of event_time or `TRUE`. This only applies if `wtr` is left
#'   as `NULL`. if specified, weighted averages/sums of treatment effects will be
#'   reported for each of these horizons separately (i.e. tau0 for the treatment
#'   period, tau1 for one period after treatment, etc.).
#'   If `TRUE`, all horizons are used.
#'   If `wtr` and `horizon` are null, then the static treatment effect is calculated.
#' @param pretrends Integer vector or `TRUE`. Which pretrends to estimate.
#'   If `TRUE`, all `pretrends` are used.
#' @param cluster_var String. Variable name for clustering groups. If not
#'   supplied, then `idname` is used as default.
#'
#' @return A `data.frame` containing treatment effect term, estimate, standard
#'   error and confidence interval. This is in `tidy` format.
#'
#' @export
#'
#' @section Examples:
#'
#'
#' Load example dataset which has two treatment groups and homogeneous treatment effects
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Load Example Dataset
#' data("df_hom", package="didimputation")
#' ```
#' ### Static TWFE
#'
#' You can run a static TWFE fixed effect model for a simple treatment indicator
#' ```{r, comment = "#>", collapse = TRUE}
#' did_imputation(data = df_hom, yname = "dep_var", gname = "g",
#'                tname = "year", idname = "unit")
#' ```
#'
#' ### Event Study
#'
#' Or you can use relative-treatment indicators to estimate an event study estimate
#' ```{r, comment = "#>", collapse = TRUE}
#' did_imputation(data = df_hom, yname = "dep_var", gname = "g",
#'                tname = "year", idname = "unit", horizon=TRUE)
#' ```
#'
#' ### Example from Cheng and Hoekstra (2013)
#'
#' Here's an example using data from Cheng and Hoekstra (2013)
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' # Castle Data
#' castle = haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did_imputation(data = castle, yname = "c(l_homicide, l_assault)", gname = "effyear",
#'               first_stage = ~ 0 | sid + year,
#'               tname = "year", idname = "sid")
#' ```
#'
did_imputation = function(
  data,
  yname,
  gname,
  tname,
  idname,
  first_stage = NULL,
  wname = NULL,
  wtr = NULL,
  horizon = NULL,
  pretrends = NULL,
  cluster_var = NULL
) {
  # CRAN Errors
  zz000treat = zz000event_time = zz000weight = zz000adj = zz000treat = NULL

  # Set-up Parameters ----------------------------------------------------------

  # Extract first stage vars from formula
  if (is.null(first_stage)) {
    first_stage = paste0("0 | ", idname, " + ", tname)
  } else if (inherits(first_stage, "formula")) {
    first_stage = as.character(first_stage)[[2]]
  }

  # Formula for fitting the first stage
  formula = stats::as.formula(paste0(yname, " ~ ", first_stage))

  # make local copy of data, convert to data.table
  data = setDT(copy(data))

  # Treatment indicator
  data$zz000treat = 1 * (data[[tname]] >= data[[gname]]) * (data[[gname]] > 0)

  # if g is NA
  data[is.na(zz000treat), zz000treat := 0]

  # Create event time
  data$zz000event_time = ifelse(
    is.na(data[[gname]]) | data[[gname]] == 0 | data[[gname]] == Inf,
    -Inf,
    as.numeric(data[[tname]] - data[[gname]])
  )

  # Get list of event_time
  event_time = data[is.finite(zz000event_time), unique(zz000event_time)]

  # horizon/allhorizon options
  if (is.null(wtr)) {
    # event-study
    if (!is.null(horizon)) {
      # create event time weights

      # allhorizons
      if (all(horizon == TRUE)) {
        horizon = event_time
      }

      # Create wtr of horizons
      wtr = paste0("zz000wtr", horizon[horizon >= 0])
      for (e in horizon[horizon >= 0]) {
        data[[paste0("zz000wtr", e)]] = +(data$zz000event_time == e)
      }
    } else {
      wtr = "zz000wtrtreat"
      data[[wtr]] = +(data$zz000treat == 1)
    }
  }

  # Weights specified or not
  if (is.null(wname)) {
    data$zz000weight = 1
  } else {
    data$zz000weight = data[[wname]]
  }

  # First Stage estimate -------------------------------------------------------

  # Estimate Y(0) using untreated observations
  first_stage_est = fixest::feols(
    formula,
    se = "standard",
    data[zz000treat == 0, ],
    weights = ~zz000weight,
    warn = FALSE,
    notes = FALSE
  )

  if (inherits(first_stage_est, "fixest_multi")) {
    yvars = lapply(first_stage_est, \(est) est$model_info$lhs)
    yvars = unname(unlist(yvars))
  } else {
    yvars = as.character(stats::terms(formula)[1][[2]])
  }
  names(yvars) = yvars

  # Residualize outcome variable(s)
  if (length(yvars) == 1) {
    data[[paste0("zz000adj_", yvars)]] =
      data[[yvars]] - stats::predict(first_stage_est, newdata = data)
  } else {
    for (est in first_stage_est) {
      yvar = est$model_info$lhs
      data[[paste0("zz000adj_", yvar)]] =
        data[[yvar]] - stats::predict(est, newdata = data)
    }
  }

  # drop anything with missing values of the residualized outcome
  todrop = apply(
    is.na(data[, paste("zz000adj", yvars, sep = "_"), with = F]),
    MARGIN = 1,
    FUN = any
  )
  data = data[!todrop, ]

  # Multiply treatment weights * weights vector
  data[, (wtr) := lapply(.SD, function(x) x * zz000weight), .SDcols = wtr]
  data[is.na(zz000weight), (wtr) := 0]
  # Normalize
  data[, (wtr) := lapply(.SD, function(x) x / sum(x)), .SDcols = wtr]

  # Point estimate for wtr
  ests = lapply(yvars, function(yvar) {
    data$zz000adj = data[[paste0("zz000adj_", yvar)]]
    data[
      data$zz000treat == 1,
      lapply(.SD, function(x) sum(x * zz000adj)),
      .SDcols = wtr
    ]
  })
  ests = data.table::rbindlist(ests, idcol = "lhs")

  # Standard Errors ------------------------------------------------------------
  if (length(yvars) == 1) {
    Z = fixest::sparse_model_matrix(
      first_stage_est,
      data = data,
      type = c("rhs", "fixef")
    )
  } else {
    Z = fixest::sparse_model_matrix(
      first_stage_est[[1]],
      data = data,
      type = c("rhs", "fixef")
    )
  }

  # Equation (6) of Borusyak et. al. 2021
  # - Z (Z_0' Z_0)^{-1} Z_1' wtr_1
  Z = Z * data$zz000weight
  wtr_mat = Matrix::Matrix(
    as.matrix(data[data$zz000treat == 1, wtr, with = FALSE]),
    sparse = TRUE
  )

  Z1 = copy(Z)
  Z1 = Z1[which(data$zz000treat == 1), ]

  Z0 = copy(Z)
  Z0 = Z0[which(data$zz000treat == 0), ]

  Z1_wtr = Matrix::crossprod(Z1, wtr_mat)
  S_Z0Z0 = Matrix::crossprod(Z0)

  v_star = -1 * Z %*% Matrix::solve(S_Z0Z0, Z1_wtr)

  # fix v_it^* = w for treated observations
  v_star[data$zz000treat == 1, ] =
    as.matrix(data[data$zz000treat == 1, wtr, with = FALSE])

  # If no cluster_var, then use idname
  if (is.null(cluster_var)) {
    cluster_var = idname
  }

  ses = lapply(yvars, function(yvar) {
    data$zz000adj = data[[paste0("zz000adj_", yvar)]]
    se_inner(data, v_star, wtr, cluster_var, gname)
  })
  ses = rbindlist(ses, idcol = "lhs")

  # Pre-event Estimates --------------------------------------------------------

  if (!is.null(pretrends) & !all(pretrends == FALSE)) {
    if (all(pretrends == TRUE)) {
      pre_formula = stats::as.formula(
        paste0(yname, " ~ i(zz000event_time) + ", first_stage)
      )
    } else {
      if (all(pretrends %in% event_time)) {
        pre_formula = stats::as.formula(
          paste0(
            yname,
            " ~ i(zz000event_time, keep = c(",
            paste(pretrends, collapse = ', '),
            ")) + ",
            first_stage
          )
        )
      } else {
        stop(paste0(
          "Pretrends not found in event_time. Event_time has values",
          event_time
        ))
      }
    }

    pre_est = fixest::feols(
      pre_formula,
      data[data$zz000treat == 0, ],
      cluster = cluster_var,
      weights = ~zz000weight,
      warn = FALSE,
      notes = FALSE
    )
  }

  # Create dataframe of results in tidy format ---------------------------------
  ests = data.table::melt(
    ests,
    id.vars = "lhs",
    variable.name = "term",
    value.name = "estimate"
  )
  ses = data.table::melt(
    ses,
    id.vars = "lhs",
    variable.name = "term",
    value.name = "std.error"
  )

  out = merge(ests, ses, by = c("lhs", "term"))

  out$term = gsub("zz000wtr", "", out$term)
  out$conf.low = out$estimate - 1.96 * out$std.error
  out$conf.high = out$estimate + 1.96 * out$std.error

  if (!is.null(pretrends) && !all(pretrends == FALSE)) {
    if (length(yvars) == 1) {
      pre_out = pre_est$coeftable
      pre_out = as.data.table(pre_out, keep.rownames = "term")
      pre_out$lhs = yvars
      setnames(
        pre_out,
        c("term", "estimate", "std.error", "t_value", "p_value", "lhs")
      )
    } else {
      pre_out = lapply(pre_est, function(est) {
        out = est$coeftable
        as.data.table(out, keep.rownames = "term")
      })
      names(pre_out) = yvars
      pre_out = rbindlist(pre_out, idcol = "lhs")
      setnames(
        pre_out,
        c("lhs", "term", "estimate", "std.error", "t_value", "p_value")
      )
    }

    pre_out = pre_out[grep("zz000event_time", pre_out$term), ]
    pre_out$term = gsub("zz000event_time::", "", pre_out$term)
    pre_out$conf.low = pre_out$estimate - 1.96 * pre_out$std.error
    pre_out$conf.high = pre_out$estimate + 1.96 * pre_out$std.error

    pre_out = pre_out[, c(
      "lhs",
      "term",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high"
    )]

    out = rbind(pre_out, out)
  }

  if (all(wtr != "zz000wtrtreat")) {
    out = out[order(out$lhs, as.numeric(out$term)), ]
  }

  if (length(yvars) == 1) {
    out$lhs = NULL
  }

  return(out)
}


se_inner = function(data, v_star, wtr, cluster_var, gname) {
  # CRAN Errors
  zz000adj = zz000treat = NULL

  vcols = paste0("zz000v", seq_along(wtr))
  tcols = paste0("zz000tau_et", seq_along(wtr))

  # Calculate v_it^* = - Z (Z_0' Z_0)^{-1} Z_1' * w_1
  for (i in seq_along(vcols)) {
    data[, vcols[i] := v_star[, i]]
  }

  # Equation (10) of Borusyak et. al. 2021
  # Calculate tau_it - \bar{\tau}_{et}
  data[,
    (tcols) := lapply(.SD, function(x) {
      sum(x^2 * zz000adj) / sum(x^2) * zz000treat
    }),
    by = c(gname, "zz000event_time"),
    .SDcols = vcols
  ]

  # Recenter tau by \bar{\tau}_{et}
  data[,
    (tcols) := lapply(.SD, function(x) {
      x[is.na(x)] = 0
      zz000adj - x
    }),
    .SDcols = tcols
  ]

  # Equation (8)
  # Calculate variance of estimate
  result = data[,
    lapply(seq_along(vcols), function(idx) {
      sum(.SD[[vcols[idx]]] * .SD[[tcols[idx]]])^2
    }),
    by = cluster_var
  ][,
    lapply(.SD, function(x) sqrt(sum(x))),
    .SDcols = paste0("V", seq_along(wtr))
  ]

  result = setnames(result, wtr)

  data[, c(vcols, tcols) := NULL]

  return(result)
}
