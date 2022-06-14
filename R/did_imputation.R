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
#'   form average treatment effects for groups of {it}.
#'
#' @import fixest
#' @import data.table
#'
#' @param data A `data.frame`
#' @param yname String. Variable name for outcome. Use `fixest` c() syntax
#'   for multiple lhs.
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
#' @param cluster_var String. Varaible name for clustering groups. If not
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
#' data("df_hom", package="did2s")
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
#' castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
#'
#' did_imputation(data = castle, yname = "c(l_homicide, l_assault)", gname = "effyear",
#'               first_stage = ~ 0 | sid + year,
#'               tname = "year", idname = "sid")
#' ```
#'
did_imputation <- function(data, yname, gname, tname, idname, first_stage = NULL,
                           wname = NULL, wtr = NULL, horizon = NULL,
                           pretrends = NULL, cluster_var = NULL) {


  # Set-up Parameters ------------------------------------------------------------

  # Extract first stage vars from formula
  if (is.null(first_stage)) {
    first_stage <- glue::glue("0 | {idname} + {tname}")
  } else if (inherits(first_stage, "formula")) {
    first_stage <- as.character(first_stage)[[2]]
  }

  # Formula for fitting the first stage
  formula <- stats::as.formula(glue::glue("{yname} ~ {first_stage}"))

  # dummy estimation to extract needed variables
  fixest_env <- feols(formula, data, lean = T, only.env = T, warn = F, notes = F)

  # extract lhs vars (allows fixest style multiple lhs specification)
  yvars <- fixest_env$lhs_names

  # extract fe vars
  fevars <- fixest_env$fixef_vars %>%
    stringr::str_extract_all("\\w+") %>%
    unlist()

  # extract rhs vars
  rhsvars <- fixest_env$linear.params %>%
    stringr::str_split("(?<!:):(?!:)") %>%
    unlist() %>%
    stringr::str_replace("::.*", "") %>%
    unique()

  # make local copy of data, convert to data.table
  needed_vars <- c(yvars, gname, tname, idname, wname, wtr, rhsvars, fevars, cluster_var) %>% unique()
  data <- copy(data[, needed_vars, with = F]) %>% setDT()
  rm(fixest_env)

  setDT(data)

  # Treatment indicator
  data[, zz000treat := 1 * (.SD[[tname]] >= .SD[[gname]]) * (.SD[[gname]] > 0)]

  # if g is NA
  data[is.na(zz000treat), zz000treat := 0]

  # Create event time
  data[, zz000event_time := dplyr::if_else(is.na(.SD[[gname]]) | .SD[[gname]] == 0 | .SD[[gname]] == Inf,
    -Inf,
    as.numeric(.SD[[tname]] - .SD[[gname]])
  )]

  # Get list of event_time
  event_time <- unique(data[, zz000event_time]) %>% purrr::keep(is.finite)

  # horizon/allhorizon options
  if (is.null(wtr)) {

    # event-study
    if (!is.null(horizon)) {
      # create event time weights

      # allhorizons
      if (all(horizon == TRUE)) horizon <- event_time

      # Create wtr of horizons
      wtr <- paste0("zz000wtr", event_time[event_time >= 0])
      purrr::walk2(
        event_time[event_time >= 0], wtr,
        function(e, v) data[, (v) := dplyr::if_else(is.na(zz000event_time), 0, 1 * (zz000event_time == e))]
      )
    } else {
      wtr <- "zz000wtrtreat"
      data[, (wtr) := 1 * (zz000treat == 1)]
    }
  }

  # Weights specified or not
  if (is.null(wname)) {
    data[, zz000weight := 1]
  } else {
    data[, zz000weight := .SD[[wname]]]
  }

  # First Stage estimate ---------------------------------------------------------

  # Estimate Y(0) using untreated observations
  first_stage_est <- fixest::feols(formula,
    se = "standard",
    data[zz000treat == 0, ],
    weights = ~zz000weight,
    warn = FALSE, notes = FALSE
  )

  # Residualize outcome variable(s)
  if (length(yvars) == 1) {
    data[, (paste("zz000adj", yvars, sep = "_")) := .SD[[yname]] - stats::predict(first_stage_est, newdata = data)]
  } else {
    data[, (paste("zz000adj", yvars, sep = "_")) := purrr::imap(.SD, ~ .x - stats::predict(first_stage_est[lhs = .y], newdata = data)),
      .SDcols = yvars
    ]
  }

  # drop anything with missing values of the residualized outcome
  todrop <- apply(is.na(data[, paste("zz000adj", yvars, sep = "_"), with = F]),
    MARGIN = 1,
    FUN = any
  )
  data <- data[!todrop, ]

  data[, (wtr) := purrr::map(.SD, ~ . * zz000weight), .SDcols = wtr] # Multiply treatment weights * weights vector
  data[is.na(zz000weight), (wtr) := 0]
  data[, (wtr) := purrr::map(.SD, ~ . / sum(.)), .SDcols = wtr] # Normalize


  # Point estimate for wtr
  ests <- yvars %>%
    purrr::set_names(yvars) %>%
    purrr::map(function(y) data[, zz000adj := .SD[[paste("zz000adj", y, sep = "_")]]][zz000treat == 1, purrr::map(.SD, ~ sum(. * zz000adj)), .SDcols = wtr]) %>%
    rbindlist(idcol = "lhs")

  # Standard Errors --------------------------------------------------------------
  if (length(yvars) == 1) {
    Z <- sparse_model_matrix(data, first_stage_est)
  } else {
    Z <- sparse_model_matrix(data, first_stage_est[[1]])
  }

  # Equation (6) of Borusyak et. al. 2021
  # - Z (Z_0' Z_0)^{-1} Z_1' wtr_1
  v_star <- make_V_star(
    (Z * data[, zz000weight]),
    (Z * data[, zz000weight])[data$zz000treat == 0, ],
    (Z * data[, zz000weight])[data$zz000treat == 1, ],
    Matrix::Matrix(as.matrix(data[zz000treat == 1, wtr, with = F]), sparse = TRUE)
  )

  # fix v_it^* = w for treated observations
  v_star[data$zz000treat == 1, ] <- as.matrix(data[zz000treat == 1, wtr, with = F])

  # If no cluster_var, then use idname
  if (is.null(cluster_var)) cluster_var <- idname

  ses <- yvars %>%
    purrr::set_names(yvars) %>%
    purrr::map(function(y) se_inner(data[, zz000adj := .SD[[paste("zz000adj", y, sep = "_")]]], v_star, wtr, cluster_var, gname)) %>%
    rbindlist(idcol = "lhs")


  # Pre-event Estimates ----------------------------------------------------------

  if (!is.null(pretrends)) {
    if (all(pretrends == TRUE)) {
      pre_formula <- stats::as.formula(glue::glue("{yname} ~ i(zz000event_time) + {first_stage}"))
    } else {
      if (all(pretrends %in% event_time)) {
        pre_formula <- stats::as.formula(
          glue::glue("{yname} ~ i(zz000event_time, keep = c({paste(pretrends, collapse = ', ')}))  + {first_stage}")
        )
      } else {
        stop(glue::glue("Pretrends not found in event_time. Event_time has values {event_time}"))
      }
    }

    pre_est <- fixest::feols(pre_formula, data[zz000treat == 0, ], weights = ~zz000weight, warn = FALSE, notes = FALSE)
  }


  # Create dataframe of results in tidy format -----------------------------------
  ests <- ests %>%
  	melt(id.vars = "lhs", variable.name = "term", value.name = "estimate")
  ses <- ses %>%
  	melt(id.vars = "lhs", variable.name = "term", value.name = "std.error")

  out <- ests[ses, on = .(lhs, term)] %>%
    .[, term := as.character(stringr::str_replace(term, "zz000wtr", ""))] %>%
    .[, `:=`(
    	conf.low = estimate - 1.96 * std.error,
    	conf.high = estimate + 1.96 * std.error
	)]


  if (!is.null(pretrends)) {
    if (length(yvars) == 1) {
      pre_out <- broom::tidy(pre_est)
      pre_out$lhs <- yvars
    } else {
      pre_out <- pre_est %>%
        purrr::map(broom::tidy) %>%
        dplyr::bind_rows(.id = "lhs")
    }

    pre_out$term <- stringr::str_remove(pre_out$term, "zz000event_time::")
    pre_out$term <- as.character(pre_out$term)

    pre_out$conf.low <- pre_out$estimate - 1.96 * pre_out$std.error
    pre_out$conf.high <- pre_out$estimate + 1.96 * pre_out$std.error

    pre_out <- pre_out[, c("lhs", "term", "estimate", "std.error", "conf.low", "conf.high")]

    out <- dplyr::bind_rows(pre_out, out)
  }

  return(dplyr::as_tibble(out))
}


se_inner <- function(data, v_star, wtr, cluster, gname) {
  # Calculate v_it^* = - Z (Z_0' Z_0)^{-1} Z_1' * w_1
  vcols <- paste0("zz000v", seq_along(wtr))
  tcols <- paste0("zz000tau_et", seq_along(wtr))
  data[, (vcols) := split(v_star, col(v_star))]

  # Equation (10) of Borusyak et. al. 2021
  # Calculate tau_it - \bar{\tau}_{et}
  data[,
    (tcols) := purrr::map(.SD, ~ sum(.^2 * zz000adj) / sum(.^2) * zz000treat),
    by = c(gname, "zz000event_time"),
    .SDcols = vcols
  ]

  # Recenter tau by \bar{\tau}_{et}
  data[, (tcols) := purrr::map(.SD, ~ zz000adj - tidyr::replace_na(., 0)), .SDcols = tcols]

  # Equation (8)
  # Calculate variance of estimate
  result <- data[!is.infinite(zz000event_time), purrr::map2(vcols, tcols, ~ sum(.SD[[.x]] * .SD[[.y]])^2),
    by = cluster
  ] %>%
    .[, purrr::map(.SD, ~ sqrt(sum(.))), .SDcols = paste0("V", seq_along(wtr))] %>%
    setnames(wtr)

  data[, (c(vcols, tcols)) := NULL]

  return(result)
}
