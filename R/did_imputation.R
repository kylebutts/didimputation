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
#' @import tidyverse
#'
#' @param data A `data.frame`
#' @param yname String. Variable name for outcome. Use fixest c() syntax for multiple lhs.
#' @param idname String. Variable name for unique unit id.
#' @param gname String. Variable name for unit-specific date of treatment
#'   (never-treated should be zero or `NA`).
#' @param tname String. Variable name for calendar period.
#' @param first_stage Formula for Y(0).
#'   Formula following \code{\link[fixest:feols]{fixest::feols}}.
#'   Fixed effects specified after "`|`".
#'   If not specified, then just unit and time fixed effects will be used.
#' @param weights String. Variable name for estimation weights of observations.
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
#' did_imputation(data = castle, yname = "l_homicide", gname = "effyear",
#'                first_stage = ~ 0 | sid + year,
#'                tname = "year", idname = "sid")
#' ```
#'
did_imputation = function(data, yname, gname, tname, idname, first_stage = NULL,
                          weights = NULL, wtr = NULL, horizon = NULL,
                          pretrends = NULL, cluster_var = NULL){


    # Set-up Parameters ------------------------------------------------------------

    data %>% setDT

    # Extract vars from formula
    if (is.null(first_stage)) {
        first_stage <- glue::glue("0 | {idname} + {tname}")
    } else if(inherits(first_stage, "formula")) {
        first_stage <- as.character(first_stage)[[2]]
    }

    # Treatment indicator
    data[, zz000treat := 1 * (get(tname) >= get(gname)) * (get(gname) > 0)]
    
    # if g is NA
    data[is.na(zz000treat), zz000treat := 0]

    # Create event time
    data[, zz000event_time := if_else(is.na(get(gname)) | get(gname) == 0 | get(gname) == Inf,
        -Inf,
        as.numeric(get(tname) - get(gname)))]

    # Get list of event_time
    event_time <- unique(data[, zz000event_time])
    event_time <- event_time[is.finite(event_time)]

    # horizon/allhorizon options
    if (is.null(wtr)) {

        # event-study
        if (!is.null(horizon)) {
            # create event time weights

            # allhorizons
            if (all(horizon == TRUE)) horizon <- event_time

            # Create wtr of horizons
            wtr <- paste0("zz000wtr", event_time[event_time >= 0])
			walk2(event_time[event_time >= 0], wtr,
				function(e, v) data[, (v) := if_else(is.na(zz000event_time), 0, 1 * (zz000event_time == e))])

	} else {
            wtr <- "zz000wtrtreat"
            data[, (wtr) := 1*(zz000treat==1)]
        }
    }

    # Weights specified or not
    if(is.null(weights)) {
		data[, zz000weight := 1]
    } else {
		data[, zz000weight := get(weights)]
    }

	data[, (wtr) := map(.SD, ~ . * zz000weight), .SDcols = wtr]  # Multiply treatment weights * weights vector
	data[is.na(zz000weight), (wtr) := 0]
	data[, (wtr) := map(.SD, ~ . / sum(.)), .SDcols = wtr] # Normalize

    # First Stage estimate ---------------------------------------------------------

    # First stage among untreated
    formula <- stats::as.formula(glue::glue("{yname} ~ {first_stage}"))

    # Estimate Y(0) using untreated observations
    first_stage_est <- fixest::feols(formula, se = "standard",
                                    data[zz000treat == 0, ],
                                    weights = ~ zz000weight,
                                    warn = FALSE, notes = FALSE)

    # Residualize outcome variable(s)
	yvars <- sub("^c[(]", "", yname)
	yvars <- sub("[)]$", "", yvars)
	yvars <- str_split(yvars, "\\s*,\\s*")[[1]]

	if (length(yvars) == 1) {
		data[, zz000adj := get(yname) - stats::predict(first_stage_est, newdata = data)]
	} else {
		data[, (paste(zz000adj, yvars, sep = "_")) := imap(.SD, ~ . - stats::predict(first_stage_est[[.y]], newdata = data))]
	}
    

    # Point estimate for wtr
    est = c()
    for(w in wtr) {
        # \sum w_{it} * \tau_{it}
        est = c(est,
                sum(
                    data[data$zz000treat == 1,][[w]] * data[data$zz000treat == 1,][["zz000adj"]]
                ))
    }


    # Standard Errors --------------------------------------------------------------

    # Create Zs
    Z = sparse_model_matrix(data, first_stage_est)

    # Equation (6) of Borusyak et. al. 2021
    # - Z (Z_0' Z_0)^{-1} Z_1' wtr_1
    v_star = make_V_star(
        (Z * weights_vector),
        (Z * weights_vector)[data$zz000treat == 0, ],
        (Z * weights_vector)[data$zz000treat == 1, ],
        Matrix::Matrix(as.matrix(data[data$zz000treat == 1, wtr]), sparse = TRUE)
    )

    # fix v_it^* = w for treated observations
    v_star[data$zz000treat == 1, ] = as.matrix(data[data$zz000treat == 1, wtr])

    se = c()
    for(i in 1:length(wtr)) {

        # Calculate v_it^* = - Z (Z_0' Z_0)^{-1} Z_1' * w_1
        data$zz000v = v_star[, i]

        # Equation (10) of Borusyak et. al. 2021
        # Calculate tau_it - \bar{\tau}_{et}

        # \bar{\tau}_{et}
        # split
        split_et <- split(data, list(data[[gname]], data$zz000event_time), drop = T)
        # apply
        results <- lapply(split_et, function(x) {
            temp = ifelse(
                x$zz000treat == 1,
                sum(x$zz000v^2 * x$zz000adj)/sum(x$zz000v^2) * x$zz000treat,
                0
            )

            temp = ifelse(is.nan(temp), 0, temp)

            x$zz000tau_et = temp

            return(x)
        })
        # combine
        data = do.call("rbind", results)

        # Recenter tau by \bar{\tau}_{et}
        data$zz000tau_centered = data$zz000adj - data$zz000tau_et

        # If no cluster_var, then use idname
        if(is.null(cluster_var)) cluster_var = idname

        # Equation (8)
        # Calculate variance of estimate
        split_id <- split(data, data[[cluster_var]], drop = T)
        results <- lapply(split_id, function(x) {
            temp = sum(x$zz000v * x$zz000tau_centered)^2
            return(temp)
        })

        variance = sum(unlist(results))

        se = c(se, sqrt(variance))
    }


    # Pre-event Estimates ----------------------------------------------------------

    if(!is.null(pretrends)) {
        if(all(pretrends == TRUE)) {
            pre_formula <- stats::as.formula(glue::glue("{yname} ~ i(zz000event_time) + {first_stage}"))
        } else {
            if(all(pretrends %in% event_time)) {
                pre_formula <- stats::as.formula(
                    glue::glue("{yname} ~ i(zz000event_time, keep = c({paste(pretrends, collapse = ', ')}))  + {first_stage}")
                )
            } else {
                stop(glue::glue("Pretrends not found in event_time. Event_time has values {event_time}"))
            }
        }

        pre_est <- fixest::feols(pre_formula, data[data$zz000treat == 0, ], weights = weights_vector[data$zz000treat == 0], warn=FALSE, notes=FALSE)
    }


    # Create dataframe of results in tidy format -----------------------------------

    # Fix term for horizon option
    wtr = stringr::str_replace(wtr, "zz000wtr", "")

    out <- dplyr::tibble(
        term      = wtr,
        estimate  = est,
        std.error = se,
        conf.low  = est - 1.96 * se,
        conf.high = est + 1.96 * se
    )
    out$term = as.character(out$term)

    if(!is.null(pretrends)) {
        pre_out <- broom::tidy(pre_est)

        pre_out$term = stringr::str_remove(pre_out$term, "zz000event_time::")
        pre_out$term = as.character(pre_out$term)

        pre_out$conf.low = pre_out$estimate - 1.96 * pre_out$std.error
        pre_out$conf.high = pre_out$estimate + 1.96 * pre_out$std.error

        pre_out = pre_out[,c("term", "estimate", "std.error", "conf.low", "conf.high")]

        out = dplyr::bind_rows(pre_out, out)
    }

    return(out)
}
