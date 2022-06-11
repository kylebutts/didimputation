# Make a sparse_model_matrix for fixest estimate. This only keeps the variables that are not removed from `fixest::feols`
sparse_model_matrix <- function(data, fixest) {
	Z <- NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) Z <- Matrix::Matrix(stats::model.matrix(fixest, data = data), sparse = T)

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
		if (any(grepl("[\\^]", fixest$fixef_vars))) {
			# check for interacted FE
			interacted <- grep("[\\^]", fixest$fixef_vars, value = T)
			de_interacted <- stringr::str_split(interacted, stringr::fixed("^"))

			data[, (sub("\\^", "_", interacted)) := purrr::map(de_interacted, ~ do.call(function(...) paste(..., sep = "_"), .SD[, ., with = F]))]
		} 

		frmla <- stats::as.formula(
			paste("~ 0 +", paste(glue::glue("factor({sub('\\\\^', '_', fixest$fixef_vars)})"), collapse = " + "))
		)

		Z_fixef <- Matrix::sparse.model.matrix(frmla, data = data)

		temp <- fixest::fixef(fixest, notes = F)

		select <- purrr::imap(temp, function(fes, fe_name){
			fe_levels <- names(fes)[abs(fes) > fixest$fixef.tol]
			glue::glue("factor({sub('\\\\^', '_', fe_name)}){fe_levels}")
			}) %>%
			unlist

		Z <- cbind(Z, Z_fixef[, select])
	}

	return(Z)
}
