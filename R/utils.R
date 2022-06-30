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

		frmlas <- purrr::map(
			paste("~ 0 +", glue::glue("factor({sub('\\\\^', '_', fixest$fixef_vars)})")),
			stats::as.formula
		)

		Z_fixef <- do.call(cbind,
			purrr::map(frmlas,
						~ Matrix::sparse.model.matrix(., data = data)))


		fe_list <- fixest::fixef(fixest, sorted = F, notes = F)

		if(sum(attr(fe_list, "references")) == length(fe_list) - 1) {
			# regular FE
			select <- purrr::imap(fe_list,
			function(fes, fe_name){
				fe_levels <- names(fes)[abs(fes) > fixest$fixef.tol]
				glue::glue("factor({sub('\\\\^', '_', fe_name)}){fe_levels}")
				}) %>%
			unlist
		} else {
			# not regular
			qrZ <- Matrix::qr(Z_fixef)
			diagR <- Matrix::diag(qrZ@R)
			tol <- max(qrZ@Dim) * .Machine$double.eps / 2
			keepcols <- which(diagR > tol * max(diagR))
			select <- qrZ@q[keepcols] + 1
		}



		Z <- cbind(Z, Z_fixef[, select])
	}

	return(Z)
}
