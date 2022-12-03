# Make a sparse_model_matrix for fixest estimate. This only keeps the variables that are not removed from `fixest::feols`
sparse_model_matrix <- function(data, fixest) {
	
  Z <- NULL

	# Coefficients
	if("coefficients" %in% names(fixest)) {
    Z <- Matrix::Matrix(stats::model.matrix(fixest, data = data), sparse = T)
  }

	# Fixed Effects
	if("fixef_id" %in% names(fixest)) {
    # check for interacted FE
		if (any(grepl("[\\^]", fixest$fixef_vars))) {
			interacted <- grep("[\\^]", fixest$fixef_vars, value = T)
			de_interacted <- strsplit(interacted, "\\^")

      # TODO: This is memory inefficient
      # Create interactions with ^
			data[, 
        (sub("\\^", "_", interacted)) := 
          lapply(de_interacted, function(x) {
            do.call(
              function(...) paste(..., sep = "_"), 
              .SD[, x, with = F]
            )
          })
      ]
		} 

    frmla <- paste(
      "~ 0 +", 
      paste("factor(", sub('\\^', '_', fixest$fixef_vars), ")", collapse = " + ")
    ) |> 
      stats::as.formula()

		Z_fixef <- Matrix::sparse.model.matrix(frmla, data = data)

		fe_list <- fixest::fixef(fixest, sorted = F, notes = F)

		if(sum(attr(fe_list, "references")) == length(fe_list) - 1) {
			# regular FE
			select <- lapply(seq_along(fe_list), function(idx) {
        fes = fe_list[[idx]]
        fe_name = names(fe_list)[idx]
        
        # Only keep FEs that are not-omitted
        fe_levels <- names(fes)[abs(fes) > fixest$fixef.tol]

        paste0("factor(", sub('\\^', '_', fe_name), ")", fe_levels)
      }) |>
			  unlist()
        
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
