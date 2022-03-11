#define ARMA_64BIT_WORD  1
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;


// did_impute ------------------------------------------------------------------

// V_proj = - Z (Z_0' Z_0)^{-1} Z_1
// [[Rcpp::export]]
arma::mat make_V_star(arma::sp_mat Z, arma::sp_mat Z0, arma::sp_mat Z1, arma::sp_mat wtr) {
	return arma::mat(- Z * arma::inv(arma::mat(Z0.t() * Z0)) * (Z1.t() * wtr));
}


