#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;


// did_impute ------------------------------------------------------------------

// V_proj = - Z (Z_0' Z_0)^{-1} Z_1
// [[Rcpp::export]]
arma::sp_mat make_V_star(arma::sp_mat Z, arma::sp_mat Z0, arma::sp_mat Z1, arma::sp_mat wtr) {
	return arma::sp_mat(- Z * arma::inv(arma::mat(Z0.t() * Z0)) * (Z1.t() * wtr));
}


