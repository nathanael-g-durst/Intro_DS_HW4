#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// set seed
// [[Rcpp::export]]
void set_seed(double seed) {
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed_r = base_env["set.seed"];
  set_seed_r(std::floor(std::fabs(seed)));
}

// monte carlo
// [[Rcpp::export]]
double find_pi_cpp(const int B, double seed) {
  set_seed(seed);
  Rcpp::NumericVector x;
  Rcpp::NumericVector y;
  Rcpp::NumericVector d;
  x = Rcpp::runif(B, -1, 1);
  y = Rcpp::runif(B, -1, 1);
  d = Rcpp::sqrt(x*x + y*y);
  return 4.0 * sum(d < 1.0) / B;
}