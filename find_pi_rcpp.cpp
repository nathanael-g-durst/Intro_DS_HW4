#include <Rcpp.h>
using namespace Rcpp;

//######## [START] C++ Monte Carlo ##########

// [[Rcpp::export]]
double find_pi_cpp(const int B, int seed) {
  setSeed(seed);
  NumericVector x = runif(B);
  NumericVector y = runif(B);
  NumericVector d = sqrt(x*x + y*y);
  return 4.0 * sum(d < 1.0) / B;
}

//######## [END] C++ Monte Carlo ##########
