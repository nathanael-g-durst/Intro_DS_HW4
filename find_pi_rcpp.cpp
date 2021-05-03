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

double inside_unit_circle(NumericVector x){
  double d= x[0]*x[0]+x[1]*x[1];
  return d;
}

double find_pi_cppp (int B = 5000, int seed = 10){
  NumericMatrix point(runif(2*B,-1,1),B,2);
  NumericVector nb_inside = apply_cpp(point,1,inside_unit_circle);
  double pi_hate =sum(nb_inside)/B;
  return 4*pi_hate;
}

//######## [END] C++ Monte Carlo ##########
