######## [START] Requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c("Rcpp", "RcppArmadillo")
################################################################################

installedPackages <- installed.packages()

for (packageName in packagesNeeded) {
  packageExists <- is.element(packageName, installedPackages)
  if(packageExists != TRUE){
    install.packages(packageName)
    library(packageName, character.only = TRUE)
    print(paste(packageName, "has been installed and the library is loaded !"))
  } else {
    library(packageName, character.only = TRUE)
    print(paste(packageName, "is installed and the library is loaded !"))
  }
}

rm(installedPackages, packageName, packagesNeeded, packageExists)

##### Add here the external files ##############################################
sourceCpp("find_pi_rcpp.cpp")
################################################################################
#### convertire R code to Rcpp

rcppFunction(' double inside_unit_circle(NumericVector x){
   
 double d= x[0]*x[0]+x[1]*x[1];
   return d;
   }


double find_pi_cpp (int B = 5000, int seed = 10){
  
   NumericMatrix point(runif(2*B,-1,1),B,2);
  
  NumericVector nb_inside = apply_cpp(point,1,inside_unit_circle);
    double pi_hate =sum(nb_inside)/B;
    return 4*pi_hate;
}')

######## [END] Requirements ########

######## [START] R Functions ########

########## [START] inside_unit_circle ##########

inside_unit_circle = function(x){
  d = x[1]^2 + x[2]^2
  (d < 1)
}

########## [END] inside_unit_circle ##########

########## [START] find_pi ##########

find_pi = function(B = 5000, seed = 10){
  # Control seed
  set.seed(seed)
  
  # Simulate B points
  point = matrix(runif(2*B, -1, 1), B, 2)
  
  # Compute the number of points inside unit circle
  nb_inside = apply(point, 1, inside_unit_circle)
  pi_hat = sum(nb_inside)/B
  
  # return estimated value of pi
  return(4*pi_hat)
}

########## [END] find_pi ##########





