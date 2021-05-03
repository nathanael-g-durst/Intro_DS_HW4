######## [START] Requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c("Rcpp", "RcppArmadillo", "microbenchmark")
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

######## [END] Requirements ########

################################################################################

######## [START] R Monte Carlo ########

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

######## [END] R Monte Carlo ########

################################################################################

######## [START] Check same results ########

########## [START] Check same results ##########

all_monte_carlo_size = 10^(0:6)
l_monte_carlo_size = length(all_monte_carlo_size)
for(monte_carlo_size_i in 1:l_monte_carlo_size){
  monte_carlo_size = all_monte_carlo_size[monte_carlo_size_i]
  # create res pi
  res_pi = vector("logical", length = 10)
  for(simu_i in seq(10)){
    seed_i = 123 + simu_i
    pi_r = find_pi(monte_carlo_size, seed=seed_i)
    pi_cpp = find_pi_cpp(monte_carlo_size, seed=seed_i)
    # test equality
    res_pi[simu_i] = all.equal(pi_r, pi_cpp)
  }
  # check if all elements are true
  print(all(res_pi))
}

########## [END] Check same results ##########

########## [START] Graph ##########

########## [END] Graph ##########

######## [END] Check same results ########

################################################################################

######## [START] Benchmark ########

########## [START] Benchmark ##########

mbmark = microbenchmark::microbenchmark(find_pi(B = 10^5, seed = 10), find_pi_cpp(B = 10^5, seed = 10))

mbdf = summary(mbmark)

meanpir = mbdf$mean[1]
meanpicpp = mbdf$mean[2]

# For the last graph
#summary(mb)
#meanpir
#meanpicpp

########## [END] Benchmark ##########

########## [START] Graph ##########



########## [END] Graph ##########

######## [END] Benchmark ########
