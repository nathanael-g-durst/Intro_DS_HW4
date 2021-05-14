######## [START] Requirements ########

##### Add here the packages needed #############################################
packagesNeeded <- c("Rcpp", "RcppArmadillo", "microbenchmark", "multcomp",
                    "ggplot2")
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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

rm(all_monte_carlo_size, l_monte_carlo_size, monte_carlo_size,
   monte_carlo_size_i, pi_cpp, pi_r, res_pi, seed_i, simu_i)

########## [END] Check same results ##########

########## [START] Graph ##########

graph <- function (uptopower, seed) {
  
  uptopower <- uptopower+1
  
  estim <- matrix(NA, uptopower, 3) 
  
  for (i in 1:uptopower) {
    r <- find_pi(10^(i-1), seed)
    cpp <- find_pi_cpp(10^(i-1), seed)
    power <- paste("1e+", i-1, sep="")
    
    results <- c(power, r, cpp)
    
    estim[i,] <- results
    name <- paste("1e+", i-1, sep="")
  }
  
  colnames <- c("Power", "R", "C++")
  
  colnames(estim) <- colnames

  return(as.data.frame(estim))
  
}

x <- graph(6,127)

par(pty="s")

plot(x = x$Power,
     y = x$R,
     log = "x",
     type = "b",
     pch = 21,
     col = "dark orange",
     cex = 2,
     bg = "white",
     lwd = 2,
     ylim = c(3.1,4.0),
     main = "Comparing results from R and Rcpp implementation",
     xlab = "Number of Monte-Carlo simulations",
     ylab = expression(hat(pi)))

grid()

lines(x = x$Power,
      y = x$`C++`,
      type = "b",
      col = "dark blue",
      pch = 16,
      cex = 1,
      bg = "dark blue",
      lwd = 1)

legend("topright",
       legend = c("R", "C++"),
       lty = c(1,1),
       pch = c(21, 16),
       lwd = 2,
       inset = 0.05,
       col = c("dark orange", "dark blue"),
       bg = c("white","dark blue"),)

rm(x)

########## [END] Graph ##########

######## [END] Check same results ########

################################################################################

######## [START] Benchmark ########

########## [START] Benchmark ##########

bench <- function (nbmc = 10^5, nbsim = 100, seed = 123) {
  x <- microbenchmark(find_pi(nbmc, seed), find_pi_cpp(nbmc, seed), times = nbsim, unit = "s")
  return (summary(x))
}

benchmark <- bench()

print(benchmark)

########## [END] Benchmark ##########

########## [START] Graph ##########

mat <- matrix(NA, 5, 3)

for (i in 1:5) {
  benchm <- bench(10^i)
  r <- benchm$mean[1]
  cpp <- benchm$mean[2]
  power <- paste("1e+", i, sep="")
  
  results <- c(power, r, cpp)
  
  mat[i,] <- results
  
}

colnames <- c("Power", "R", "C++")

colnames(mat) <- colnames

mat <- as.data.frame(mat)

rm(benchm, r, cpp, power, i, colnames, results)

plot(x = mat$Power,
     y = mat$R,
     log = "x",
     type = "b",
     pch = 21,
     col = "dark orange",
     cex = 1,
     bg = "dark orange",
     lwd = 1,
     main = "Comparing execution time from R and Rcpp implementation",
     xlab = "Number of Monte-Carlo simulations",
     ylab = "Mean execution time (seconds)")

grid()

lines(x = mat$Power,
      y = mat$`C++`,
      type = "b",
      col = "dark blue",
      pch = 21,
      cex = 1,
      bg = "dark blue",
      lwd = 1)

legend("topleft",
       legend = c("R", "C++"),
       col = c("dark orange", "dark blue"),
       lty = 1,
       pch = 16,
       lwd = 2,
       inset = 0.05)

########## [END] Graph ##########

######## [END] Benchmark ########

