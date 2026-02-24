library(INLA)
library(doParallel)

set.seed(2)

# Baseline and shifted distributions
dat <- read.csv('DREAM-HD-aggregate-counts.csv')

# Type I scenario - simulate from same distribution
simulateTrial1 <- function(n) {
  sim_data <- data.frame(
    trt = factor(rep(c(0,1), each=n)),
    outcome = c(sample(dat$outcome, n, replace=T, dat$prop),
                sample(dat$outcome, n, replace=T, dat$prop))
  )
  
  model <- inla(outcome ~ trt, data = sim_data,
                family = "pom", 
                verbose=FALSE, control.compute = list(config=TRUE))
  
  return(c(model$summary.fixed[2,1],
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .9,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .925,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .95,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .975,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .99))
}

# Power scenario - simulate from OR > 1
simulateTrial <- function(n, ind) {
  sim_data <- data.frame(
    trt = factor(rep(c(0,1), each=n)),
    outcome = c(sample(dat$outcome, n, replace=T, dat$prop),
                sample(dat$outcome, n, replace=T, dat[,ind]))
  )
  
  model <- inla(outcome ~ trt, data = sim_data,
                family = "pom", 
                verbose=FALSE, control.compute = list(config=TRUE))
  

  return(c(model$summary.fixed[2,1],
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .9,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .925,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .95,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .975,
           1-inla.pmarginal(0, model$marginals.fixed$trt1) > .99))
}


ns <- c(1110)
# Parallel computing code - change to do to dopar below
# cl <- makeCluster((detectCores()-2))
# registerDoParallel(cl)
for (n in ns) {
  trials0 <- foreach(i = 1:1000, .combine = 'rbind', .packages='INLA') %do% {
    simulateTrial1(n=n)
  }
  write(c(n, mean(trials0[,1]), mean(trials0[,2]), mean(trials0[,3]), 
          mean(trials0[,4]), mean(trials0[,5]), mean(trials0[,6])),
        "result0.txt", append=T)
  for (ind in 3:6) {
    name <- paste0("result", ind, ".txt")
    trials <- foreach(i = 1:1000, .combine = 'rbind', .packages='INLA') %do% {
      simulateTrial(n=n, ind=ind)
    }
    write(c(n, mean(trials[,1]), mean(trials[,2]), mean(trials[,3]), 
            mean(trials[,4]), mean(trials[,5]), mean(trials[,6])), name, append=T)
  }
}
# stopCluster(cl)

# The loop above outputs:
# sample_size trt_effect success_.9 success_.925 success_.95 success_.975 success_.99
# The result0.txt file will provide type I error
# The result5.txt will provide power for odds ratio 1.3

