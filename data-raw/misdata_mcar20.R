## code to prepare `misdata_mcar20`

#--------------helper functions (may eventually be moved to main package) -------#

# The following functions mod1.gen and mcar.mod1 are from the original
# Savalei & Rhemtulla (2017) code (used to be available in 'SavaleiRhemtullaFunctions.R' on OSF)


# generates nreps number of complete datasets of size N on 27 variables (see paper for model)
mod1.gen <- function(N, nreps) {
  #---- population covariance matrix
  Psi <- diag(rep(c(.91, .84, .75), 3 * 3)) # residual variances
  l <- c(.3, .4, .5)
  i <- diag(1, 9)
  L <- kronecker(i, l) # loadings matrix for first order factors

  # correlation matrix of first order factors (needs to be structured in terms of second order factors)
  l2 <- c(.6, .7, .8)
  i2 <- diag(1, 3)
  L2 <- kronecker(i2, l2)
  Phi2 <- diag(3)
  Phi2[1, 2] <- Phi2[2, 1] <- .6
  Phi2[2, 3] <- Phi2[3, 2] <- .6
  Phi2[1, 3] <- Phi2[3, 1] <- (.6 * .6)
  Phi <- L2 %*% Phi2 %*% t(L2)
  diag(Phi) <- 1

  Sigma <- L %*% Phi %*% t(L) + Psi # population covariance matrix for Figure 1
  colnames(Sigma) <- paste("Y", 1:27, sep = "")
  rownames(Sigma) <- paste("Y", 1:27, sep = "")
  mu <- rep(0, 27)

  datasets <- list()
  seedlist <- sample(1:9999999, nreps) # generate vector of random number seeds
  for (i in 1:nreps) {
    set.seed(seedlist[i])
    datasets[[i]] <- mvrnorm(Sigma = Sigma, n = N, mu = mu)
  }
  return(list(seedlist, datasets))
}


# generates an incomplete dataset from simdata list with percent missing on
# selected variables
mcar.mod1 <- function(simdata, percent) {
  p <- list(c(1, 5, 9), c(10, 11), c(14, 15, 16, 18), c(20, 21), c(22, 24), c(25, 26))
  for (j in seq_along(simdata)) {
    data <- simdata[[j]]
    N <- nrow(data)
    for (i in seq_along(p)) {
      patt <- p[[i]]
      repeat {
        # pick random row:
        row <- sample(1:N, 1)
        # set p1 to missing
        data[row, patt] <- NA
        if (sum(is.na(data[, patt[1]])) == N * percent) break
      }
    }
    simdata[[j]] <- data
  }
  return(simdata)
}


# now the code (the dataset will be slightly different due to random seed, which I did not record):
data <- mod1.gen(N = 200, nreps = 1)[[2]]
misdata_mcar20 <- mcar.mod1(data, percent = .20)[[1]]

usethis::use_data(misdata_mcar20, overwrite = TRUE)
