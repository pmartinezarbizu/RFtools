#'@title Multivariate Structure and Flexibility Test
#'
#'@description Test the multivariate structure of your data against an empirical null model
#' and the flexibility of your model to correctly classify new observations created by smoothing the classes.
#'
#'@param target Vector or target Column with names of classes.
#'
#'@param predictors Dataframe or matrix with predictor variables.
#'
#'@param nMC Number of smoothed observations returned per class (default = 999) 
#' 
#'
#'
#'@return An object of class MVSF.
#'  Dataframe with results of multivariate test and flexibility test.
#' \item{oob.err}{OOB of trained model} 
#' \item{null.err}{mean OOB of null model} 
#' \item{P.null}{Pseudo P value, probability that trained OOB can happen by random} 
#' \item{q05.null}{0.5 quantile of the ecdf of the null error} 
#' \item{q95.null}{0.95 quantile of the ecdf of the null error} 
#' \item{smooth.err}{prediction error rate of the smoothed classes} 
#' \item{p.smooth}{p value of prop.test, probability that the trained error and the prediction error of smoothed classes are the same} 
#'
#'
#'@details
#' Multivariate Test:
#' The observed OOB is compared against an empirical null model with no multivariate structure.
#' The null model is created by shuffling the lables of the classes nMC times
#' and keeping the OOB of each class under the null model. A Pseudo P-value is returned as P = r/nMC-1,
#' with r = the number of times that the null model OOB >= trained OOB, and nMC = number of Monte Carlo simulations.
#' 
#' Flexibility test:
#' Create nMC of each class observations smoothing the classes and predict using the trained model.
#' Return error rate as n/nMC, with n = total number of missclassifications and nMC = number of Monte Carlo simulations.       
#' prop.test is used for testing the null that trained OOB error = error of the smoothed observations
#'
#' A plot method is available
#'
#'@author Pedro Martinez Arbizu & Sven Rossel
#'
#'@references 
#' Rossel, S. & P. Martinez Arbizu (2018) Automatic specimen identification of Harpacticoids (Crustacea:Copepoda) using Random Forest
#' and MALDI‐TOF mass spectra, including a post hoc test for false positive discovery. Methods in Ecology and Evolution,
#' 9(6):1421-1434.
#' 
#'\url{https://doi.org/10.1111/2041-210X.13000}
#'
#'
#'@examples
#' data(iris)
#' MVSF.iris <- MVSF.test(iris$Species,iris[,1:4],nMC=99)
#' print(MVSF.iris)
#' plot(MVSF.iris)
#'@export MVSF.test
#'@import foreach doParallel parallel randomForest stats
# 
#'@seealso \code{\link{plot.MVSF}} \code{\link{robust.test}}




############################################ 
MVSF.test <- function(target, predictors, nMC = 999) {
    
    # how many corers to be used in parallel?
    doParallel::registerDoParallel(parallel::detectCores())
    
    
    # calculate trained rf OOB error
    rf <- randomForest::randomForest(target ~ ., data = predictors, proximity = TRUE, 
        importance = TRUE, ntree = 1000, sampsize = c(rep(min(table(target)), length(unique(target)))))
    
    # Multivariate structure test shuffle species lables nMC times use
    # paralellization save the error.rate for each species in variable rf.par
    rf.par <- foreach::foreach(i = 1:nMC, .combine = "cbind", .packages = "randomForest") %dopar% 
        randomForest(sample(target, length(target), replace = FALSE) ~ ., data = predictors, 
            proximity = TRUE, importance = TRUE, ntree = 1000, sampsize = c(rep(min(table(target)), 
                length(unique(target)))))$confusion[, length(unique(target)) + 1]
    # close connections
    closeAllConnections()
    
    
    # calculate mean error rate for each species under null model
    mean.par <- apply(rf.par, FUN = mean, MARGIN = 1)
    # lower 95% confidence interval
    q05 <- apply(rf.par, FUN = function(x) quantile(ecdf(x), 0.05), MARGIN = 1)
    # upper 95% confidence interval
    q95 <- apply(rf.par, FUN = function(x) quantile(ecdf(x), 0.95), MARGIN = 1)
    
    
    # calculate Pseudo value P > obs
    p.par <- c()  # probability that the null model error rate is greater than the observed error rate 
    for (i in 1:length(unique(target))) {
        p.par <- c(p.par, length(which(rf.par[i, ] <= rf$confusion[, length(unique(target)) + 
            1][i]))/(nMC + 1))
    }
    
    # Flexibility test create nMC smooth observations per class
    dat.t2 <- smooth.data(target, predictors, nMC)
    
    # predict on dat.t2
    p <- predict(rf, dat.t2)  #predict the new observations (new specimens) using the rf model (observed model)
    res.t2 <- cbind(art = as.double(dat.t2$class), p = as.double(p))  # write results to res.t2
    
    # calculate error rate
    nwrong <- data.frame(art = unique(target), error = rep(0, length(unique(target))))
    
    for (spec in 1:length(unique(target))) {
        nwrong[spec, 2] <- 1 - sum(ifelse(subset(res.t2, res.t2[, 1] == spec)[, 2] == 
            spec, 1, 0))/nMC
    }
    
    ##### calculate the prob that the error rates of fake species and observed error rate
    ##### are the same.  use proportion test (proportion of errors)
    p.sim <- c()
    for (i in 1:length(unique(target))) {
        suc <- sum(subset(res.t2, res.t2[, 1] == spec)[, 2] != spec)
        prop <- as.integer(rf$confusion[, length(unique(target)) + 1][i] * 2000)
        p.sim <- c(p.sim, prop.test(x = c(suc, prop), n = c(nMC, 2000))$p.value)
    }
    
    # write results
    res2 = data.frame(oob.err = rf$confusion[, length(unique(target)) + 1], null.err = mean.par, 
        P.null = p.par, q05.null = q05, q95.null = q95, smooth.err = nwrong[, 2], 
        p.smooth = p.sim)
    class(res2) <- c("MVSF", "data.frame")
    return(res2)
    
}  # END of function
############################################ 


