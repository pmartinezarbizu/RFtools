#'@title smooth classes
#'
#'@description Create a new dataset with smoothed classes and nMC observations per class.
#' 
#'
#'@param target Vector or target Column with names of classes.
#'
#'@param predictors Dataframe or matrix with predictor variables.
#'
#'@param nMC Number of smoothed observations returned per class (default = 99) 
#' 
#'
#'@return Dataframe with nMC smoothed observations.
#' First column named 'class' contains the target classes, predictor names are mantained. 
#'
#'
#'@details
#' For each class, the function will shuffle the rows of each predictor separately
#' and extract one row for each class. This is repeated nMC times.
#' By doing so we generate nMC in silico observations for each class,
#' but mantaining the range (observed variability) for each predictor.
#' 
#'@author Pedro Martinez Arbizu & Sven Rossel
#'
#'@examples
#' data(iris)
#' iris.sm <- smooth.data(iris$Species,iris[,1:4])
#' summary(iris.sm)
#'@export smooth.data
# 
#'@seealso \code{\link{add.null.class}}



smooth.data <- function(target, predictors, nMC = 999) {
    
    # number of times to multiply
    n <- min(table(target))
    nt <- as.integer(nMC/n) + 1
    # create matrix of predictors
    pred2 <- predictors
    target2 <- as.character(target)
    for (i in 1:nt) {
        pred2 <- rbind(pred2, predictors)
        target2 <- c(target2, as.character(target))
    }
    # smoothing the classes
    class <- c()
    m <- c()  #store shuffled matrix here
    for (i in unique(target2)) {
        # for each species separatelly
        class <- c(class, rep(i, nMC))
        m <- rbind(m, apply(pred2[target2 == i, ], 2, sample)[1:nMC, ])
    }
    
    return(data.frame(class, m))
    
}  #END of function
