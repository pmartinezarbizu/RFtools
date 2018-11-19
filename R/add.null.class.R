#'@title Add a Null Class
#'
#'@description Create nMC observations of a null class.
#' 
#'
#'@param target Column with names of classes.
#'
#'@param predictors The columns containig the predictor variables.
#'
#'@param nMC Number returned observations of the null class (default = 50) 
#' 
#'
#'
#'@return Dataframe with original observatios and nMC observations of a null class.
#' First column named 'class' contains the target classes, predictor names are mantained. 
#'
#'
#'@details
#' A null class has no multivariate structure. It is created by shuffling the 
#' rows across the whole dataset and then shuffling the column labels.
#' By doing so we generate nMC in silico observations for a class containing
#' only random variable values but preserving the variable space.
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
#' iris.null <- add.null.class(iris$Species,iris[,1:4])
#' summary(iris.null)
#'@export add.null.class
# 
#'@seealso \code{\link{smooth.data}}



add.null.class <- function(target, predictors, nMC = 50) {
    
    # number of times to multiply
    n <- min(table(target))
    nt <- as.integer(nMC/n) + 1
    pred2 <- predictors
    target2 <- as.character(target)
    for (i in 1:nt) {
        pred2 <- rbind(pred2, predictors)
        target2 <- c(target2, as.character(target))
    }
    
    res.null <- as.data.frame(apply(pred2, 2, sample)[, sample(ncol(pred2))])[1:nMC, 
        ]
    
    # add name of class
    res.null <- cbind(class = rep("null", nMC), res.null)
    res.null <- rbind(data.frame(class = target, predictors), res.null)
    return(res.null)
}  #END of function
####### 












