#'@title RF Post-Hoc Test
#'
#'@description Test for false positives in RF prediction.
#'
#'@param rf Object of class randomForest.
#'
#'@param newdata Dataframe to predict using RF model.
#'
#'@return An object of class RF.ph List with results of Post-Hoc test .
#'\itemize{
#' \item{ecdf}{ The empirical cumulative distribution fuction of the beta distributed
#'  RF probability of assignment to correct class. }
#' \item{post.hoc}{ Dataframe with results of post-hoc test. }
#'  \itemize{
#'      \item{winner}{ The predicted class. }
#'      \item{POA}{ Probability of assigment to the winner class. }
#'      \item{p.post.hoc}{ Probability that POA belongs to POA distribution of this class in the trained model. }
#'      \item{sig}{ Significance code:  0 ***, 0.001 **, 0.01 *, 0.05 ., > ns  }
#'  }
#' }
#'
#'
#'@details
#' RF Post-Hoc test:
#' Take an object created with randomForest and a new dataset to predict.
#' From the votes matrix of RF object take only correct assigments and calculate for each predicted class
#' the beta distribution of the probability of assignment (POA) to the correct class using fitdistr() from MASS.
#' In case fitdistr fails in estimating the parameters of beta,
#' the non-parametric empirical cumulative distribution function (ecdf) is used instead.
#' The for each observation in newdata, the probability that the POA to winner class (p.assign) belongs to
#' the beta (or ecdf) of that class in the training data set is calculated (p.post.hoc).
#' Use p.post.hoc to reject the hypothesis that the predicted POA belongs to the trained POA.
#' A low p.post.hoc probability (e.g. <= 0.05) indicates a possible missclassification of that observation.
#'
#' Methods plot, print and summary are available
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
#'@examples
#'#example with maldi data
#' data(maldi)
#' library(randomForest)
#' unique(maldi$species)
#' maldi_train <- maldi[maldi$species != 'Cletodes limicola',]
#' maldi_test <- maldi[maldi$species == 'Cletodes limicola',]
#'#exclude Cletodes limicola from factors
#' maldi_train$species <- factor(maldi_train$species)
#' rf <- randomForest(species ~ ., data = maldi_train[-1])
#' ph <- rf.post.hoc(rf,maldi_test)
#' plot(ph)
#' plot(ph,'Tachidius discipes')
#'@export rf.post.hoc summary.RFPH
#'@import randomForest MASS stats
#'@seealso \code{\link{add.null.class}} \code{\link{smooth.data}} \code{\link{robust.test}} \code{\link{plot.RFPH}}


######################################################### Post hoc test of RF prediction using probability of class assignment
rf.post.hoc <- function(rf, newdata) {

    # initial checks
    if (class(rf)[1] != "randomForest.formula") {
        stop("rf should be a randomForest Object\n ")
    }
    # predict with rf model
    res.pred <- data.frame(class = predict(rf, newdata), predict(rf, newdata, type = "p"))

    # winner probability of assignment
    prob.assign <- apply(res.pred[-1], max, MARGIN = 1)

    # consider only predicted classes
    pred.class.votes <- data.frame(
        rf.pred = rf$y[which(rf$y %in% unique(res.pred$class))],
        votes.pred = rf$votes[which(rf$y %in% unique(res.pred$class)),]
    )
    colnames(pred.class.votes) <- c('class',colnames(rf$votes))

    # winner class consider correctly predicted
    winner <- colnames(pred.class.votes[,-1])[apply(pred.class.votes[,-1],1,which.max)]

    #consider only corrently predicted
    pred.class <- pred.class.votes[pred.class.votes$class == winner,]

    # for each species calculate the empirical beta distribution
    res.ecdf <- vector("list", length(unique(pred.class.votes$class)))
    for (i2 in unique(pred.class.votes$class)) {
        # list of probabilities of correct assigment for class i
        prob.ca <- pred.class.votes[pred.class.votes == i2, i2]
		
		# laplace correction
		prob.ca[prob.ca == 0] <- 1e-3
		prob.ca[prob.ca == 1] <- 1-(1e-9)

        # first calculate lower 5% quantile from ecdf
        ed <- ecdf(prob.ca)
        # delete beta values
        beta1 = NULL
        beta2 = NULL
        # try optimization of empirical beta distribution of the probability of correct
        # assigment
        try(beta1 <- MASS::fitdistr(prob.ca, "beta", start = list(shape1 = 1, shape2 = 1),lower=c(0,0)),
            silent = TRUE)
        # now try to generate 5000 random numbers with the parameters of fitdistr
        try(beta2 <- rbeta(5000, beta1$estimate[1], beta1$estimate[2]), silent = TRUE)
        if(!is.null(beta2)){ed <- ecdf(beta2)}
        # write to list
        res.ecdf[[i2]] <- list(rf$classes[i2], ed)
    }



    # probability that probability of assigment belongs to OOB POA for winner class
    p.post.hoc <- c()
    for (i in 1:nrow(res.pred)) {
        df <- res.ecdf[[as.character(res.pred$class[i])]][[2]]
        p.post.hoc <- c(p.post.hoc, df(prob.assign[i]))
    }



    # significance symbols
    sig = c(rep("ns", length(p.post.hoc)))
    sig[p.post.hoc <= 0.05] <- "."
    sig[p.post.hoc <= 0.01] <- "*"
    sig[p.post.hoc <= 0.001] <- "**"
    sig[p.post.hoc <= 1e-04] <- "***"


    res <- list(ecdf = res.ecdf, post.hoc = data.frame(winner = res.pred$class, POA = prob.assign,
        p.post.hoc = round(p.post.hoc, 4), sig = sig))
    class(res) <- c("RFPH", "list")
    return(res)

}
## end of function


### Method summary
summary.RFPH = function(object, ...) {
    cat("Results of RF post-hoc test:\n")
    cat("\n")
    print(object$post.hoc, ...)
    cat("Signif. codes: ns > 0.05, . <= 0.05, * <=0.01, ** <=0.001, *** <=0.0001\n")
    cat("\n")
    cat("\n")
    cat("Table of counts by class:\n")
    print(table(object$post.hoc[, c(1, 4)]), ...)
}
## end of method summary





