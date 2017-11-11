#'@title RF Robustness Test
#'
#'@description Test how prone is your RF model to misclassification of classes,
#' that were not included in the training dataset after applying the post-hoc test. 
#'
#'@param target Vector or target Column with names of classes.
#'
#'@param predictors Dataframe or matrix with predictor variables.
#'
#'@param nMC Number of smoothed observations returned per class (default = 999) 
#' 
#'@param q Number, quantile to use for the post-hoc test, default 0.05
#'
#'@return An object of class MVSF. Dataframe with results of multivariate test and flexibility test.
#' \item{assignment}{Table with the class assigments per model} 
#' \item{false.pos}{Table with the false positives, after post-hoc test, per model} 
#' \item{false.pos.rate}{Table with the false positive rate, after post-hoc test, per model} 
#'
#'
#'@details
#' RF Robustness Test:
#' Models are calculated leaving one class out. Observations of the class that is excluded from the training dataset
#' are used to predict with the trained model. It is evident that these observations cannot belong to any of the target classes.
#' After prediction, the post-hoc test for false positive discovery is applied to the results. A residual false positive rate is calculated.
#' Row lables display the class that was excluded from the model. Column names display the predicted class.
#'
#' A plot method is available
#'
#'@author Pedro Martinez Arbizu
#'
#'@examples
#' data(iris)
#' rbt.iris <- robust.test(iris$Species,iris[,1:4])
#' print(rbt.iris)
#' plot(rbt.iris)
#'@export robust.test
#'@import randomForest MASS
#'@seealso \code{\link{add.null.class}} \code{\link{smooth.data}}




# ROBUSTNESS TEST
robust.test <- function(target, predictors, nMC = 999, q = 0.05) {
    
    tot.assign <- c()
    false.pos <- c()
    false.pos.rate <- c()
    
    for (c in 1:length(unique(target))) {
        # divide the dataset in a train (excluding 1 species) and a test dataset (with
        # the excluded species)
        dattrain <- data.frame(class = target, predictors)[which(target != unique(target)[c]), 
            ]
        dattest <- data.frame(class = target, predictors)[which(target == unique(target)[c]), 
            ]
        # rearrange factors
        dattrain$class <- factor(dattrain$class)
        dattest$class <- factor(dattest$class)
        
        
        
        # calculate RF on training dataset
        rf <- randomForest::randomForest(class ~ ., data = dattrain, proximity = TRUE, 
            importance = TRUE, ntree = 1000, sampsize = c(rep(min(table(dattrain$class)), 
                length(unique(dattrain$class)))))
        
        # create 999 fake specimens from the test dataset use function from test 2
        dattest2 <- smooth.data(dattest$class, dattest[, 2:ncol(dattest)], nMC)
        
        
        # predict with rf model
        res.test5 <- data.frame(class = predict(rf, dattest2), predict(rf, dattest2, 
            type = "p"))
        
        # consider only correct rf assigment list of correct names
        art.correct.train <- dattrain$class[dattrain$class == rf$predicted]
        # corresponding votes
        rfv2.train <- rf$votes[which(dattrain$class == rf$predicted), ]
        
        # for each species calculate the lower 5% quantile of the empirical beta
        # distribution : save in variable q5 From test3
        q5 <- c()
        for (i2 in 1:length(levels(dattrain$class))) {
            # list of probabilities of correct assigment for class i
            prob.ca <- rfv2.train[art.correct.train == unique(dattrain$class)[i2], 
                i2]
            # first calculate lower 5% quantile from ecdf
            oq5 <- quantile(ecdf(prob.ca), q)
            # delete beta values
            beta1 = NULL
            beta2 = NULL
            # try optimization of empirical beta distribution of the probability of correct
            # assigment
            try(beta1 <- MASS::fitdistr(prob.ca, "beta", start = list(shape1 = 1, 
                shape2 = 1)), silent = TRUE)
            # now try to generate 5000 random numbers with the parameters of fitdistr
            try(beta2 <- rbeta(5000, beta1$estimate[1], beta1$estimate[2]), silent = TRUE)
            try(oq5 <- quantile(ecdf(beta2), q), silent = TRUE)
            # try to calculate the lower 5% quantile
            q5 <- c(q5, oq5)
        }
        
        
        # extract from res.test3 the assigment for each species and calculate false
        # positives 'fa' total assigments
        ta <- c()
        # assigments with probability greater than q5 = false positives 'fa'
        fa <- c()
        
        for (i3 in 1:length(levels(dattrain$class))) {
            op <- res.test5[which(res.test5$class == levels(dattrain$class)[i3]), 
                i3 + 1]
            ta <- c(ta, length(op))
            fa <- c(fa, length(op[op > q5[i3]]))
        }
        
        ta <- append(ta, NA, c - 1)
        fa <- append(fa, NA, c - 1)
        fpr <- fa/ta
        
        tot.assign <- rbind(tot.assign, ta)
        false.pos <- rbind(false.pos, fa)
        false.pos.rate <- rbind(false.pos.rate, fpr)
    }
    
    colnames(tot.assign) <- unique(target)
    rownames(tot.assign) <- unique(target)
    colnames(false.pos) <- unique(target)
    rownames(false.pos) <- unique(target)
    colnames(false.pos.rate) <- unique(target)
    rownames(false.pos.rate) <- unique(target)
    false.pos.rate[is.na(false.pos.rate)] <- 0
    
    # add error columns
    tot.assign <- cbind(tot.assign, assign.error = apply(tot.assign[, which(colnames(tot.assign) != 
        "null")], sum, MARGIN = 1, na.rm = T)/nMC)
    false.pos <- cbind(false.pos, res.error = apply(false.pos[, which(colnames(false.pos) != 
        "null")], sum, MARGIN = 1, na.rm = T)/nMC)
    
    rob.res <- list(assignment = data.frame(tot.assign), false.pos = data.frame(false.pos), 
        false.pos.rate = data.frame(false.pos.rate), nMC = nMC)
    
    class(rob.res) <- c("RBT", "list")
    
    rob.res
    
}  #END of function
######################################################### 
