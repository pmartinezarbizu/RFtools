#'@title RF Post-Hoc Test
#'
#'@description Test for false positives in RF prediction.
#'
#'@param rf Object of class randomForest.
#'
#'@param dataset Dataframe to predict using RF model.
#'
#'@return An object of class RF.ph List with results of Post-hoc test .
#'\itemize{
#' \item{ecdf}{ The empirical cumulative distribution fuction of the beta distributed
#'  RF probability of assignment to correct class. } 
#' \item{post.hoc}{ Dataframe with results of post-hoc test. } 
#'  \itemize{
#'      \item{winner}{ Dataframe with results of post-hoc test. } 
#'      \item{p.assign}{ Dataframe with results of post-hoc test. } 
#'      \item{p.post.hoc}{ Dataframe with results of post-hoc test. } 
#'      \item{sig}{ Significance code:  0 ***, 0.001 **, 0.01 *, 0.05 ., > ns  } 
#'  }
#' } 
#'
#'
#'@details
#' RF Post-Hoc test:
#' Take an object created with randomForest and a new dataset to predict. 
#' From the votes matrix of RF object take only correct assigments and calculate for each class
#' the beta distribution of the probability of assignment (POA) to the correct class using fitdistr() from MASS.
#' In case fitdistr fails in estimating the parameters of beta,
#' the non-parametric empirical cumulative distribution function (ecdf) is used instead.
#' The for each observation in newdata, the probability that the POA to winner class (p.assign) belongs to 
#' the beta (or ecdf) of that class in the training data set is calculated (p.post.hoc).
#' Use p.post.hoc to reject the hypothesis that the predicted POA belongs to the trained POA. 
#' A much low p.post.hoc (e.g. <= 0.05) indicates a possible missclassification to that class.
#'
#' Methods plot, print and summary are available
#'
#'@author Pedro Martinez Arbizu & Sven Rossel
#'
#'@examples
#' data(iris)
#' library(randomForest)
#' irSe <- iris[iris$Species == 'setosa',]
#' ir <- iris[iris$Species != 'setosa',]
#' ir$Species <- factor(ir$Species)
#' irNc <- add.null.class(ir$Species,ir[,1:4])
#' rf <- randomForest(class ~ ., data = irNc)
#' table(predict(rf,irSe))
#' posth <- rf.post.hoc(rf,irSe)
#' summary(posth)
#'@export rf.post.hoc summary.RFPH
#'@import randomForest MASS
#'@seealso \code{\link{add.null.class}} \code{\link{smooth.data}} \code{\link{robust.test}}

 
#########################################################
# Post hoc test of RF prediction using
# probability of class assignment
rf.post.hoc <- function(rf,newdata){

# initial checks
if(class(rf)[1] != 'randomForest.formula'){stop('rf should be a randomForest Object\n ')}

# consider only correct rf assigment
#list of correct names
sp.correct.train <- rf$y[rf$y == rf$predicted]
#corresponding votes
votes.train <- rf$votes[which(rf$y == rf$predicted),]

# for each species calculate the empirical beta distribution
#
res.ecdf <- vector('list',length(rf$classes))
for (i2 in 1:length(rf$classes)){
# list of probabilities of correct assigment for class i
prob.ca <- votes.train[sp.correct.train==rf$classes[i2],i2]
# first calculate lower 5% quantile from ecdf
ed <- ecdf(prob.ca)
#delete beta values
beta1 = NULL
beta2 = NULL
# try optimization of empirical beta distribution of the probability of correct assigment
try(beta1 <- fitdistr(prob.ca,'beta',start=list(shape1=1,shape2=1)),silent=TRUE)
# now try to generate 5000 random numbers with the parameters of fitdistr
try(beta2 <- rbeta(5000,beta1$estimate[1],beta1$estimate[2]),silent=TRUE)
try(ed <- ecdf(beta2),silent=TRUE)
# try to calculate the lower 5% quantile
res.ecdf[[i2]] <- list(rf$classes[i2],ed)
}


# predict with rf model  
res.pred <- data.frame(class=predict(rf, newdata), predict(rf, newdata, type='p'))

# probabiloity of assignment
prob.assign <- apply(res.pred[-1],max,MARGIN=1)

# probability that probability of assigment belongs to OOB POA for winner class
p.post.hoc <- c()
for(i in 1:nrow(res.pred)){
n_spec <- which(rf$classes == res.pred$class[i])
df <- res.ecdf[[n_spec]][[2]]
p.post.hoc <- c(p.post.hoc, df(prob.assign[i]))
}



# significance symbols
sig = c(rep('ns',length(p.post.hoc)))
sig[p.post.hoc <= 0.05] <-'.'
sig[p.post.hoc <= 0.01] <-'*'
sig[p.post.hoc <= 0.001] <-'**'
sig[p.post.hoc <= 0.0001] <-'***'


res <- list(ecdf=res.ecdf,post.hoc=data.frame(winner=res.pred$class,p.assign=prob.assign,p.post.hoc=round(p.post.hoc,4),sig=sig))
class(res) <- c('RFPH','list')
return(res)

}
## end of function


###Method summary
summary.RFPH = function(obj) {
 cat('Results of RF post-hoc test:\n')
 cat('\n')
 print(obj$post.hoc)
 cat('Signif. codes: ns > 0.05, . <= 0.05, * <=0.01, ** <=0.001, *** <=0.0001\n')
 cat('\n')
 cat('\n')
 cat('Table of counts by class:\n')
  print(table(obj$post.hoc[,c(1,4)]))
}
## end of method summary





