% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robust.test.R
\name{robust.test}
\alias{robust.test}
\title{RF Robustness Test}
\usage{
robust.test(target, predictors, nMC = 999, q = 0.05)
}
\arguments{
\item{target}{Vector or target Column with names of classes.}

\item{predictors}{Dataframe or matrix with predictor variables.}

\item{nMC}{Number of smoothed observations returned per class (default = 999)}

\item{q}{Number, quantile to use for the post-hoc test, default 0.05}
}
\value{
An object of class RBT. List with results of RF robustness test.
\item{assignment}{Table with the class assigments per model} 
\item{false.pos}{Table with the false positives, after post-hoc test, per model} 
\item{false.pos.rate}{Table with the false positive rate, after post-hoc test, per model}
}
\description{
Test how prone is your RF model to misclassification of classes,
that were not included in the training dataset after applying the post-hoc test.
}
\details{
RF Robustness Test:
Models are calculated leaving one class out. Observations of the class that is excluded from the training dataset
are used to predict with the trained model. It is evident that these observations cannot belong to any of the target classes.
After prediction, the post-hoc test for false positive discovery is applied to the results. A residual false positive rate is calculated.
Row lables display the class that was excluded from the model. Column names display the predicted class.
}
\examples{
data(iris)
rbt.iris <- robust.test(iris$Species,iris[,1:4])
print(rbt.iris)

}
\references{
Rossel, S. & P. Martinez Arbizu (2018) Automatic specimen identification of Harpacticoids (Crustacea:Copepoda) using Random Forest
and MALDI‐TOF mass spectra, including a post hoc test for false positive discovery. Methods in Ecology and Evolution,
9(6):1421-1434.

\url{https://doi.org/10.1111/2041-210X.13000}
}
\seealso{
\code{\link{add.null.class}} \code{\link{smooth.data}}
}
\author{
Pedro Martinez Arbizu
}
