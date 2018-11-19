#'@title Plot Post-hoc test
#'
#'@description Plots the results of rf.post.hoc.
#'
#'@param x Object of class RFPH.
#'
#'@param Species Name of class to be plotted or 'all' for all classes
#'
#'@param q Significance level.
#'
#'@return Plot
#'
#'@details
#' If name of class to be plotted is specify, the plot returns
#' the density function of the probability of assignment 'POA' to the correct class
#' in the trainig data and the POA of the predicted data as circles symbols.
#' Circle is filled 'red' if not significantly different at q level, otherwise 'white'.
#' The POA at significance level q is plotted as dashed grey line.
#'
#' If Species = 'all' (default), then all classes are plotted. Instead of density function for trained POA, the median (grey symbol)
#' and the range between quantiles q and 1-q are plotted.
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
#'@import stats graphics
#'@export plot.RFPH
#'
#'@seealso \code{\link{rf.post.hoc}}

### Method plot
plot.RFPH <- function(x, Species = "all", q = 0.05) {

    classes <- levels(x$post.hoc$winner)
    if (Species %in% classes) {

        df <- x$ecdf[[Species]][[2]]
        qv <- quantile(df, q)
        sub_dat <- x$post.hoc[x$post.hoc$winner == Species, ]
        col_sig <- ifelse(sub_dat$POA >= qv, "red4", "white")
        prob <- runif(1000, 1e-04, 1)
        vy <- quantile(df, prob)
        plot(density(vy), xlim = c(0, 1), lwd = 2, main = Species)
        abline(v = qv, col = "grey", lty = 2)
        points(sub_dat$POA, rep(0, nrow(sub_dat)), pch = 21, bg = col_sig, cex = 2)

    } else if (Species == "all") {
        c1 <- unique(x$post.hoc$winner)
        min_POA <- floor(min(x$post.hoc$POA) * 100)/100
        plot(1:length(c1), 1:length(c1), xlim = c(min_POA - 0.5, 1.2), ylim = c(0,
            length(c1) + 1.5), yaxt = "n", xaxt = "n", bty = "n", xlab = "", ylab = "",
            type = "n")

        for (class in c1) {
            n_spec <- which(c1==class)
            sub_dat <- x$post.hoc[x$post.hoc$winner == class, ]
            df <- x$ecdf[[as.character(class)]][[2]]
            qv <- quantile(df, q)
            col_sig <- ifelse(sub_dat$POA >= qv, "red4", "white")
            prob <- runif(1000, 1e-04, 1)
            vy <- quantile(df, prob)
            median <- median(vy)
            q05 <- quantile(vy, q)
            q95 <- quantile(vy, 1 - q)
            arrows(q05, n_spec, q95, n_spec, lwd = 2, code = 3, angle = 90, length = 0.05)
            points(median, n_spec, pch = 23, cex = 2, bg = "grey")
            points(sub_dat$POA, rep(n_spec, nrow(sub_dat)), pch = 21, bg = col_sig,
                cex = 1.5)
            text(min_POA - 0.5, n_spec, labels = class, pos = 4, font = 3)
        }

        # make some lables and beauty
        abline(h = length(c1) + .6, lwd = 1.5)
        abline(h = 0.4, lwd = 1.5)
        axis(1, at = c(min_POA, min_POA + ((1 - min_POA)/2), 1), labels = c(min_POA,
            min_POA + ((1 - min_POA)/2), "1"), lwd.ticks = 1)
    } else (stop(paste(Species,'is not a valid class')))
}


