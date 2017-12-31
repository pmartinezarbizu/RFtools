#'@title Plot Results of MVSF Tests
#'
#'@description Plot results of Multivariate Structure and Flexibility Test.
#'
#'
#'@param x Object created by MVSF.test()
#'
#'@param pnull Significance level for the multivariate structure test
#'
#'@param psmooth Significance level for the flexibility test
#'
#'
#'@details
#' For each class, the trained OOB, the prediction error of smoothed class
#' and the error under null model is plotted. For the null error the 0.5-0.95 quantile interval is plotted as error line.
#' Filled symbols indicate significant difference under alfa level defined by pnull and psmooth.
#'
#'
#'
#'@author Pedro Martinez Arbizu
#'
#'@import stats graphics
#'@examples
#' data(iris)
#' MVSF.iris <- MVSF.test(iris$Species,iris[,1:4])
#' print(MVSF.iris)
#' plot(MVSF.iris)
#'@export plot.MVSF



############################################ plot the results of MVSF.test() x is a dataframe created by MVS.test()
plot.MVSF <- function(x, pnull = 0.05, psmooth = 0.05) {

    # colors, black is significant
    col.p.sim <- ifelse(x$p.smooth < psmooth, "black", "white")
    col.p.par <- ifelse(x$P.null > pnull, "black", "white")

    plot(x$oob.err, seq(nrow(x), 1, -1), pch = 2, xlim = c(-1.5, 1.2), ylim = c(0,
        nrow(x) + 1.5), yaxt = "n", xaxt = "n", bty = "n", xlab = "", ylab = "",
        cex = 1.5)
    points(x$smooth.err, seq(nrow(x), 1, -1), pch = 25, cex = 1.5, bg = col.p.sim)
    points(x$null.err, seq(nrow(x), 1, -1), pch = 21, bg = col.p.par, cex = 1.5)

    # add confidence intervals
    cd = nrow(x):1
    for (i in 1:nrow(x)) {
        arrows(x$q05.null[cd[i]], nrow(x) - (i - 1), x$q95.null[cd[i]], nrow(x) -
            (i - 1), lwd = 1.5, code = 3, angle = 90, length = 0.04)
    }

    # add species names
    for (i in 1:nrow(x)) {
        text(-1.5, nrow(x) - (i - 1), label = rownames(x)[i], font = 3, pos = 4,
            cex = 1)
    }

    # make some labels and beauty
    abline(h = nrow(x) + 0.5, lwd = 1.5)
    abline(h = 0.5, lwd = 1.5)

    points(0, nrow(x) + 1.5, pch = 2, cex = 1.2)
    text(0, nrow(x) + 1.5, labels = "trained oob error", pos = 4)

    points(0, nrow(x) + 0.9, pch = 6, cex = 1.2)
    text(0, nrow(x) + 0.9, labels = "smoothed class error", pos = 4)

    points(0.95, nrow(x) + 0.9, pch = 21, bg = "white", cex = 1.2)
    text(0.95, nrow(x) + 0.9, labels = "null error", pos = 4)

    text(-1.5, nrow(x) + 1, labels = "class", pos = 4)

    axis(1, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"), lwd.ticks = 1)
}  # END of function
############################################
