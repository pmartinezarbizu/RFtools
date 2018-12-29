#'@title Votes t-sne 
#'
#'@description Calculate t-distributed stochastic neighbor embedding on randomForest votes.
#'
#'
#'@param x A randomForest object
#'
#'@param method One of the transformations offered by decostand from package vegan. Default is 'none'
#'
#'@param perplex.factor Multiply the number of observations by the perplex.factor to get the perpelexity value passed to t-sne
#'
#'@param max_iter Maximum number of iterations: parameter passed to t-sne
#'
#'
#'@details
#' VotesPlot will use the votes matrix of a randomForest object to calculate the probabiliy that
#' two observations belong to the same cluster using t-distributed stochastic neighbor embedding from package tsne.
#' An t-sne ordination graph is plotted to trace the results after every 100 iterations until max_iter is reached. 
#' Main control parameter of t-sne analysis is the perplexity value. Here we use a default perplexity
#' of 15% of the number of observations. The default perplex.factor = 0.15 is used to calculate perplexity.
#' Contrasting plot symbols and colors are assigned automatically, but can be adjusted manually.
#' Although not recommended here, it might be interesting to transform the votes table prior to t-sne.
#' The argument method can be used to pass the method to decostand. Hellinger transformation might be meaningful. 
#' Consider to increase max_iter if there is no stabilization of the error after the default 1000 iterations.
#'
#' returns an object of type votes_tsne 
#' A plot method is available
#'
#'@author Pedro Martinez Arbizu
#'
#'@import randomForest tsne stats graphics vegan
#'@examples
#' data(iris)
#' rf <- randomForest(Species ~.,data = iris)
#' vt <- votes.tsne(rf)
#' 
#' plot(vt,margin=1,y.low=5)
#'
#'@export votes.tsne
#'@seealso \code{\link{plot.votes_tsne}} 


votes.tsne <- function(x, method = 'none', perplex.factor = 0.15, max_iter=1000)
{
	#initial checks
	if(!(inherits(x,'randomForest'))) {
        stop('x should be a randomForest Object\n ')
    }

    if (perplex.factor >= 1) {
        stop('perplex.factor should be > 0 < 1\n ')
    }

    if (perplex.factor <= 0) {
        stop('perplex.factor should be > 0 < 1\n ')
    }

	#define some plotting parameters for tracing graphs	
	cex=1.5
	alpha=220
	
	# define some colors	
	colors <- c(
	'lightblue' =rgb(102,203,254,maxColorValue=255,alpha=alpha),
	'pink' =rgb(254,102,254,maxColorValue=255,alpha=alpha),
	'green' =rgb(102,254,102,maxColorValue=255,alpha=alpha),
	'yellow' =rgb(254,203,102,maxColorValue=255,alpha=alpha),
	'darkblue' =rgb(  0,128,128,maxColorValue=255,alpha=alpha),
	'bgpng' =rgb(32, 32, 32, maxColorValue=255,alpha=alpha),
	'AJ' =rgb(240,240,  0,maxColorValue=255,alpha=alpha),
	'redbrown' =rgb(200,100,50,max=255,alpha=alpha),
	'B6'  =rgb(128,128,128,maxColorValue=255,alpha=alpha),
	'steelgreen' =rgb(20,210,200,max=255,alpha=alpha),
	'129' =rgb(240,128,128,maxColorValue=255,alpha=alpha),
	'NOD2' =rgb( 16, 14,250,maxColorValue=255,alpha=alpha),
	'CAST'=rgb(  0,160,  0,maxColorValue=255,alpha=alpha),
	'marfil2' =rgb(150,150,200,max=255,alpha=alpha),
	'NZO2' =rgb(  0,180,255,maxColorValue=255,alpha=alpha),
	'PWK' =rgb(240,  0,  0,maxColorValue=255,alpha=alpha),
	'WSB' =rgb(144,  0,224,maxColorValue=255,alpha=alpha),
	'hotpink'    =rgb(254,  0,128,maxColorValue=255,alpha=alpha),	              
	'myorange'     =rgb(255,  170,0,maxColorValue=255,alpha=alpha),
	'mypink' =rgb(250,150,200,max=255,alpha=alpha),
	'lightpurple'=rgb(190,192,50,maxColorValue=255,alpha=alpha)
	)

 #define matching table
	coltable <- data.frame(class=unique(x$predicted),
					color=colors[unique(x$predicted)],
					symbol=rep(21:24,ceiling(length(unique(x$predicted))/4))[1:length(unique(x$predicted))],
					symbol2=rep(c(19,15,18,17),ceiling(length(unique(x$predicted))/4))[1:length(unique(x$predicted))]
					)
	#define some plotting parameters for tracing graphs	
	pch = coltable$symbol[match(x$predicted,coltable$class)]
	bg = coltable$color[match(x$predicted,coltable$class)]

	#check if transformation is needed
	ifelse (method == 'none', dat <- x$votes, dat <- decostand(x$votes,method=method))

	#define tracing function	
	ecb <- function(x,y){ plot(x,cex=cex,pch=pch,bg=as.vector(bg),xlab='',ylab='')}

	#calculate t-sne
	tsne <- tsne(dat, epoch_callback = ecb, perplexity= nrow(x$votes)*perplex.factor,max_iter=max_iter)
	
	res <- list(points=tsne,predicted=x$predicted,match=coltable)
	class(res) <- c("votes_tsne", "list")
    return(res)
}


