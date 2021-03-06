#'@title Plot votes_tsne  
#'
#'@description Plot a votes_tsne object
#'
#'@param x A votes_tsne object
#'
#'@param bg Fill color of symbols
#'
#'@param main Title passed to plot
#'
#'@param alpha The transparency value for the symbols
#' 
#'@param margin The Graphical parameter passed to oma for the lower margin, to accomodate legend
#'
#'@param ncol The number of columns for the legend
#'
#'@param x.intersp Space between legend symbol and legend text
#'
#'@param y.low Distance of legend from y axis
#'
#'@param pal Color palette to be sued for symbols (default is internal colors)
#'
#'@details
#' Plot the resuls of a t-sne analysis of the randomForest votes matrix.
#' The fuction has an internal color palette 'colors' that allows for ajustement
#' of the transparency values using the parameter alpha.
#' USe your own color palette with the parameter pal (note that alpha will not wrok then)
#' The graph legend can be easyly placed at the bottom of the graph
#' using the parameters margin, ncol and y.low.
#' 
#' 
#'@author Pedro Martinez Arbizu
#'
#'@import graphics 
#'@examples
#' data(maldi)
#' rf <- randomForest(species ~.,data = maldi[,-1])
#' vt <- votes.tsne(rf)
#' plot(vt)
#' plot(vt,margin=5,y.low=3,ncol=3,cex=2,alpha=180,main='rf votes on Maldi-Tof spectra')
#'
#'@export plot.votes_tsne
#'@exportS3Method plot votes_tsne
#'@seealso \code{\link{votes.tsne}} 

plot.votes_tsne <- function(x, cex=1.5, 
							pch = coltable$symbol[match(x$predicted,coltable$class)],
							bg = coltable$color[match(x$predicted,coltable$class)],
							main='',
							alpha=220,
							margin=7,
							ncol=4,
							x.intersp=1,
							y.low=3,
							pal=colors) {
 
 	# define some colors	
	colors <- c(
	'lightblue' =rgb(102,203,254,maxColorValue=255,alpha=alpha),
	'pink' =rgb(254,102,254,maxColorValue=255,alpha=alpha),
	'green' =rgb(102,254,102,maxColorValue=255,alpha=alpha),
	'yellow' =rgb(254,203,102,maxColorValue=255,alpha=alpha),
	'darkblue' =rgb(  0,128,128,maxColorValue=255,alpha=alpha),
	'bgpng' =rgb(32, 32, 32, maxColorValue=255,alpha=alpha),
	'AJ' =rgb(240,240,  0,maxColorValue=255,alpha=alpha),
	'B6'  =rgb(128,128,128,maxColorValue=255,alpha=alpha),
	'redbrown' =rgb(200,100,50,max=255,alpha=alpha),
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
					color=pal[unique(x$predicted)],
					symbol=rep(21:24,ceiling(length(unique(x$predicted))/4))[1:length(unique(x$predicted))],
					symbol2=rep(c(19,15,18,17),ceiling(length(unique(x$predicted))/4))[1:length(unique(x$predicted))]
					)
	
	# change margins to allow legend
	par(oma = c(margin, 0, 0, 0),xpd=NA)
	
	#plot graph
	plot(x$points,cex=cex,pch=pch,bg=as.vector(bg),xlab='',ylab='',main=main)

	#add the legnd
	legend(min(x$points[,1]), min(x$points[,2])-y.low,bty='n',ncol=ncol,
	pt.cex=1.5, x.intersp=x.intersp,legend=coltable$class,pch=coltable$symbol2, col= as.vector(coltable$color))

	legend(min(x$points[,1]), min(x$points[,2])-y.low,bty='n',ncol=ncol,
	pt.cex=1.5, x.intersp=x.intersp,legend=coltable$class,pch=coltable$symbol)

}
