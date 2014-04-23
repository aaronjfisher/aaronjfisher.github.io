
for max_abs_x, set from = range(all_x), passed to rescale_mid in ...
for images, replaces x with seq(min(x),max(x),length=n)

x<-runif(900)*4-1
n<-20

rescale_discrete<-function(x,n,interp=FALSE,...){
	if(interp) x<-seq(min(x),max(x),length=100)
	ind<-cut(rescale_mid(x,...),breaks=seq(0,1,length=n))
	(1:n)[ind]
}

n<-20
palDiv<-diverge_hcl(n)
pal(palDiv[rescale_discrete(x-2,n,from=c(-1,1)*5,interp=TRUE)])


map_pal<-function(x,col_pal,...){
	ind<-rescale_discrete(x,n=length(col_pal),...)
	col_pal[ind]
}

y<-rnorm(900)
#discrete
plot(x,y,col=map_pal(x,palDiv),pch=19)
#continuous
plot(x,y,col=div_gradient_pal()(rescale_mid(x)),pch=19)



















setwd("/Users/aaronfisher/Documents/JH/Website/pelican_test/content/blog_supplements/2014-04-08_map-col-to-variable")


####
# Pick a color palette
palDiv<-rev(diverge_hcl(60))

##########
# Figure 1
#
# Scatterplot example


# Generate sample data
library(MASS)
n<-600
set.seed(900)
Sigma<-matrix(c(
	1,0,.5,
	0,1,.8,
	.5,.8,2), nrow=3)
X<-mvrnorm(n,mu=c(0,1.2,0),Sigma=Sigma)


# Plot results

png(file='scatterplot_color.png',width=600,height=300,pointsize=13)

# Faster code with, fast.color.legend()
par(mfrow=c(1,2),mar=c(4,3,3,3))
plot(X[,1],X[,2],pch=19,col=mappal(X[,2],palDiv),main='Color mapped (redundantly)\nto Y variable',xlab='',ylab='')
mtext('X',1,2)
mtext('Y',2,2)
fast.color.legend(x=X[,1],y=X[,2],z=X[,2],digits=2,col=mappal(X[,2],palDiv,interp_x=TRUE))

plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palDiv),main='Color mapped to an\nexternal variable (Z)',xlab='',ylab='')
mtext('X',1,2)
mtext('Y',2,2)
fast.color.legend(x=X[,1],y=X[,2],z=X[,3],digits=2,col=mappal(X[,3],palDiv,interp_x=TRUE))

dev.off()

map_diverge<-function(x,max_abs_x=max(abs(x)),...)
	div_gradient_pal(...)(rescale_mid(x,from=c(-1,1)*max_abs_x))


x<-rnorm(600)
y<-rnorm(600)+1
z<-rnorm(600)

par(mfrow=c(1,2),mar=c(4,3,3,3))
plot(X[,1],X[,2],pch=19,col=mappal(X[,2],rev(palDiv)))
plot(X[,1],X[,2],pch=19,col=map_diverge(X[,2]))

plot(X[,1],X[,2],pch=19,col=mappal(X[,3],rev(palDiv)))
plot(X[,1],X[,2],pch=19,col=map_diverge(X[,3]))


par(mfrow=c(2,1),mar=c(1,1,1,1))
pal(palDiv)
pal(rev(map_diverge(-30:30)))



# More flexible code with, color.legend()
palDiv<-rev(diverge_hcl(40))
par(mfrow=c(1,2),mar=c(4,3,3,3))
plot(X[,1],X[,2],pch=19,col=mappal(X[,2],palDiv),main='Color mapped (redundantly)\nto Y variable',xlab='',ylab='')
mtext('X',1,2)
mtext('Y',2,2)
color.legend(xl=max(X[,1])+.4,xr=max(X[,1])+.8,yb=min(X[,2]),yt=max(X[,2]),rect.col=mappal(X[,2],palDiv,interp_x=TRUE),legend=signif(range(X[,2]),digits=2),gradient='y',align='rb')

plot(X[,1],X[,2],pch=19,col=mappal(X[,3],palDiv),main='Color mapped to an\nexternal variable (Z)',xlab='',ylab='')
mtext('X',1,2)
mtext('Y',2,2)
color.legend(xl=max(X[,1])+.4,xr=max(X[,1])+.8,yb=min(X[,2]),yt=max(X[,2]),rect.col=mappal(X[,3],palDiv,interp_x=TRUE),legend=signif(range(X[,3]),digits=2),gradient='y',align='rb')



#########
# Figure 2
#
# image() example
# sample generating code adapted from pp() function at:
# http://docs.ggplot2.org/0.9.3.1/geom_tile.html
ppMat <- function (n,r=4) {
 x <- seq(-r*pi, r*pi, len=n)
 mat<-matrix(NA,n,n)
 for(i in 1:n){
 	for(j in 1:n){
 		r <- sqrt(x[i]^2 + x[j]^2)
 		mat[i,j]<-cos(r^2)*exp(-r/6)
 }}
 mat
}

x<-ppMat(140)+.25

png(file='image_color.png',width=600,height=300,pointsize=13)

# Plot results
par(mfrow=c(1,2),mar=c(3,2,3,4))

# without mappal()
image(x,col=palDiv,main='Without mappal(),\nred=zero, gray>zero')
color.legend(xl=1.02, yb=0,xr=1.07,yt=1,legend=signif(range(x),digits=2), rect.col=palDiv,  align='rb', gradient='y')

# with mappal()
mappedColImage<-mappal(x,palDiv,interp_x=TRUE)
image(x,col=mappedColImage,main='With mappal(),\ngray=zero')
color.legend(xl=1.02, yb=0,xr=1.07,yt=1,legend=signif(range(x),digits=2), rect.col=mappedColImage,  align='rb', gradient='y')

dev.off()

##########
# Figure 3

# Common maximum intensity across plots
# Consider redundant plots as an example

png(file='compare_color.png',width=600,height=300,pointsize=13)

max_abs_x<-max(abs(X[,2]))
palDiv<-rev(diverge_hcl(40))
par(mfrow=c(1,2),mar=c(4,3,3,3))

subpop1_ind<-c(which(X[,2]>2),sample(which(X[,2]<2),n/2))
Xs1<-X[subpop1_ind,]
plot(Xs1[,1],Xs1[,2],pch=19,col=mappal(Xs1[,2],palDiv,max_abs_x=max_abs_x),main='Data subset 1:\ncolor mapped to Y variable',xlab='',ylab='',ylim=range(X[,2]),xlim=range(X[,1]))
mtext('X',1,2)
mtext('Y',2,2)
fast.color.legend(x=X[,1],y=X[,2],z=Xs1[,2],digits=2,col=mappal(Xs1[,2],palDiv,interp_x=TRUE,max_abs_x=max_abs_x))

Xs2<-X[-subpop1_ind,]
plot(Xs2[,1],Xs2[,2],pch=19,col=mappal(Xs2[,2],palDiv,max_abs_x=max_abs_x),main='Data subset 2:\ncolor mapped to Y variable',xlab='',ylab='',ylim=range(X[,2]),xlim=range(X[,1]))
mtext('X',1,2)
mtext('Y',2,2)
fast.color.legend(x=X[,1],y=X[,2],z=Xs2[,2],digits=2,col=mappal(Xs2[,2],palDiv,interp_x=TRUE,max_abs_x=max_abs_x))

dev.off()
