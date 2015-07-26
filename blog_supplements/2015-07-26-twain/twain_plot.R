(x<-read.csv('twain.csv',header=TRUE))


firstplot<-function(){
	par(mar = c(5,5,2,5))
	plot(x$year,x$wordsPerDay,type='o',ylim=c(0,max(x$wordsPerDay)),main="Mark Twain's Writing Speed",ylab='', xlab='Year',col='darkred',lwd=2,lty=2)
	mtext(side=2,line=2.5,"Words per day")
}

png(file='twain_words_per_day.png',height=550,width=550,pointsize=16)
	firstplot()
dev.off()


png(file='twain_words_per_hour.png',height=550,width=550,pointsize=16)
	firstplot()

	par(new=TRUE)
	skip<-is.na(x$hrsPerDay)
	ylim2<-c(0,2*max(x$wordsPerDay/x$hrsPerDay,na.rm=TRUE))
	plot(x$year[!skip],(x$wordsPerDay/x$hrsPerDay)[!skip],type='o',axes=FALSE,xlab=NA,ylab=NA,col='darkblue',lwd=2, ylim=ylim2,lty=3)
	axis(side=4)
	mtext(side=4,line=2.5,"Words per hour")

	legend('bottomleft',c('Words per day','Words per hour'),col=c('darkred','darkblue'),lty=c(2,3),lwd=2,pch=1)
dev.off()

