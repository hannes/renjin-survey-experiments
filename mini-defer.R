#svydata <- readRDS("alabama.rds")
#wts <- svydata[,200:279]

svydata <- readRDS("california.rds")
wts <- svydata[,195:274]

x <- svydata$agep
dim(x) <- c(length(x), 1)
pw <- 1L

print(system.time({
	repmeans<-matrix(ncol=NCOL(x), nrow=ncol(wts))
	for(i in 1:ncol(wts)){
	  repmeans[i,]<-t(colSums(wts[,i]*x*pw)/sum(pw*wts[,i]))
	}
}))

print(system.time({
	print(repmeans)
}))