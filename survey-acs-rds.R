library(survey)

system.time(
	svydata <- readRDS(paste0(commandArgs(TRUE),".rds"))
	#svydata <- readRDS("california.rds")
)	#svydata <- readRDS("acs3yr.rds")
  names(svydata) <- tolower(names(svydata))


system.time(
   svydsgn <- svrepdesign(
      weight = ~pwgtp ,
      repweights = 'pwgtp[1-9]' ,
      scale = 4 / 80 ,
      rscales = rep( 1 , 80 ) ,
      mse = TRUE ,
      data = svydata) 
)


options(na.action="na.pass")

# from survey package, R/surveyrep.R
# slightly patched to use numeric NAs in matrices and avoid over-decorating result
svymean <- function(x,design, na.rm=FALSE, rho=NULL,
                                           return.replicates=FALSE,deff=FALSE,...)
{
  # if (!exists(".Generic",inherits=FALSE))
  #   .Deprecated("svymean")
  if (!inherits(design,"svyrep.design")) stop("design is not a replicate survey design")
  
  if (inherits(x,"formula")){
    ## do the right thing with factors
    mf<-model.frame(x,design$variables,na.action=na.pass)
    xx<-lapply(attr(terms(x),"variables")[-1],
               function(tt) model.matrix(eval(bquote(~0+.(tt))),mf))
    cols<-sapply(xx,NCOL)
    x<-matrix(data=as.numeric(NA), nrow=NROW(xx[[1]]),ncol=sum(cols))
    scols<-c(0,cumsum(cols))
    for(i in 1:length(xx)){
      x[,scols[i]+1:cols[i]]<-xx[[i]]
    }
    colnames(x)<-do.call("c",lapply(xx,colnames))
  } else {
      if(typeof(x) %in% c("expression","symbol"))
          x<-eval(x, design$variables)
      else {
          if(is.data.frame(x) && any(sapply(x,is.factor))){
              xx<-lapply(x, function(xi) {if (is.factor(xi)) 0+(outer(xi,levels(xi),"==")) else xi})
              cols<-sapply(xx,NCOL)
              scols<-c(0,cumsum(cols))
              cn<-character(sum(cols))
              for(i in 1:length(xx))
                  cn[scols[i]+1:cols[i]]<-paste(names(x)[i],levels(x[[i]]),sep="")
              x<-matrix(nrow=NROW(xx[[1]]),ncol=sum(cols))
              for(i in 1:length(xx)){
                  x[,scols[i]+1:cols[i]]<-xx[[i]]
              }
              colnames(x)<-cn
          }
      }
  } 
  
  x<-as.matrix(x)
  
  if (na.rm){
    nas<-rowSums(is.na(x))
    design<-design[nas==0,]
    x<-x[nas==0,,drop=FALSE]
  }
  
  wts<-design$repweights
  scale<-design$scale
  rscales<-design$rscales
  if (!is.null(rho)) .NotYetUsed("rho")
  
  if (!design$combined.weights)
    pw<-design$pweights
  else
    pw<-1
  
  rval<-colSums(design$pweights*x)/sum(design$pweights)

  if (getOption("survey.drop.replicates") && !is.null(design$selfrep) && all(design$selfrep)){
    v<-matrix(0,length(rval),length(rval))
    repmeans<-NULL
  } else {
  if (inherits(wts, "repweights_compressed")){
    repmeans<-matrix(ncol=NCOL(x), nrow=ncol(wts$weights))
    for(i in 1:ncol(wts$weights)){
      wi<-wts$weights[wts$index,i]
      repmeans[i,]<-t(colSums(wi*x*pw)/sum(pw*wi))
    }
  } else {
    repmeans<-matrix(data=as.numeric(NA), ncol=NCOL(x), nrow=ncol(wts))
    for(i in 1:ncol(wts)){
      repmeans[i,]<-t(colSums(wts[,i]*x*pw)/sum(pw*wts[,i]))
    }
  }
  repmeans<-drop(repmeans)
  v <- svrVar(repmeans, scale, rscales,mse=design$mse, coef=rval)
}
# this is easy to fix, but for now...
return(v)
  attr(rval,"var") <-v
  attr(rval, "statistic")<-"mean"
  if (return.replicates){
    attr(repmeans,"scale")<-design$scale
    attr(repmeans,"rscales")<-design$rscales
    attr(repmeans,"mse")<-design$mse
    rval<-list(mean=rval, replicates=repmeans)
  }
  if (is.character(deff) || deff){
      nobs<-length(design$pweights)
      npop<-sum(design$pweights)
      vsrs<-unclass(svyvar(x,design,na.rm=na.rm, return.replicates=FALSE,estimate.only=TRUE))/length(design$pweights)
      if (deff!="replace")
        vsrs<-vsrs*(npop-nobs)/npop
      attr(rval,"deff") <- v/vsrs
  }
  class(rval)<-"svrepstat"
  rval
}

system.time({
      agep <- svymean(~agep, svydsgn, se=TRUE)
      relp <- svymean(~relp, svydsgn, se=TRUE)
      print(agep)
      print(relp)
    })[[3]]


# dd <- list(svymean(~agep, svydsgn, se=TRUE),
# 	svytotal(~I(relp %in% 0:15), svydsgn),
# 	svytotal(~I(relp %in% 0:15), svydsgn),
# 	svytotal(~I(relp %in% 16:17), svydsgn),
# 	svytotal(~I(relp == 16), svydsgn),
# 	svytotal(~I(relp == 17), svydsgn),
# 	tt <- svytotal(~sex, svydsgn) ,
# 	svytotal(~I(agep %in% 0:4) + I(agep %in% 5:9) + I(agep %in% 10:14) + I(agep %in% 15:19), svydsgn),
# 	svytotal(~I(agep %in% 20:24) + I(agep %in% 25:34) + I(agep %in% 35:44) + I(agep %in% 45:54), svydsgn),
# 	svytotal(~I(agep %in% 55:59) + I(agep %in% 60:64) + I(agep %in% 65:74) + I(agep %in% 75:84) + I(agep > 84), svydsgn)
# )
# print(dd)

