library(survey)
library(MonetDB.R) # cough, cough 

system.time({
	adata <- import(org.renjin.cran.MonetDBR.NoDBFrame)$new("/Users/hannes/source/survey-experiments/acsdata/alabama.csv",",")$df
	names(adata) <- tolower(names(adata))
})
system.time(
	alabama <- svrepdesign(
		weight = ~pwgtp ,
		repweights = 'pwgtp[1-9]' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		data = adata)
)
system.time(
	svymean(~agep, alabama, se=TRUE)
)
