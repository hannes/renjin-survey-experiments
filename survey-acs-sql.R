library(MonetDB.R)
library(survey)
library(sqlsurvey)

# we need to clean up for now because there are tables left over atm
db <- dbConnect(MonetDB.R(), "acs")

svyobj <- sqlrepsurvey("pwgtp",paste("pwgtp",1:80,sep=""),
	scale=4/80,rscales=rep(1,80), mse=TRUE,
	database="monetdb://localhost/acs", driver=MonetDB.R(),
	key="idkey",user="monetdb",password="monetdb",table.name="alabama", check.factors=data.frame())


svymean(~agep, svyobj, se=TRUE)


#svytotal(~I(relp %in% 0:17), svyobj)
# svytotal(~I(relp %in% 0:15), svyobj)
# svytotal(~I(relp %in% 16:17), svyobj)
# svytotal(~I(relp ==16), svyobj)
# svytotal(~I(relp ==17), svyobj)
svytotal(~sex, svyobj)
# svytotal(~I(agep %in% 0:4)+I(agep %in% 5:9)+I(agep %in% 10:14)+I(agep %in% 15:19), svyobj)
# svytotal(~I(agep %in% 20:24)+I(agep %in% 25:34)+I(agep %in% 35:44)+I(agep %in% 45:54), svyobj)
# svytotal(~I(agep %in% 55:59)+I(agep %in% 60:64)+I(agep %in% 65:74)+I(agep %in% 75:84)+I(agep>84), svyobj)
