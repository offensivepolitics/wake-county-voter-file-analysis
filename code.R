## clean the original voter file
## drop unused fields, rename others. format the registration date as a Date object.
wv <- read.csv("WakeVoters_20111121.txt")
wv.a <- wv[c("voter_reg_num","res_zip_code","race","gender","party","Age","registr_dt","voter_status","Pct","Muni","Cong","NCSen","NCHse","e_14","e_13","e_12","e_9","e_8","e_7","e_2","e_1")]
wv.a$registr_dt <- as.Date(wv.a$registr_dt,format="%Y%m%d")
names(wv.a) <- c("voterid","zipcode","race","gender","party","age","regdate", "status","precinct","municipality", "congress","ncsen","nchse","g2010","p2010-2","p2010-1","g2008","p2008-2","p2008-1","g2006","p2006")
write.csv(wv.a,"voterfile20111121.csv",row.names=F)
## i gzipped the file after this to make it easier to distribute

## load packages
library(ggplot2) # loads plyr automatically
library(gmodels) # for CrossTable function
library(RColorBrewer) # for pretty colors

## load the voterfile

vf <- read.csv("voterfile20111121.csv.gz")
vf$regdate <- as.Date(as.character(vf$regdate),format="%Y%m%d")


## create agegroup 17-29,30-39,40-49,50-59,60-69,70-79,80-89
vf$age.group <- cut(vf$age,breaks=c(17,30,40,50,60,70,80,90,100,110,120),right=F)

## collapse the voting type to simply did vote
vf$g2010.v <- (vf$g2010 != "")
vf$g2008.v <- (vf$g2008 != "")
vf$g2006.v <- (vf$g2006 != "")

## simple crosstabs
### table is nice but CrossTable is where its at
CrossTable(vf$status,prop.c=F,prop.chisq=F,format="SPSS",max.width=10)

### gender
CrossTable(vf$gender,prop.c=F,prop.chisq=F,format="SPSS",max.width=10)

### age group
CrossTable(vf$age.group,prop.c=F,prop.chisq=F,format="SPSS",max.width=10)

### party registration
CrossTable(vf$party,prop.c=F,prop.chisq=F,format="SPSS",max.width=10)

### party affiliation by gender
CrossTable(vf$gender,vf$party,prop.c=F,prop.chisq=F,format="SPSS",max.width=10)

### age distribution of all registered voters
qplot(age.group,data=vf,type="histogram",main="Wake County Registered Voters, by Age Group ")

### 2010 turnout by age group
qplot(age.group,data=vf[vf$regdate <= "2010-11-04",],type="histogram",fill=g2010.v,position="dodge",main="Wake Count 2010 Turnout, by Age Group") + scale_fill_brewer(name="Voted 2010", pal="Set1")

### Turnout in 2010 vs 2008, by Gender
#### summarize gender / turnout by year
gender.turnout <- ddply(vf,"gender",function(x) { 
data.frame(total=nrow(x),turnout=c(sum(x$g2010.v),sum(x$g2008.v)),election=c("2010","2008")) })
#### plot data
qplot(gender,turnout / total, data=gender.turnout, geom="histogram", stat="identity",fill=election,position="dodge", main="Turnout in 2010 vs 2008 Wake County, by Gender") + scale_fill_brewer(name="Election cycle",pal="Set1") 

### Turnout in 2010 vs 2008 by precinct
#### summarize precinct turnout
precinct.turnout <- ddply(vf, "precinct", summarize, turnout2010=sum(g2010.v) / length(g2010.v), turnout2008=sum(g2008.v) / length(g2010.v),reg2010=length(g2008.v) )
#### plot
qplot(turnout2010, turnout2008, data=precinct.turnout,xlim=c(.1,1),ylim=c(.1,1),main="Turnout percentage 2008 to 2010, by Precinct") + geom_abline(intercept=0,slope=1)

#### fit a linear model & print it out
to2010.model <- lm(turnout2008~turnout2010,data=precinct.turnout)
summary(to2010.model)
#### redo the last plot but now with a line of best fit
qplot(turnout2010, turnout2008, data=precinct.turnout,xlim=c(.1,1),ylim=c(.1,1),main="Turnout percentage 2008 to 2010, by Precinct") + geom_abline(intercept=0,slope=1) + geom_abline(intercept=0.4124,slope=.62)

### Democratic registration percentage and 2010 turnout by precinct
#### use ddply to summarize registration and turnout by precinct
dem.reg.prec <- ddply(vf, "precinct", summarize, registered=length(status),turnout2010=sum(g2010.v),dem.pct=sum(party == "DEM") / length(party) )
##### now plot it
qplot(dem.pct,turnout2010/registered,data=dem.reg.prec,alpha=I(0.8), main="Democratic registration percentage and 2010 turnout\n by precinct",xlab="Democratic Registration%", ylab="Turnout 2010 (All Parties)",xlim=c(0,1), ylim=c(0,1))

