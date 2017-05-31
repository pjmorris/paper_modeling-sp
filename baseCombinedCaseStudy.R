


# First thing Sunday: Build merged dataset
#	Openhub base
#	NVD to 2012, summed not meaned (nevermind)
#	CII
#	WLSMV, ordered CVSS
#	as complete a picture as you can get of the theorized data
#	pull BadgeApp data?
# Step 1: Collect OpenHub projects
# create mdddf

library(lavaan)
library(semPlot)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)

#### WHAT DO YOU REALLY NEED HERE?
# THESE FUNCTIONS TRANSLATE CVSS METRICS TO 'RISK' factors 
# cvss_access_vector=avec2num(nvd$cvss_access_vector)
# access_vector: LOCAL NETWORK ADJACENT_NETWORK
avec2num <- function(v) { return(ifelse(v == "LOCAL",1,ifelse(v == "ADJACENT_NETWORK",2,ifelse(v == "NETWORK",3,0)))) }

# cvss_access_complexity=acplx2num(nvd$cvss_access_complexity)
# access_complexity: HIGH MEDIUM LOW
acplx2num <- function(v) { return(ifelse(v == "HIGH",1,ifelse(v == "MEDIUM",2,ifelse(v == "LOW",3,0)))) }

# cvss_auth=auth2num(nvd$cvss_auth)
# auth: MULTIPLE_INSTANCES SINGLE_INSTANCE NONE
auth2num <- function(v) { return(ifelse(v == "MULTIPLE_INSTANCES",1,ifelse(v == "SINGLE_INSTANCE",2,ifelse(v == "NONE",3,0)))) }

# cvss_conf_impact=imp2num(nvd$cvss_conf_impact)
# impact: NONE PARTIAL COMPLETE
imp2num <- function(v) { return(ifelse(v == "NONE",1,ifelse(v == "PARTIAL",2,ifelse(v == "COMPLETE",3,0)))) }

# THESE FUNCTIONS TRANSLATE CVSS METRICS TO ORDINALS 
# cvss_access_vector=avec2num(nvd$cvss_access_vector)
# access_vector: LOCAL NETWORK ADJACENT_NETWORK
cvssavec2num <- function(v) { return(ifelse(v == "LOCAL",1,ifelse(v == "ADJACENT_NETWORK",2,ifelse(v == "NETWORK",3,0)))) }

# cvss_access_complexity=acplx2num(nvd$cvss_access_complexity)
# access_complexity: HIGH MEDIUM LOW
cvssacplx2num <- function(v) { return(ifelse(v == "HIGH",1,ifelse(v == "MEDIUM",2,ifelse(v == "LOW",3,0)))) }

# cvss_auth=auth2num(nvd$cvss_auth)
# auth: MULTIPLE_INSTANCES SINGLE_INSTANCE NONE
cvssauth2num <- function(v) { return(ifelse(v == "MULTIPLE_INSTANCES",1,ifelse(v == "SINGLE_INSTANCE",2,ifelse(v == "NONE",3,0)))) }

# cvss_conf_impact=imp2num(nvd$cvss_conf_impact)
# impact: NONE PARTIAL COMPLETE
cvssimp2num <- function(v) { return(ifelse(v == "NONE",1,ifelse(v == "PARTIAL",2,ifelse(v == "COMPLETE",3,0)))) }

### WHAT FUNCTIONS DO YOU REALLY NEED?

setwd("/Users/admin/Dropbox/github/paper_modeling-sp/Draft")

nvd <- read.csv("~/Dropbox/NCSU/Spring2013/CSC720_AI/NVD_PGM_Project/nvdall2.csv",stringsAsFactors=FALSE)

mddraw <- read.csv("~/Dropbox/github/paper_modeling-sp/naggapan_2013_masterdata.txt",sep="\t",stringsAsFactors=FALSE)
# cbind(nvdr2s[nvdr2s$software == "abuse",],mddraw[mddraw$url_name %in% "abuse",])
mddplist <- unique(levels(mddraw$url_name))

ciiraw <- read.csv("~/Dropbox/github/cii-census/results.csv")


nvdxlated <- data.frame(pub_year=nvd$pub_year,adherence=as.numeric(nvd$pub_year),cvss_score=nvd$cvss_score,
cvss_access_vector=cvssavec2num(nvd$cvss_access_vector),
cvss_access_complexity=cvssacplx2num(nvd$cvss_access_complexity),
cvss_auth=cvssauth2num(nvd$cvss_auth),
AccessVectorRisk=avec2num(nvd$cvss_access_vector),
AccessComplexityRisk=acplx2num(nvd$cvss_access_complexity),
AuthenticationRisk=auth2num(nvd$cvss_auth),
cvss_conf_impact=imp2num(nvd$cvss_conf_impact),
cvss_integ_impact=imp2num(nvd$cvss_integ_impact),
cvss_avail_impact=imp2num(nvd$cvss_avail_impact),
cve_id=nvd$cve_id,mfgr=nvd$mfgr,software=nvd$software,cwe_id=nvd$cwe_id)

nvdxlated$cvss_score <- as.numeric(levels(nvdxlated$cvss_score))[nvdxlated$cvss_score]
nvdxlated$pubyear <- as.numeric(levels(nvdxlated$pub_year))[nvdxlated$pub_year]
nvdxlated$adherence <- nvdxlated$pubyear - 1988
nvdxlated$adherence <- scale(nvdxlated$adherence) + abs(min(scale(nvdxlated$adherence),na.rm=TRUE))


nvdxlated$cvss_score <- as.numeric(levels(nvdxlated$cvss_score))[nvdxlated$cvss_score]
nvdxlated$pubyear <- as.numeric(levels(nvdxlated$pub_year))[nvdxlated$pub_year]
nvdxlated$adherence <- nvdxlated$pubyear - 1988
nvdxlated$adherence <- scale(nvdxlated$adherence) + abs(min(scale(nvdxlated$adherence),na.rm=TRUE))


rolled <- group_by(nvdxlated[nvdxlated$pubyear <= 2012,],software) %>% summarise(pubyear=max(pubyear),CVECount=n(),cvss_score=mean(cvss_score),adherence=mean(adherence),cvss_auth=median(cvss_auth),cvss_access_vector=median(cvss_access_vector),cvss_access_complexity=median(cvss_access_complexity),cvss_conf_impact=median(cvss_conf_impact), cvss_integ_impact=median(cvss_integ_impact), cvss_avail_impact=median(cvss_avail_impact),AuthenticationRisk=mean(AuthenticationRisk),AccessVectorRisk=mean(AccessVectorRisk),AccessComplexityRisk=mean(AccessComplexityRisk))

rolled_software <- data.frame(software=rolled$software,CVECount=rolled$CVECount, logCVECount=log(rolled$CVECount+1),cvss_score=rolled$cvss_score,adherence=scale(rolled$pubyear) + abs(min(scale(rolled$pubyear),na.rm=TRUE)),cvss_auth=rolled$cvss_auth,cvss_access_vector=rolled$cvss_access_vector,cvss_access_complexity=rolled$cvss_access_complexity,cvss_conf_impact=rolled$cvss_conf_impact, cvss_integ_impact=rolled$cvss_integ_impact, cvss_avail_impact=rolled$cvss_avail_impact,AuthenticationRisk=rolled$AuthenticationRisk,AccessVectorRisk=rolled$AccessVectorRisk,AccessComplexityRisk=rolled$AccessComplexityRisk,PubYear=rolled$pubyear)

rolled_software$adherence <- scale(rolled_software$adherence) + abs(min(scale(rolled_software$adherence),na.rm=TRUE))


ciiraw$DevChurnAdh <-  ciiraw$total_contributor_count / ciiraw$total_code_lines

ciiraw$CVE <- ciiraw$CVE_since_2010
ciiraw$RiskIndex <- ciiraw$risk_index

ciiraw$RiskIndex <- as.numeric(ciiraw$risk_index)

ciiraw$TeamActivity <- as.numeric(act2num(ciiraw$fact_activity))
ciiraw$CodeAge <- as.numeric(age2num(ciiraw$fact_age))
ciiraw$CodeComments <- as.numeric(cmt2num(ciiraw$fact_comments))
ciiraw$TeamSize <- as.numeric(tsz2num(ciiraw$fact_team_size))

assignRiskIndexFields <- function(row) {
	for (c in strsplit(as.character(row$risk_index_components),",")) {fields <- strsplit(c,":")}
	
	row[["WebsiteRisk"]] = as.integer(fields[[1]][2])
	row[["CVERisk"]] = as.integer(fields[[2]][2])
	row[["ContributorRisk"]] = as.integer(fields[[3]][2])
	row[["PopularityRisk"]] = as.integer(fields[[4]][2])
	row[["LanguageRisk"]] = as.integer(fields[[5]][2])
	row[["ExposureRisk"]] = as.integer(fields[[6]][2])
	row[["DataRisk"]] = as.integer(fields[[7]][2])
	row 
}

ciiraw[["WebsiteRisk"]] = NA
ciiraw[["CVERisk"]] = NA
ciiraw[["ContributorRisk"]] = NA
ciiraw[["PopularityRisk"]] = NA
ciiraw[["LanguageRisk"]] = NA
ciiraw[["ExposureRisk"]] = NA
ciiraw[["DataRisk"]] = NA

for (i in 1:length(ciiraw$project_name)) {ciiraw[i,] <- assignRiskIndexFields(ciiraw[i,]) }


ciicooked <- ciiraw[!is.na(ciiraw$DevChurnAdh),c(1,16,6,13,14,15,22,25,26,27,28,31,32,33,34,35,36,37,38,39,40,41,42,43)]


ciicooked$log12mcc <- log(ciicooked$twelve_month_contributor_count+1)
ciicooked$logCVE2010 <- log(ciicooked$CVE_since_2010+1)
ciicooked$logCVECount <- log(ciicooked$CVE+1)
ciicooked$KSLOC <- ciicooked$total_code_lines/1000
ciicooked$logKSLOC <- log(ciicooked$KSLOC+1)
ciicooked$logSLOC <- log(ciicooked$total_code_lines+1)

#ciicooked$total_code_lines <- log(ciicooked$total_code_lines/1000)
ciicooked$logContributorCount <- log(ciicooked$total_contributor_count)
ciicooked$logPackagePopularity <- log(ciicooked$package_popularity)

scaled.cii <- data.frame(apply(ciicooked[,-c(1,2)],2,scale))



# Step 2: add fields for other data sets
empty_nvd <- rolled_software[1,-c(1:3)]
empty_nvd[1,names(empty_nvd)] <- NA
empty_nvd$CVECount <- 0

empty_cii <- ciicooked[1,c(3,4,5,6,7,8,9,10,11,13,15,16,17,18,19,20,21,22,23,24)]
empty_cii[1,names(empty_cii)] <- NA
empty_cii$isCII <- FALSE
# Step 3: Roll up NVD projects to Openhub specs (2012), one project per row, add NVD fields to OpenHub list, NA or values as applicable (+isNVD)

# borrowed

# fresh
trythis <- left_join(mddraw,rolled_software,by=c("url_name" = "software"))

# you may need: str(trythis) 
# trythis$adherence <- as.numeric(trythis$adherence)

tr <- left_join(trythis,ciicooked,by=c("url_name" = "project_name")) # next, try with ciiraw[1,c(1,16,18,19,20,21,6,13,14,15,22,25,26,27,32:44)]


tr$isCII <- FALSE
tr[!is.na(tr$risk_index),]$isCII <- TRUE

tr$isNVD <- FALSE
tr[!is.na(tr$cvss_score),]$isNVD <- TRUE

tr[is.na(tr$CVECount),]$CVECount <- 0
tr$log12mcc <- log(tr$twelve_month_contributor_count.x+1)
tr$logCVE2010 <- log(tr$CVE_since_2010+1)
tr$logCVECount <- log(tr$CVECount+1)
tr$KSLOC <- (tr$total_code_lines.x+1)/1000
tr$logKSLOC <- log(tr$KSLOC+1)
tr$logSLOC <- log(tr$total_code_lines.x+1)

tr$project_age <- (as.numeric(as.Date(tr$max_month) - as.Date(tr$min_month))/365)*12

tr$codechurn12 <- tr$code_added_12months + tr$code_removed_12months
tr$commentchurn12 <- tr$comments_added_12months + tr$comments_removed_12months
tr$blankchurn12 <- tr$blanks_added_12months + tr$blanks_removed_12months

tr$DevChurnAdh12 <- tr$twelve_month_contributor_count.x / (tr$code_churn_12months+1)

length(tr[tr$isCII==TRUE,]$name)
# 57
length(tr[tr$isNVD==TRUE,]$name)
# 698


tr2scale <- tr[,c(5,16,6:8,13,14,23,47,49,50,51,52:63,65:100)]

nu <- cbind(tr2scale[tr2scale$isNVD==TRUE ,c(1,2,10,55,56)],apply(tr2scale[tr2scale$isNVD==TRUE ,-c(1,2,10,55,56)],2,scale))

nu$DevChurnAdh12 <- nu$twelve_month_contributor_count.x / nu$code_churn_12months
nu$DevChurnAdh12 <- scale(nu$DevChurnAdh12)


nu2 <- cbind(tr2scale[,c(1,2,10,55,56)],apply(tr2scale[,-c(1,2,10,55,56)],2,scale))
nu2$DevChurnAdh12 <- nu2$twelve_month_contributor_count.x / nu2$code_churn_12months
nu2$DevChurnAdh12 <- scale(nu2$DevChurnAdh12)
nu2$logUserCount <- log(nu2$user_count+1)


# Could this be the answer you’re looking for?
model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x + twelve_month_contributor_count.x + project_age + code_churn_12months#
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevChurnAdh12#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count'#
fit <- sem(model.combinedl,data=nu2); 
summary(fit,fit.measures=TRUE)


summary(lm(CVECount ~
total_code_lines.x + twelve_month_contributor_count.x + project_age + DevChurnAdh12 + code_churn_12months + user_count,data=nu2))

summary(lm(CVECount ~
total_code_lines.x + twelve_month_contributor_count.x + project_age + DevChurnAdh12 + code_churn_12months + user_count,data=nu2[nu2$isNVD==TRUE,]))

summary(lm(CVECount ~
total_code_lines.x + twelve_month_contributor_count.x + project_age + DevChurnAdh12 + code_churn_12months + user_count,data=nu2[nu2$isCII==TRUE,]))

# generate figure
semPaths(fit,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)



# Aligned with Stanford SEM class
library(corrplot)
library(psych)
plot_matrix <- function(matrix_toplot){
	corrplot(matrix_toplot, is.corr = FALSE, 
			 type = 'lower', 
			 order = "original", 
			 tl.col='black', tl.cex=.75)
}


# trx <- tr[,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevChurnAdh12","user_count")]
trx <- scale(tr[tr$CVECount>0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevChurnAdh12","user_count")])


model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevChurnAdh12 + twelve_month_contributor_count.x#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
'#
fit2 <- sem(model.combinedl,data=trx); summary(fit2,fit.measures=TRUE)

semPaths(fit2,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)
anova(fit,fit2)

trx2 <- tr[tr$CVECount>0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevChurnAdh12","user_count")]
trx2$total_code_lines.x <- log(trx2$total_code_lines.x+1)
trx2$code_churn_12months <- log(trx2$code_churn_12months+1)
trx2$twelve_month_contributor_count.x <- log(trx2$twelve_month_contributor_count.x+1)
trx3 <- data.frame(scale(trx2))

model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevChurnAdh12 + twelve_month_contributor_count.x#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
'#
fit3 <- sem(model.combinedl,data=trx3); summary(fit3,fit.measures=TRUE)

anova(fit2,fit3)
plot_matrix(residuals(fit3)$cov)
residuals(fit3)
semPaths(fit3,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)



