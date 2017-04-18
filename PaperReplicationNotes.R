# Goes in to the paper…

# For paper

setwd("/Users/admin/Dropbox/github/paper_modeling-sp/Draft")
semPaths(fit,,"std",title=FALSE,sizeLat=20,nCharNodes=15,residuals=FALSE,intercepts=FALSE,layout="spring",structural=TRUE,filetype="png",filename="modelzero",height=2,width=3)

# Another try for the paper
model.zero <- '
SoftwareRisk =~ SoftwareContextFactors
AssetRisk =~ AssetContextFactors
Outcomes =~ OutcomesContextFactors 
Adherence =~ AdherenceContextFactors
Outcomes ~ SoftwareRisk + AssetRisk
SoftwareRisk ~ Adherence

AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes'

semzero <- simulateData(model.zero,sample.nobs=8000L)
fit <- sem(model.zero,data=semzero)
semPaths(fit,,"std",title=FALSE,sizeLat=12,sizeMan=10,nCharNodes=25,residuals=FALSE,intercepts=FALSE,layout="spring",structural=TRUE,thresholds=FALSE,filetype="png",filename="modelzeroB",height=2,width=3)

model.syntax <- 'LatentVariable =~ MeasuredVariable1 + MeasuredVariable2'
semsyntax <- simulateData(model.syntax,sample.nobs=500L)
fit <- sem(model.syntax,data=semsyntax)
semPaths(fit,,"std",title=FALSE,sizeLat=12,sizeMan=10,nCharNodes=25,residuals=FALSE,intercepts=FALSE,layout="spring",structural=FALSE,thresholds=FALSE,filetype="png",filename="syntax_asmeasuredby",height=2,width=3)

model.syntax <- 'SoftwareRisk =~ Language + OperatingSystem + Domain + ProductAge + SLOC + CodeChurn + TeamSize'
semsyntax <- simulateData(model.syntax,sample.nobs=500L)
fit <- sem(model.syntax,data=semsyntax)
semPaths(fit,,"std",title=FALSE,sizeLat=12,sizeMan=10,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="spring",structural=FALSE,thresholds=FALSE,filetype="png",filename="syntax_swrisk_asmeasuredby",height=2,width=3)

model.syntax <- 'SoftwareRisk =~ Language + OperatingSystem + Domain + ProductAge + SLOC + CodeChurn + TeamSize'
semsyntax <- simulateData(model.syntax,sample.nobs=500L)
fit <- sem(model.syntax,data=semsyntax)
semPaths(fit,,"std",title=FALSE,sizeLat=12,sizeMan=10,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="spring",structural=FALSE,thresholds=FALSE,filetype="png",filename="syntax_swrisk_asmeasuredby",height=2,width=3)

semPaths(fit,,"std",title=FALSE,sizeLat=12,sizeMan=10,nCharNodes=25,residuals=FALSE,intercepts=FALSE,layout="spring",structural=FALSE,thresholds=FALSE,height=2,width=3)

model.syntax <- 'LatentVariable =~ MeasuredVariable1; LatentVariable2 =~ MeasuredVariable2; LatentVariable3 =~ MeasuredVariable3; LatentVariable ~ LatentVariable2 + LatentVariable3'
semsyntax <- simulateData(model.syntax,sample.nobs=8000L)
fit <- sem(model.syntax,data=semsyntax)
semPaths(fit,,"std",title=FALSE,sizeLat=12,sizeMan=10,nCharNodes=25,residuals=FALSE,intercepts=FALSE,layout="spring",structural=FALSE,filetype="png",filename="syntax_latentregress",thresholds=FALSE,height=2,width=3)


# latest table: variable, value low, value medium, value high
# recoded as 		1		2		3
# Case study details for the NVD dataset

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

# For conventional metric interpretation, rather than risk, use these:
# acplx2num <- function(v) { return(ifelse(v == "HIGH",3,ifelse(v == "MEDIUM",2,ifelse(v == "LOW",1,0)))) }
# auth2num <- function(v) { return(ifelse(v == "MULTIPLE_INSTANCES",3,ifelse(v == "SINGLE_INSTANCE",2,ifelse(v == "NONE",1,0)))) }

# THESE FUNCTIONS TRANSLATE CVSS METRICS TO ORDINALS 
# cvss_access_vector=avec2num(nvd$cvss_access_vector)
# access_vector: LOCAL NETWORK ADJACENT_NETWORK
cvssavec2num <- function(v) { return(ifelse(v == "LOCAL",1,ifelse(v == "ADJACENT_NETWORK",2,ifelse(v == "NETWORK",3,0)))) }

# cvss_access_complexity=acplx2num(nvd$cvss_access_complexity)
# access_complexity: HIGH MEDIUM LOW
cvssacplx2num <- function(v) { return(ifelse(v == "HIGH",3,ifelse(v == "MEDIUM",2,ifelse(v == "LOW",2,0)))) }

# cvss_auth=auth2num(nvd$cvss_auth)
# auth: MULTIPLE_INSTANCES SINGLE_INSTANCE NONE
cvssauth2num <- function(v) { return(ifelse(v == "MULTIPLE_INSTANCES",3,ifelse(v == "SINGLE_INSTANCE",2,ifelse(v == "NONE",1,0)))) }

# cvss_conf_impact=imp2num(nvd$cvss_conf_impact)
# impact: NONE PARTIAL COMPLETE
cvssimp2num <- function(v) { return(ifelse(v == "NONE",1,ifelse(v == "PARTIAL",2,ifelse(v == "COMPLETE",3,0)))) }

# For conventional metric interpretation, rather than risk, use these:
# acplx2num <- function(v) { return(ifelse(v == "HIGH",3,ifelse(v == "MEDIUM",2,ifelse(v == "LOW",1,0)))) }
# auth2num <- function(v) { return(ifelse(v == "MULTIPLE_INSTANCES",3,ifelse(v == "SINGLE_INSTANCE",2,ifelse(v == "NONE",1,0)))) }


nvd <- read.csv("~/Dropbox/NCSU/Spring2013/CSC720_AI/NVD_PGM_Project/nvdall2.csv",stringsAsFactors=FALSE)

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

# STOP HERE, and reapply nvdxlated

# scaling - THIS CODE TURNS nums into arrays and I don’t know why
#nvdabridged$adherence2 <- scale(nvdabridged$adherence) + abs(min(scale(nvdabridged$adherence),na.rm=TRUE))
#nvdxlated$cvss_conf_impact <- scale(nvdxlated$cvss_conf_impact) + abs(min(scale(nvdxlated$cvss_conf_impact),na.rm=TRUE))
#nvdxlated$cvss_avail_impact <- scale(nvdxlated$cvss_avail_impact) + abs(min(scale(nvdxlated$cvss_avail_impact),na.rm=TRUE))
#nvdxlated$cvss_integ_impact <- scale(nvdxlated$cvss_integ_impact) + abs(min(scale(nvdxlated$cvss_integ_impact),na.rm=TRUE))
#nvdxlated$cvss_access_vector <- scale(nvdxlated$cvss_access_vector) + abs(min(scale(nvdxlated$cvss_access_vector),na.rm=TRUE))
#nvdxlated$cvss_access_complexity <- scale(nvdxlated$cvss_access_complexity) + abs(min(scale(nvdxlated$cvss_access_complexity),na.rm=TRUE))
#nvdxlated$cvss_auth <- scale(nvdxlated$cvss_auth) + abs(min(scale(nvdxlated$cvss_auth),na.rm=TRUE))
# THIS CODE PROBABLY BROKE SOMETHING scaling - THIS CODE TURNS nums into arrays and I don’t know why

#


# You could probably just do this instead of all that…
nvdabridged <- nvdxlated[nvdxlated$pubyear > 2000 & nvdxlated$pubyear < 2017,]

# scaling
#nvdxlated$adherence2 <- scale(nvdxlated$adherence) + abs(min(scale(nvdxlated$adherence),na.rm=TRUE))



library(lavaan)
library(semPlot)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)


 model.zero.nvd <- '
SoftwareRisk =~ cvss_access_vector + cvss_access_complexity + cvss_auth
AssetRisk =~ cvss_conf_impact + cvss_integ_impact + cvss_avail_impact
Adherence =~ adherence
Outcomes =~ CVECount
Outcomes ~ SoftwareRisk + AssetRisk
SoftwareRisk ~  Adherence'

fit <- sem(model.zero.nvd,data=nvdxlated)
summary(fit,fit.measures=TRUE)


rolled <- group_by(nvdxlated,software,pubyear) %>% summarise(CVECount=n(),cvss_score=mean(cvss_score),adherence=mean(adherence),cvss_auth=mean(cvss_auth),cvss_access_vector=mean(cvss_access_vector),cvss_access_complexity=mean(cvss_access_complexity),cvss_conf_impact=mean(cvss_conf_impact), cvss_integ_impact=mean(cvss_integ_impact), cvss_avail_impact=mean(cvss_avail_impact),AuthenticationRisk=mean(AuthenticationRisk),AccessVectorRisk=mean(AccessVectorRisk),AccessComplexityRisk=mean(AccessComplexityRisk))

rolled_software <- data.frame(CVECount=rolled$CVECount, logCVECount=log(rolled$CVECount+1),cvss_score=rolled$cvss_score,adherence=scale(rolled$pubyear) + abs(min(scale(rolled$pubyear),na.rm=TRUE)),cvss_auth=rolled$cvss_auth,cvss_access_vector=rolled$cvss_access_vector,cvss_access_complexity=rolled$cvss_access_complexity,cvss_conf_impact=rolled$cvss_conf_impact, cvss_integ_impact=rolled$cvss_integ_impact, cvss_avail_impact=rolled$cvss_avail_impact,AuthenticationRisk=rolled$AuthenticationRisk,AccessVectorRisk=rolled$AccessVectorRisk,AccessComplexityRisk=rolled$AccessComplexityRisk)

rolled_software$adherence <- scale(rolled_software$adherence) + abs(min(scale(rolled_software$adherence),na.rm=TRUE))

rtrunc <- rolled_software[rolled_software$CVECount > 1 & rolled_software$CVECount < 50,]
summary(lm(
 CVECount ~ cvss_access_vector + cvss_access_complexity + cvss_auth + adherence
 + cvss_conf_impact + cvss_integ_impact + cvss_avail_impact
,data=rolled_software[rolled_software$CVECount > 1 & rolled_software$CVECount < 50,]))

# NVD: No constraints on latent variables
model.zero.nvd.norules <- '
SoftwareRisk =~ cvss_access_vector + cvss_access_complexity + cvss_auth
AssetRisk =~ cvss_conf_impact + cvss_integ_impact + cvss_avail_impact
Adherence =~ adherence
Outcomes =~ logCVECount
'

fit <- sem(model.zero.nvd.norules,data=rolled_software[rolled_software$CVECount > 1 & rolled_software$CVECount < 50,])
summary(fit,fit.measures=TRUE)


# NVD RESPECIFIED - THIS IS THE ONE FOR THE PAPER + constraints - Adherence
model.respecified.nvd <- '
SoftwareRisk =~ cvss_access_vector  + cvss_access_complexity + cvss_auth
AssetRisk =~  cvss_conf_impact + cvss_integ_impact + cvss_avail_impact
Adherence =~ adherence
Outcomes =~ logCVECount
Outcomes ~ SoftwareRisk + AssetRisk
SoftwareRisk ~  Adherence

AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
'
nvd_respecified_fit <- sem(model.respecified.nvd,data=rolled_software[rolled_software$CVECount > 1 & rolled_software$CVECount < 50,])
summary(nvd_respecified_fit,fit.measures=TRUE)

# generate figure
semPaths(nvd_respecified_fit,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)

# Manually save the above output as NVD_Respecified_SEM_Model.pdf


# Regressing cvss_access_complexity on adherence, controlling for everything else...
summary(lm(cvss_access_complexity ~ adherence + cvss_access_vector + cvss_auth + cvss_conf_impact + cvss_avail_impact + cvss_integ_impact,data=rtrunc))
# generating tex table for paper
stargazer(lm(logCVECount ~ cvss_access_complexity + adherence + cvss_access_vector + cvss_auth + cvss_conf_impact + cvss_avail_impact + cvss_integ_impact,data=rtrunc),style="asr")



nvdx <- gather(nvdxlated,variable=c(2,3,4,5,6,7,8,9))
dev.new()
ggplot(nvdx[nvdx$pubyear > 2000 & nvdx$key %in% c("cvss_access_vector","cvss_access_complexity","cvss_auth"),]) + aes(pubyear,value,group=key,linetype=key) + geom_smooth()
dev.new()
ggplot(nvdx[nvdx$pubyear > 2000 & nvdx$key %in% c("cvss_conf_impact","cvss_integ_impact","cvss_avail_impact"),]) + aes(pubyear,value,group=key,shape=key,linetype=key) + geom_smooth()



# CII

# Model zero cii

library(psych)



# fact_activity=actv2num(cii$fact_activity)
# fact_activity: Decreasing Y-O-Y development activity, Stable Y-O-Y development activity,  Increasing Y-O-Y development activity,
act2num <- function(v) { return(ifelse(v == "Decreasing Y-O-Y development activity",1,ifelse(v == "Stable Y-O-Y development activity",2,ifelse(v == "Increasing Y-O-Y development activity",3,0)))) }


# fact_age=age2num(cii$fact_age)
# fact_age: Decreasing Y-O-Y development activity, Stable Y-O-Y development activity,  Mature, well-established codebase
age2num <- function(v) { return(ifelse(v == "Short source control history",1,ifelse(v == "Young, but established codebase",2,ifelse(v == "Well-established codebase",3,ifelse(v == "Mature, well-established codebase",4,0))))) }


# comments
cmt2num <- function(v) {
	switch(v,
		"Very few source code comments" = 1,   
		"Few source code comments" = 2,
		"Average number of code comments" = 3,
		"Well-commented source code" = 4,  
		"Very well-commented source code" = 5
		)
}

cmt2num <- function(v) {
	return (ifelse(v == "Very few source code comments", 1,   
		ifelse(v == "Few source code comments", 2,
		ifelse(v == "Average number of code comments", 3,
		ifelse(v == "Well-commented source code",4,  
		ifelse(v == "Very well-commented source code", 5,-1
		))))))
}

# team_size 
tsz2num <- function(v) {
	return(
		ifelse(v == "No recent development activity", 0,
		ifelse(v == "Only a single active developer",1,   
		ifelse(v == "Small development team",2,
		ifelse(v == "Average size development team",3,
		ifelse(v == "Large, active development team", 4,  
		ifelse(v == "Very large, active development team", 5, -1
		)))))))
}


# ciicooked[,c("total_code_lines", "total_contributor_count","LanguageRisk", "PopularityRisk", "ExposureRisk", "direct_network_exposure", "process_network_data", "potential_privilege_escalation", "CVERisk", "CVE", "CVE_since_2010")]

ciiraw <- read.csv("~/Dropbox/github/cii-census/results.csv")

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


# Initial CII model
model.zero.cii <- '
SoftwareRisk =~ total_contributor_count + ContributorRisk + total_code_lines + LanguageRisk + direct_network_exposure + process_network_data + potential_privilege_escalation + CodeAge
AssetRisk =~ package_popularity
Outcomes =~ logCVECount 
Adherence =~ TeamActivity + CodeComments + twelve_month_contributor_count + WebsiteRisk
Outcomes ~ SoftwareRisk + AssetRisk
SoftwareRisk ~ Adherence

AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes'
fit <- sem(model.zero.cii,data=ciicooked)
summary(fit,fit.measures=TRUE)

# 
# Dropped constraints for CII model, transformed measurement variables to get variances in range
model.zero.cii.hacked <- '
SoftwareRisk =~ logContributorCount + ContributorRisk + logSLOC + LanguageRisk + direct_network_exposure + process_network_data + potential_privilege_escalation + CodeAge
AssetRisk =~ logPackagePopularity
Outcomes =~ logCVECount 
Adherence =~ TeamActivity + CodeComments  + WebsiteRisk
'
fit <- sem(model.zero.cii.hacked,data=ciicooked)
summary(fit,fit.measures=TRUE)


# First respecified CII model
model.respecified.cii <- '
SoftwareRisk =~ logContributorCount + ContributorRisk + logSLOC + LanguageRisk + direct_network_exposure + process_network_data + potential_privilege_escalation + CodeAge
AssetRisk =~ logPackagePopularity
Outcomes =~ logCVECount 
Adherence =~ TeamActivity + CodeComments  + WebsiteRisk
Outcomes ~ SoftwareRisk + AssetRisk
SoftwareRisk ~ Adherence

AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes'
fit <- sem(model.respecified.cii,data=ciicooked)
summary(fit,fit.measures=TRUE)

# Best fit attained for a respecified CII model
model.respecified.cii <- '
SoftwareRisk =~ logContributorCount + ContributorRisk + logSLOC  + direct_network_exposure + process_network_data + potential_privilege_escalation + CodeAge
AssetRisk =~ logPackagePopularity
Outcomes =~ logCVECount 
Adherence =~ TeamActivity + CodeComments + WebsiteRisk
Outcomes ~ SoftwareRisk + AssetRisk
SoftwareRisk ~ Adherence

AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes'
fit <- sem(model.respecified.cii,data=ciicooked)
summary(fit,fit.measures=TRUE)

ciiplist <- unique(levels(ciicooked$project_name))

# nvd[nvd$software %in% ciiplist,]
# rolled[rolled$software %in% ciiplist,]

rolledcii <- group_by(nvdxlated[nvdxlated$software %in% ciiplist,],software,pubyear,add=TRUE) %>% summarise(CVECount=n(),cvss_score=mean(cvss_score),adherence=mean(adherence),cvss_auth=mean(cvss_auth),cvss_access_vector=mean(cvss_access_vector),cvss_access_complexity=mean(cvss_access_complexity),cvss_conf_impact=mean(cvss_conf_impact), cvss_integ_impact=mean(cvss_integ_impact), cvss_avail_impact=mean(cvss_avail_impact),AuthenticationRisk=mean(AuthenticationRisk),AccessVectorRisk=mean(AccessVectorRisk),AccessComplexityRisk=mean(AccessComplexityRisk))

oldrolled <- rolled
rolled <- rolledcii

rolled_software_cii <- data.frame(baseCVECount=rolled$CVECount, CVECount=log(rolled$CVECount+1),cvss_score=rolled$cvss_score,adherence=scale(rolled$pubyear) + abs(min(scale(rolled$pubyear),na.rm=TRUE)),cvss_auth=rolled$cvss_auth,cvss_access_vector=rolled$cvss_access_vector,cvss_access_complexity=rolled$cvss_access_complexity,cvss_conf_impact=rolled$cvss_conf_impact, cvss_integ_impact=rolled$cvss_integ_impact, cvss_avail_impact=rolled$cvss_avail_impact,AuthenticationRisk=rolled$AuthenticationRisk,AccessVectorRisk=rolled$AccessVectorRisk,AccessComplexityRisk=rolled$AccessComplexityRisk)
 rolled_software_cii$logCVECount <- log(rolled_software_cii$CVECount+1)

# linear regression
summary(lm(
 CVECount ~ cvss_access_vector + cvss_access_complexity + cvss_auth + adherence
 + cvss_conf_impact + cvss_integ_impact + cvss_avail_impact
,data=rolled_software_cii))

# applying the NVD model to the CII projects
rolled_software_cii$logCVECount <- log(rolled_software_cii$CVECount+1)

# fit <- sem(model.zero.nvd,data=rolled_software_cii); summary(fit,fit.measures=TRUE)
# summary(fit,fit.measures=TRUE)
# semPaths(fit,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)

# DO YOU HAVE NVD LOADED?  PROBABLY NOT, YO


rolledcii_project <- group_by(nvdxlated[nvdxlated$software %in% ciiplist,],software,add=TRUE) %>% summarise(CVECount=n(),cvss_score=mean(cvss_score),adherence=mean(adherence),cvss_auth=mean(cvss_auth),cvss_access_vector=mean(cvss_access_vector),cvss_access_complexity=mean(cvss_access_complexity),cvss_conf_impact=mean(cvss_conf_impact), cvss_integ_impact=mean(cvss_integ_impact), cvss_avail_impact=mean(cvss_avail_impact))

nvdr2s <- group_by(nvdxlated,software) %>% summarise(CVECount=n(),cvss_score=mean(cvss_score),adherence=mean(adherence),cvss_auth=mean(cvss_auth),cvss_access_vector=mean(cvss_access_vector),cvss_access_complexity=mean(cvss_access_complexity),cvss_conf_impact=mean(cvss_conf_impact), cvss_integ_impact=mean(cvss_integ_impact), cvss_avail_impact=mean(cvss_avail_impact))

nvdr2sp <- data.frame(CVECount=nvdr2s$CVECount, logCVECount=log(nvdr2s$CVECount+1),cvss_score=nvdr2s$cvss_score,adherence=mean(nvdr2s$adherence),cvss_auth=nvdr2s$cvss_auth,cvss_access_vector=nvdr2s$cvss_access_vector,cvss_access_complexity=nvdr2s$cvss_access_complexity,cvss_conf_impact=nvdr2s$cvss_conf_impact, cvss_integ_impact=nvdr2s$cvss_integ_impact, cvss_avail_impact=nvdr2s$cvss_avail_impact)

clist <- ciicooked[ciicooked$project_name %in% nvdr2s$software,]$project_name
df <- data.frame()
for (pname in clist) {df <- rbind(df,cbind(nvdr2s[nvdr2s$software == pname,],ciicooked[ciicooked$project_name == pname,]))}


#
# CII - THIS IS THE ONE FOR THE PAPER
#
fit <- sem(model.zero.cii,data=df)
summary(fit,fit.measures=TRUE)

df$log12mcc <- log(df$twelve_month_contributor_count+1)
df$logCVE2010 <- log(df$CVE_since_2010+1)
df$logCVECount <- log(df$CVECount+1)
rolled$logCVECount <- log(rolled$CVECount+1)

# .08 RMSEA
model.combined <- '
 SoftwareRisk =~ cvss_access_vector + cvss_access_complexity + cvss_auth + LanguageRisk + total_code_lines + TeamSize 
 Outcomes =~  logCVECount 
 Outcomes ~ SoftwareRisk + AssetRisk
 Adherence =~ adherence + twelve_month_contributor_count + CodeComments + TeamActivity
 SoftwareRisk ~  Adherence
 AssetRisk =~ cvss_conf_impact + cvss_integ_impact + cvss_avail_impact'

fit <- sem(model.combined,data=df)
summary(fit,fit.measures=TRUE)

# lm comparison to above model
summary(lm(logCVECount  ~ cvss_access_vector + cvss_access_complexity + cvss_auth + total_code_lines + TeamSize 
 + adherence + twelve_month_contributor_count + CodeComments + TeamActivity + cvss_conf_impact + cvss_integ_impact + cvss_avail_impact,data=df))


# Comparison between levels for a fixed model

model.combinedl <- '
 SoftwareRisk =~ cvss_access_vector + cvss_access_complexity + cvss_auth + total_code_lines + TeamSize + LanguageRisk
 Outcomes =~  logCVECount 
 Outcomes ~ SoftwareRisk + AssetRisk
 Adherence =~ adherence + twelve_month_contributor_count + CodeComments + TeamActivity 
 SoftwareRisk ~  Adherence
 AssetRisk =~ cvss_conf_impact + cvss_integ_impact + cvss_avail_impact + package_popularity'


fit <- sem(model.combinedl,data=df[df$process_network_data==0,]); summary(fit,fit.measures=TRUE)

fit <- sem(model.combinedl,data=df[df$process_network_data==1,]); summary(fit,fit.measures=TRUE)



# How many NVD projects?
rolled <- group_by(nvdxlated,software,pubyear) %>% summarise(CVECount=n(),cvss_score=mean(cvss_score),adherence=mean(adherence),cvss_auth=mean(cvss_auth),cvss_access_vector=mean(cvss_access_vector),cvss_access_complexity=mean(cvss_access_complexity),cvss_conf_impact=mean(cvss_conf_impact), cvss_integ_impact=mean(cvss_integ_impact), cvss_avail_impact=mean(cvss_avail_impact),AuthenticationRisk=mean(AuthenticationRisk),AccessVectorRisk=mean(AccessVectorRisk),AccessComplexityRisk=mean(AccessComplexityRisk))
rolled_software <- data.frame(software=rolled$software,CVECount=rolled$CVECount, logCVECount=log(rolled$CVECount+1),cvss_score=rolled$cvss_score,adherence=scale(rolled$pubyear) + abs(min(scale(rolled$pubyear),na.rm=TRUE)),cvss_auth=rolled$cvss_auth,cvss_access_vector=rolled$cvss_access_vector,cvss_access_complexity=rolled$cvss_access_complexity,cvss_conf_impact=rolled$cvss_conf_impact, cvss_integ_impact=rolled$cvss_integ_impact, cvss_avail_impact=rolled$cvss_avail_impact,AuthenticationRisk=rolled$AuthenticationRisk,AccessVectorRisk=rolled$AccessVectorRisk,AccessComplexityRisk=rolled$AccessComplexityRisk)
length(as.character(unique(rolled_software[rolled_software$CVECount > 1 & rolled_software$CVECount < 50,]$software)))
# 6695


