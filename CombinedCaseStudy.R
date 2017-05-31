
# R packages we use 
library(lavaan)
library(semPlot)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(stargazer)

# Step 1: Collect OpenHub projects
# create mdddf


### WHAT FUNCTIONS DO YOU REALLY NEED?

setwd("/Users/admin/Dropbox/github/paper_modeling-sp/Draft")

nvd <- read.csv("~/Dropbox/NCSU/Spring2013/CSC720_AI/NVD_PGM_Project/nvdall2.csv",stringsAsFactors=FALSE)

mddraw <- read.csv("~/Dropbox/github/paper_modeling-sp/naggapan_2013_masterdata.txt",sep="\t",stringsAsFactors=FALSE)
# cbind(nvdr2s[nvdr2s$software == "abuse",],mddraw[mddraw$url_name %in% "abuse",])
mddplist <- unique(levels(mddraw$url_name))


nvdxlated <- data.frame(pub_year=nvd$pub_year,cve_id=nvd$cve_id,mfgr=nvd$mfgr,software=nvd$software,cwe_id=nvd$cwe_id)
nvdxlated$pubyear <- as.numeric(levels(nvdxlated$pub_year))[nvdxlated$pub_year]

rolled <- group_by(nvdxlated[nvdxlated$pubyear <= 2012,],software) %>% summarise(pubyear=max(pubyear),CVECount=n())
rolled_software <- data.frame(software=rolled$software,CVECount=rolled$CVECount,PubYear=rolled$pubyear)


# Step 2: add fields for other data sets
empty_nvd <- rolled_software[1,-c(1:3)]
empty_nvd[1,names(empty_nvd)] <- NA
empty_nvd$CVECount <- 0

# borrowed
trythat <- inner_join(mddraw,rolled_software[!is.na(rolled_software$software),],by=c("url_name" = "software"))
trythat$project_age <- (as.numeric(as.Date(trythat$max_month) - as.Date(trythat$min_month))/365)*12
trythat$DevAttention <- trythat$twelve_month_contributor_count / (trythat$code_churn_12months+1)
combined <-trythat[,c("total_code_lines","twelve_month_contributor_count","project_age","code_churn_12months","CVECount","DevAttention","user_count")]


# fresh
trythis <- left_join(mddraw,rolled_software,by=c("url_name" = "software"))

# you may need: str(trythis) 
trythis$adherence <- as.numeric(trythis$adherence)

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

tr$DevAttention <- tr$twelve_month_contributor_count.x / (tr$code_churn_12months+1)

length(tr[tr$isCII==TRUE,]$name)
# 57
length(tr[tr$isNVD==TRUE,]$name)
# 698


tr2scale <- tr[,c(5,16,6:8,13,14,23,47,49,50,51,52:63,65:100)]

nu <- cbind(tr2scale[tr2scale$isNVD==TRUE ,c(1,2,10,55,56)],apply(tr2scale[tr2scale$isNVD==TRUE ,-c(1,2,10,55,56)],2,scale))

nu$DevAttention <- nu$twelve_month_contributor_count.x / nu$code_churn_12months
nu$DevAttention <- scale(nu$DevAttention)


nu2 <- cbind(tr2scale[,c(1,2,10,55,56)],apply(tr2scale[,-c(1,2,10,55,56)],2,scale))
nu2$DevAttention <- nu2$twelve_month_contributor_count.x / nu2$code_churn_12months
nu2$DevAttention <- scale(nu2$DevAttention)
nu2$logUserCount <- log(nu2$user_count+1)


# Could this be the answer you’re looking for?
model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x + twelve_month_contributor_count.x + project_age + code_churn_12months#
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count'#
fit <- sem(model.combinedl,data=nu2); 
summary(fit,fit.measures=TRUE)


summary(lm(CVECount ~
total_code_lines.x + twelve_month_contributor_count.x + project_age + DevAttention + code_churn_12months + user_count,data=nu2))

summary(lm(CVECount ~
total_code_lines.x + twelve_month_contributor_count.x + project_age + DevAttention + code_churn_12months + user_count,data=nu2[nu2$isNVD==TRUE,]))

summary(lm(CVECount ~
total_code_lines.x + twelve_month_contributor_count.x + project_age + DevAttention + code_churn_12months + user_count,data=nu2[nu2$isCII==TRUE,]))

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


# trx <- tr[,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]
trx <- scale(tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")])


model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention + twelve_month_contributor_count.x#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
'#
fit2 <- sem(model.combinedl,data=trx); summary(fit2,fit.measures=TRUE)

semPaths(fit2,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)
anova(fit,fit2)

trx2 <- tr[tr$CVECount>0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]
trx2$total_code_lines.x <- log(trx2$total_code_lines.x+1)
trx2$code_churn_12months <- log(trx2$code_churn_12months+1)
trx2$twelve_month_contributor_count.x <- log(trx2$twelve_month_contributor_count.x+1)
trx3 <- data.frame(scale(trx2))

model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention + twelve_month_contributor_count.x#
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


model.combinedl <- '#
SoftwareRisk =~ cvss_access_vector + cvss_access_complexity + cvss_auth + logSLOC + LanguageRisk + log12mcc#
Outcomes =~  logCVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ adherence  + CodeComments + TeamActivity #
SoftwareRisk ~  Adherence#
AssetRisk =~ cvss_conf_impact + cvss_integ_impact + cvss_avail_impact + logPackagePopularity'#
fit <- sem(model.combinedl,data=df2)
summary(fit,fit.measures=TRUE)

# Don't forget to clip only above the cut/paste

# OpenHub model baseCVECountmodel.combinedl <- '#
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months + twelve_month_contributor_count.x
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count + project_age
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
'#
fitX <- sem(model.combinedl,data=scale(tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")])); summary(fitX,fit.measures=TRUE)

# OpenHub model reestimated
model.combinedl <- '#
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months 
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count + project_age
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
'#
fitX <- sem(model.combinedl,data=scale(tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")])); summary(fitX,fit.measures=TRUE)


model.combinedl <- 'code_churn_12months ~~ user_count
SoftwareRisk =~ total_code_lines.x  + project_age + code_churn_12months + twelve_month_contributor_count.x
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count + project_age
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
total_code_lines.x  ~~ code_churn_12months
user_count ~~ twelve_month_contributor_count.x'#
fitX <- sem(model.combinedl,data=scale(tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]),test = "Satorra-Bentler"); summary(fitX,fit.measures=TRUE)

# "For the paper" (like you always say)
tr_paper <- tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]
names(tr_paper)[names(tr_paper)=="total_code_lines.x"] <- "total_code_lines"
names(tr_paper)[names(tr_paper)=="twelve_month_contributor_count.x"] <- "twelve_month_contributor_count"
names(tr_paper)[names(tr_paper)=="DevAttention"] <- "DevAttention"

tr_paper <- tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]
names(tr_paper)[names(tr_paper)=="total_code_lines.x"] <- "total_code_lines"
names(tr_paper)[names(tr_paper)=="twelve_month_contributor_count.x"] <- "twelve_month_contributor_count"
names(tr_paper)[names(tr_paper)=="DevAttention"] <- "DevAttention"

tr_paper_nvd <- tr[tr$CVECount>0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]
names(tr_paper_nvd)[names(tr_paper_nvd)=="total_code_lines.x"] <- "total_code_lines"
names(tr_paper_nvd)[names(tr_paper_nvd)=="twelve_month_contributor_count.x"] <- "twelve_month_contributor_count"
names(tr_paper_nvd)[names(tr_paper_nvd)=="DevAttention"] <- "DevAttention"

png('combined_pairs.png')
pairs(tr_paper)
dev.off()

skewness(scale(tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]))

kurtosis(scale(tr[tr$CVECount>=0,c("total_code_lines.x","twelve_month_contributor_count.x","project_age","code_churn_12months","CVECount","DevAttention","user_count")]))

model.combinedl <- '
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
total_code_lines  ~~ code_churn_12months'#
fitX <- sem(model.combinedl,data=scale(tr_paper,center=FALSE),estimator="MLR",test = "Satorra-Bentler"); summary(fitX,fit.measures=TRUE)

semPaths(fitX,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)

stargazer(round(residuals(fitX)$cov,2),style="asr",digits=2)


# Yet another paper try, tr taken from trythat
model.combinedl <- '
project_age ~~ code_churn_12months
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
DevAttention ~~ DevAttention
total_code_lines ~~ code_churn_12months'; 
fitX <- sem(model.combinedl,data=scale(log(combined+1)),test = "Satorra-Bentler"); summary(fitX,fit.measures=TRUE)

semPaths(fitX,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)
combined <-tr[tr$main_language_name=="Java",c("total_code_lines","twelve_month_contributor_count","project_age","code_churn_12months","CVECount","DevAttention","user_count")]

# Latest base model
model.combinedl <- '

SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
';
fitX <- sem(model.combinedl,data=scale(log(combined+1)),test = "Satorra-Bentler"); summary(fitX,fit.measures=TRUE)

# Model from Santiago-Buenos Aires flight 
model.combinedl <- 'project_age ~~            code_churn_12months
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
DevAttention ~~ DevAttention
total_code_lines ~~            code_churn_12months'; 
fitX <- sem(model.combinedl,data=scale(log(combined+1))); summary(fitX,fit.measures=TRUE)
semPaths(fitX,,"std",title=FALSE,sizeLat=10,sizeMan=9,nCharNodes=25,residuals=TRUE,intercepts=FALSE,layout="tree2",structural=FALSE)

# Most recent decent model, pre-DevAttention relabeling
model.combinedl <- '
DevAttention ~~ DevAttention
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes

total_code_lines ~~   code_churn_12months 
twelve_month_contributor_count ~~ code_churn_12months'; 
fitX <- sem(model.combinedl,data=scale(log(combined+1))); summary(fitX,fit.measures=TRUE)



# whoa
model.combinedl <- '
DevAttention ~~ DevAttention
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes

total_code_lines ~~ project_age
total_code_lines ~~ code_churn_12months 
total_code_lines ~~ twelve_month_contributor_count
code_churn_12months ~~ twelve_month_contributor_count
';
fitX <- sem(model.combinedl,data=scale(combined),fixed.x=FALSE,test = "Satorra-Bentler"); summary(fitX,fit.measures=TRUE)


# most complicated

model.combinedl <- 'total_code_lines ~~ total_code_lines
DevAttention ~~ DevAttention
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes
twelve_month_contributor_count ~~ twelve_month_contributor_count
total_code_lines ~~ project_age
total_code_lines ~~ code_churn_12months 
total_code_lines ~~ twelve_month_contributor_count
code_churn_12months ~~ twelve_month_contributor_count
code_churn_12months ~~ code_churn_12months';
fitX <- sem(model.combinedl,data=scale(log(combined+1)),fixed.x=FALSE); summary(fitX,fit.measures=TRUE)

# As of 5/30 at 8:13pm, this is the one
model.combinedl <- '
DevAttention ~~ DevAttention
SoftwareRisk =~ total_code_lines  + project_age + code_churn_12months + twelve_month_contributor_count
Outcomes =~  CVECount #
Outcomes ~ SoftwareRisk + AssetRisk#
Adherence =~ DevAttention#
SoftwareRisk ~  Adherence#
AssetRisk =~ user_count 
AssetRisk ~~  Adherence
SoftwareRisk ~~ 0*AssetRisk
Adherence ~~ 0*Outcomes

total_code_lines ~~  code_churn_12months 
twelve_month_contributor_count ~~ code_churn_12months'; 
fitX <- sem(model.combinedl,data=scale(log(combined+1)),test = "Satorra-Bentler"); summary(fitX,fit.measures=TRUE)


# standard evaluation package
inspect(fitX,"r2")
residuals(fitX)


