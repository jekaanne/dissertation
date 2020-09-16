library(here) #Sets working directory
library(renv) #Package management by RStudio
library(data.table) #Smart data frames
library(tidyverse) #Packages for tidy data
library(Gmisc) #Descriptive statistics tables
library(sjPlot) #Spearman correlation matrix function 
library(rmarkdown) #Dynamic document creator
library(pander) #Pandoc writer
library(broom) #Tidy up statistical objects
library(kableExtra) #For complex tables
library(Matrix) #Various matrix options
library(car) #Companion to Applied Regression book (ncvTest)
library(psych) #Basic data functions
#Confirmatory Fcctor Analysis
library(MVN) #Test for multivariate normality
library(ICS) #Tools for exploring multivariate data
library(lavaan) #LAtent VAriable ANalysis
library(semTools) #Tools for SEM
library(semPlot) #Plot SEM models
library(MASS) #Support functions for lme and mediation packages
library(Hmisc) #Functions for data analysis, graphics, utility operations, functions for computing sample size and power

# Demographic Data Tables ####
# Table 7 demographic profiles
htmlTable::setHtmlTableTheme(css.rgroup = "")
label(ardraw2$gender) <- "Gender"
label(ardraw2$white) <- "Race/Ethnicity"
label(ardraw2$education) <- "Education"
label(ardraw2$income_range) <- "Income Range"
label(ardraw2$emp_ft) <- "Full-time"
label(ardraw2$emp_pt) <- "Part-time"
label(ardraw2$emp_self) <- "Self-Employed"
label(ardraw2$emp_student) <- "Student"
label(ardraw2$emp_unemp) <- "Unemployed"
label(ardraw2$emp_retired) <- "Retired"

getTable1Stats <- function(x, digits = 0, prop_fn = describeProp, total_col_show_perc = TRUE){
  getDescriptionStatsBy(x = x, by = ardraw2$disability_status)
}

t1 <- list()
t1[["Gender"]] <- getTable1Stats(ardraw2$gender)
t1[["Race/Ethnicity"]] <- getTable1Stats(ardraw2$white)
t1[["Education"]] <- getTable1Stats(ardraw2$education)
t1[["Income Range"]] <- getTable1Stats(ardraw2$income_range)
t1[["Full Time"]] <- table(ardraw2$emp_ft, ardraw2$disability_status)
t1[["Part Time"]] <- table(ardraw2$emp_pt, ardraw2$disability_status)
t1[["Self-Employed"]] <- table(ardraw2$emp_self, ardraw2$disability_status)
t1[["Student"]] <- table(ardraw2$emp_student, ardraw2$disability_status)
t1[["Unemployed"]] <- table(ardraw2$emp_unemp, ardraw2$disability_status)
t1[["Retired"]] <- table(ardraw2$emp_retired, ardraw2$disability_status)
mergeDesc(t1, htmlTable_args = list(caption  = "Participant demographic profiles"))

# create scale tables and calculate means ####
# complete subscales survey 1
ardraw2factors <- dplyr::select(ardraw2, aut1, aut2, aut3, aut4, aut5, aut6, 
                 rel1, rel2, rel3, rel4, rel5, rel6,
                 com1, com2, com3, com4,
                 trans_pac1, trans_pac2, trans_pac3, trans_pac4, 
                 gse1, gse2, gse3, gse4,  
                 disc1, disc2, disc3, disc4)
ttbpn2 <- dplyr::select(ardraw2, aut1, aut2, aut3, aut4, aut5, aut6, 
                 rel1, rel2, rel3, rel4, rel5, rel6,
                 com1, com2, com3, com4)
aut2 <- dplyr::select(ardraw2factors, aut1, aut2, aut3, aut4, aut5, aut6)
rel2 <- dplyr::select(ardraw2factors, rel1, rel2, rel3, rel4, rel5, rel6)
com2 <- dplyr::select(ardraw2factors, com1, com2, com3, com4)
pac2 <- dplyr::select(ardraw2factors, trans_pac1, trans_pac2, trans_pac3, trans_pac4)
gse2 <- dplyr::select(ardraw2factors, gse1, gse2, gse3, gse4)
disc2 <- dplyr::select(ardraw2factors, disc1, disc2, disc3, disc4)
altbpnf2 <-  dplyr::select(ardraw2factors, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  disc1, disc2, disc3, disc4)

# scale means for correlation table
subscale_means2 <- select(ardraw2, autmean, relmean, commean, pacmean, discrmean, gsemean)

#overall score correlation matrix
tab_corr(subscale_means2, na.deletion = c("listwise"),
         corr.method = c("spearman"),)

# Table 8-9 descriptive table of items
psych::describe(ttbpn2)
psych::describe(pac2)
psych::describe(gse2)
psych::describe(discr2)
# test scale alphas  #####
psych::alpha(aut2, use = "pairwise.complete.obs")
psych::alpha(rel2, use = "pairwise.complete.obs")
psych::alpha(com2, use = "pairwise.complete.obs")
psych::alpha(pac2, use = "pairwise.complete.obs")
psych::alpha(gse2, use = "pairwise.complete.obs")
psych::alpha(discr2, use = "pairwise.complete.obs")

# assumptions/normality ####

# checking for multivariate normal #
mvn(ttbpn2, mvnTest = c("mardia"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE, desc = TRUE, transform = "none", univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"), univariatePlot = "none", multivariatePlot = "none",  multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded", showOutliers = TRUE, showNewData = TRUE)
scatterplotMatrix(subscale_means[7:9])
           #univariate plots
result <- mvn(data = ttbpn, mvnTest = "royston", univariatePlot = "histogram")

mvn(altbpnf2, mvnTest = c("mardia", "hz", "royston", "dh", "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE, desc = TRUE, transform = "none", univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"), univariatePlot = "none", multivariatePlot = "none",  multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded", showOutliers = TRUE, showNewData = TRUE)

# 3-factor TTBPN model baseline with FIML ####
# Table 10a 
threefactor2.cfa <- '
aut=~aut1 + aut2+ aut3+ aut4+ aut5+ aut6
rel=~rel1+  rel2+ rel3+ rel4+ rel5+ rel6 
com=~com1+ com2+ com3+ com4
aut~~aut
rel~~rel
com~~com
aut~~rel
aut~~com
rel~~com
'
threefactor2.fit <- cfa(threefactor2.cfa, data = ardraw2, sample.nobs = 211, estimator = "MLR", missing = "FIML", std.lv=TRUE)
# print summary w/ fit statistics
summary(threefactor2.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor2.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor2.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor2.residuals <- resid(threefactor2.fit, type = "standardized")
# covariance of residuals
vcov(threefactor2.fit)
# print Table of Factor loadings
threefactor2param <- parameterEstimates(threefactor2.fit, standardized=TRUE) %>% filter(op == "=~") %>% dplyr::select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% kable(digits = 3, format="pandoc", caption="Factor Loadings")
threefactor2param

# 3- factor modified TTBPN model ####
threefactor2mod.cfa <- '
aut=~a*aut2 + b*aut3+ c*aut5+ d*aut6
rel=~e*rel2 + f*rel3 + g*rel4+ h*rel6 
com=~i*com1 + j*com2+ k*com3+ l*com4
aut~~rel
aut~~com
rel~~com
rel2~~rel3
com3~~com4
'
threefactor2mod.fit <- cfa(threefactor2mod.cfa, data = ardraw2, sample.nobs = 211, estimator = "MLR", missing = "FIML", std.lv=TRUE)
# print summary w/ fit statistics
# Table 13a
summary(threefactor2mod.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor2mod.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor2mod.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor2mod.residuals <- resid(threefactor2mod.fit, type = "standardized")
# covariance of residuals
vcov(threefactor2mod.residuals)

# print Table of Factor loadings
# Table 11
parameterEstimates(threefactor2mod.fit, ci = TRUE, remove.nonfree = TRUE,  standardized=TRUE) %>%  filter(op == "=~") %>% dplyr::select('Indicator'=rhs, 'Beta'=std.all, 'SE'=se, 'Z'=z, 'CI.Lower'=ci.lower, 'CI.Upper'=ci.upper) %>% 
kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")

# Figure 3 
semPaths(threefactor2mod.fit, what="paths", whatLabels="par", rotation = 1, label.prop=1.75, edge.label.color = "black", edge.width = 1, shapeMan = "rectangle", shapeLat = "ellipse", sizeMan = 5, sizeLat = 5,  sizeLat2 = 5, sizeInt = 3, sizeInt2 = 3, curve=1, intercepts = TRUE, edge.label.cex = 1.75, cardinal = FALSE, style ="lisrel", residuals = TRUE, repulsion = .2, curvePivotShape = .5)

# 3-factor ALT-bpnf model with other scales ####
# Table 10b 
alt.bpnf.threefactor2.cfa <- '
pac=~a*trans_pac1 + b*trans_pac2 + c*trans_pac3 + d*trans_pac4
gse=~e*gse1 +f*gse2 + f*gse3 +  h*gse4
discr=~i*disc1 + j*disc2+ k*disc3 + l*disc4
pac~~gse
pac~~discr
gse~~discr
'
alt.bpnf.threefactor2.fit <- cfa(alt.bpnf.threefactor2.cfa, data = ardraw2, sample.nobs = 211, estimator = "MLR", std.lv=TRUE)
# print summary w/ fit statistics
# Table 13b
summary(alt.bpnf.threefactor2.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(alt.bpnf.threefactor2.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(alt.bpnf.threefactor2.fit, what = "std")$lambda
# check residuals (want value < .1)
alt.bpnf.threefactor2.residuals <- resid(bpn.threefactor2.fit, type = "standardized")
# covariance of residuals
vcov(alt.bpnf.threefactor2.fit)
# Table 11 print Table of Factor loadings
parameterEstimates(alt.bpnf.threefactor2.fit, ci = TRUE, remove.nonfree = TRUE,  standardized=TRUE) %>% filter(op == "=~") %>%  dplyr::select('Indicator'=rhs, 'Beta'=std.all, 'SE'=se, 'Z'=z,  'CI.Lower'=ci.lower,'CI.Upper'=ci.upper) %>% 
kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")

# Figure 2
semPaths(alt.bpnf.threefactor2.fit, what="paths", whatLabels="par", rotation = 1, label.prop=1.75, edge.label.color = "black", edge.width = 1, shapeMan = "rectangle", shapeLat = "ellipse", sizeMan = 5, sizeLat = 5,  sizeLat2 = 5, sizeInt = 3, sizeInt2 = 3, curve=.75, intercepts = TRUE, edge.label.cex = 1.75, cardinal = FALSE, style ="lisrel", residuals = TRUE, repulsion = .2, curvePivotShape = .5)

# TTBPN scale validity measures####
# TTBPN indicator and composite reliability #
#select indicators for modified model 
ttbpnmod2 <- dplyr::select(ardraw2, aut2, aut3, aut5, aut6, rel2, rel3, rel4, rel6, com1, com2, com3, com4)

# Define the model
ttbpnmod2.mod <- '
aut=~a*aut2 + b*aut3+ c*aut5+ d*aut6
rel=~e*rel2 + f*rel3 + g*rel4+ h*rel6 
com=~i*com1 + j*com2+ k*com3+ l*com4
'
# Fit the model
ttbpnmod2.fit <- cfa(ttbpnmod2.mod, data = ardraw2, estimator = "MLR", WLS.V = TRUE, std.lv=TRUE)
sl <- standardizedSolution(ttbpnmod2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- lavInspect(ttbpnmod2.fit, what="std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)
library(semTools)
reliability(ttbpnmod2.fit)
discriminantValidity(ttbpnmod2.fit, cutoff = 0.9, merge = FALSE, level = 0.95)
htmt(ttbpnmod2.mod, data = ttbpnmod2, sample.cov = NULL, missing = "listwise", ordered = NULL, absolute = TRUE)

#subscale aut CR calculation
#select final aut vars 
altaut2 <- dplyr::select(aut2, aut2, aut3, aut5, aut6 )
aut2.model <- ' aut  =~ aut2 + aut3+ aut5+ aut6'
aut2.fit <- cfa(aut2.model, data = ttbpnmod2)
sl <- standardizedSolution(aut2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altaut2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for aut
ave2.aut <- (sum(sl^2))/6
ave2.aut
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(aut2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#rel CR calculation 
#select final rel vars 
altrel2 <- dplyr::select(rel2, rel2, rel3, rel4, rel6)
rel2.model <- ' rel  =~ rel2 + rel3 + rel4+ rel6'
rel2.fit <- cfa(rel2.model, data = ttbpnmod2)
sl <- standardizedSolution(rel2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altrel2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
ave2.rel <- (sum(sl^2))/6
ave2.rel
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(rel2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#com CR calculation 
com2 <- dplyr::select(rel2, rel2, rel3, rel4, rel6)
com2.model <- ' com  =~ com1 + com2 + com3 + com4'
com2.fit <- cfa(com2.model, data = ttbpnmod2)
sl <- standardizedSolution(com2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(com2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for com
ave2.com <- (sum(sl^2))/6
ave2.com
# Compute composite comiability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(com2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# compute final 3-factor model validity
cr2.threef.fit <- cfa(ttbpnmod2.mod, data = ttbpnmod2)
sl <- standardizedSolution(cr2.threef.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod2)
# Compute residual variance of each item
re <- 1 - sl^2
# Compute complete final scale composite reliability
cr.threef2 <- sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(cr2.threef.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# create mean correlation table and test for discriminant validity by squaring correlation
ttbpn.factors <- ardraw2factors[, c("autmean", "relmean", "commean")]
corr.ttbpn2 <- cor(ttbpnmod2, use = "pairwise.complete.obs")
sq.corr.ttbpn2 <- corr.ttbpn2^2
sq.corr.ttbpn2




# ALT-BPNF scale validity measures for final ####
# Define the model
alt.bpnf2.model <- '
pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4
gse=~gse1 +gse2 + gse3 +  gse4
discr=~disc1 + disc2+ disc3 + disc4
pac~~gse
pac~~discr
gse~~discr
'
# Fit the model
alt.bpnf2.fit <- cfa(alt.bpnf2.model, data = altbpnf2)
sl <- standardizedSolution(alt.bpnf2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altbpnf2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(alt.bpnf2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#pac CR calculation
pac.model <- 'pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4'
pac.fit <- cfa(pac.model, data = altbpnf2)
sl <- standardizedSolution(pac.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(pac)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for aut
ave.pac <- (sum(sl^2))/6
ave.pac
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(pac.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#gse CR calculation 
gse.model <- 'gse=~gse1 +gse2 + gse3 +  gse4'
gse.fit <- cfa(gse.model, data = gse2)
sl <- standardizedSolution(gse.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(gse)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
alt.ave.gse <- (sum(sl^2))/6
alt.ave.gse
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(gse.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#discr CR calculation 
discr.model <- '
discr=~discr1 + discr2+ discr3 + discr4'
discr.fit <- cfa(discr.model, data = discr2)
sl <- standardizedSolution(discr.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(discr)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for discr
ave.discr <- (sum(sl^2))/6
ave.discr
# Compute composite comiability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(discr.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#htmt for discriminant validity
htmt(alt.bpnf2.model, data = ardraw2, sample.cov = NULL, missing = "listwise", ordered = NULL, absolute = TRUE)





# Measurement Invariance for TTBPN final model ####
# Table 14
# see https://rstudio-pubs-static.s3.amazonaws.com/194879_192b64ad567743d392b559d650b95a3b.html for instructions 
fit.indices <- c("chisq", "df", "rmsea", "tli", "cfi", "aic")

groups.model <- '
aut=~a*aut2 + b*aut3+ c*aut5+ d*aut6
rel=~e*rel2 + f*rel3 + g*rel4+ h*rel6 
com=~i*com1 + j*com2+ k*com3+ l*com4
aut~~rel
aut~~com
com~~rel
rel2~~rel3
com3~~com4
aut2~~aut6
aut3~~aut6
'

groups.baseline<-lavaan::cfa(groups.model, data=ardraw2, sample.nobs = 211, estimator = "MLR", meanstructure = TRUE, std.lv=TRUE, missing = "FIML")

fitMeasures(groups.baseline, fit.indices)
modindices(groups.baseline, sort. = TRUE)

configural <- cfa(groups.model, data=ardraw2, group = "disability_status", sample.nobs = 211, estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
fitMeasures(configural, fit.indices)
weak.invariance <- cfa(groups.model, data=ardraw2, group = "disability_status", group.equal = "loadings", sample.nobs = 211, estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
fitMeasures(weak.invariance,  fit.indices)

anova(weak.invariance, configural)
fit.stats <- rbind(fitmeasures(configural, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")), fitmeasures(weak.invariance, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural", "weak invariance")
fit.stats
strong.invariance <- cfa(groups.model, data=ardraw2, group = "disability_status", group.equal = c( "loadings", "intercepts"), sample.nobs = 211,  estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
summary(strong.invariance, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)

anova(strong.invariance, weak.invariance)

fit.stats <- rbind(fit.stats, fitmeasures(strong.invariance, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")))
rownames(fit.stats)[3:3] <- c("strong")
round(fit.stats, 3)

# alt-bpn measurement invariance
altgroups.model <- '
pac=~a*trans_pac1 + b*trans_pac2 + c*trans_pac3 + d*trans_pac4
gse=~e*gse1 +f*gse2 + f*gse3 +  h*gse4
discr=~i*disc1 + j*disc2+ k*disc3 + l*disc4
pac~~gse
pac~~discr
gse~~discr
'

altgroups.baseline<-lavaan::cfa(altgroups.model, data=ardraw2, sample.nobs = 211, estimator = "MLR")
fitMeasures(altgroups.baseline, fit.indices)
modindices(altgroups.baseline, sort. = TRUE)


configural <- cfa(altgroups.model, data=ardraw2, group = "disability_status", sample.nobs = 211,  estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
fitMeasures(configural, fit.indices)
weak.invariance <- cfa(altgroups.model, data=ardraw2, group = "disability_status", group.equal = "loadings", sample.nobs = 211,  estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
fitMeasures(weak.invariance,  fit.indices)
anova(weak.invariance, configural)
fit.stats <- rbind(fitmeasures(configural, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")), fitmeasures(weak.invariance, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural", "weak invariance")
fit.stats
strong.invariance <- cfa(altgroups.model, data=ardraw2, group = "disability_status", group.equal = c( "loadings", "intercepts"), sample.nobs = 211,  estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
summary(strong.invariance, fit.measures = TRUE)

anova(strong.invariance, weak.invariance)

#partial invariance test
lavTestScore(strong.invariance)
partable.alt <- parTable(strong.invariance)
strong.invariance2 <- cfa(altgroups.model, data=ardraw2, group = "disability_status", group.equal = c( "loadings", "intercepts"), group.partial=c("disc3~1"), sample.nobs = 211,  estimator = "MLR", std.lv=TRUE, meanstructure = TRUE, missing = "FIML")
anova(strong.invariance2, weak.invariance)
summary(strong.invariance, fit.measures = TRUE)

fit.stats <- rbind(fit.stats, fitmeasures(strong.invariance, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")), fitmeasures(strong.invariance2, fit.measures = c("chisq", "df", "rmsea", "tli", "cfi", "aic")))
rownames(fit.stats)[3:4] <- c("strong", "strong w disc3")
round(fit.stats, 3)