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
library(ICS) 
library(car) #Companion to Applied Regression book (ncvTest)
library(psych) #Basic data functions
library(MVN) #Test for multivariate normality
library(lavaan) #LAtent VAriable ANalysis
library(semTools) #Tools for SEM
#library(semPlot) #Plot SEM models
library(MASS) #Support functions for lme and mediation packages
library(Hmisc) #Functions for data analysis, graphics, utility operations, functions for computing sample size and power




# Demographic Data Tables ####
htmlTable::setHtmlTableTheme(css.rgroup = "")
label(ardraw$gender) <- "Gender"
label(ardraw$white) <- "Race/Ethnicity"
label(ardraw$education) <- "Education"
label(ardraw$income_range) <- "Income Range"
label(ardraw$emp_status) <- "Employment Status"
getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, by = ardraw$disability_status)
}
t1 <- list()
t1[["Gender"]] <- getTable1Stats(ardraw$gender)
t1[["Race/Ethnicity"]] <- getTable1Stats(ardraw$white)
t1[["Education"]] <- getTable1Stats(ardraw$education)
t1[["Income Range"]] <- getTable1Stats(ardraw$income_range)
t1[["Employment Status"]] <- getTable1Stats(ardraw$emp_status)
mergeDesc(t1, htmlTable_args = list(caption  = "Participant demographic profiles"))


# create scale tables and calculate means ####
ardrawfactors <- dplyr::select(ardraw, aut1, aut2, aut3, aut4, aut5, aut6, 
                    rel1, rel2, rel3, rel4, rel5, rel6,
                    com1, com2, com3, com4, com5, com6,
                    aut_f1, aut_f2, aut_f3, aut_f4, 
                    rel_f1, rel_f2, rel_f3, rel_f4, 
                    com_f1, com_f2, com_f3, com_f4,
                    )
# complete cases only
ardrawfactors <- ardrawfactors[complete.cases(ardrawfactors),]
# complete subscales survey 1
ttbpn <- dplyr::select(ardrawfactors, aut1, aut2, aut3, aut4, aut5, aut6, 
                    rel1, rel2, rel3, rel4, rel5, rel6,
                    com1, com2, com3, com4, com5, com6)
aut <- dplyr::select(ardrawfactors, aut1, aut2, aut3, aut4, aut5, aut6)
rel <- dplyr::select(ardrawfactors, rel1, rel2, rel3, rel4, rel5, rel6)
com <- dplyr::select(ardrawfactors, com1, com2, com3, com4, com5, com6)
bpnf <- dplyr::select(ardrawfactors, aut_f1, aut_f2, aut_f3, aut_f4, 
                rel_f1, rel_f2, rel_f3, rel_f4, 
                com_f1, com_f2, com_f3, com_f4)
autf <- dplyr::select(ardrawfactors, aut_f1, aut_f2, aut_f3, aut_f4)
relf <- dplyr::select(ardrawfactors, rel_f1, rel_f2, rel_f3, rel_f4)
comf <- dplyr::select(ardrawfactors, com_f1, com_f2, com_f3, com_f4)

#altbpn measures
altbpnfactors <- dplyr::select(ardraw, aut1, aut2, aut3, aut4, aut5, aut6, rel1, rel2, rel3, rel4, rel5, rel6,
                        com1, com2, com3, com4, com5, com6,
                        aut_f1, aut_f2, aut_f3, aut_f4, 
                        rel_f1, rel_f2, rel_f3, rel_f4, 
                        com_f1, com_f2, com_f3, com_f4, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  dis_identity1, dis_identity2, dis_identity3, dis_identity4)
# complete cases only
altbpnfactors <- altbpnfactors[complete.cases(altbpnfactors),]




# mean-score-calculations removing missing values for TTBPN and BPNF
ardrawfactors$ttbpnmean <- rowMeans(ttbpn, na.rm = TRUE)
ardrawfactors$autmean <- rowMeans(aut, na.rm = TRUE)
ardrawfactors$relmean <- rowMeans(rel, na.rm = TRUE)
ardrawfactors$commean <- rowMeans(com, na.rm = TRUE)
ardrawfactors$autfmean <- rowMeans(autf, na.rm = TRUE)
ardrawfactors$relfmean <- rowMeans(relf, na.rm = TRUE)
ardrawfactors$comfmean <- rowMeans(comf, na.rm = TRUE)
ardrawfactors$bpnfmean <- rowMeans(bpnf, na.rm = TRUE)
ardraw$flourmean <- rowMeans(flour, na.rm = TRUE)
ardraw$ttbpnmean <- rowMeans(ttbpn, na.rm = TRUE)


#alt-bpn complete cases only
altbpnfactors <- altbpnfactors[complete.cases(altbpnfactors),]
# mean-score-calcualtins removing missing values for ALT-BPNF
altbpnfactors$autmean <- rowMeans(altaut, na.rm = TRUE)
altbpnfactors$relmean <- rowMeans(altrel, na.rm = TRUE)
altbpnfactors$commean <- rowMeans(altcom, na.rm = TRUE)
altbpnfactors$autfmean <- rowMeans(altautf, na.rm = TRUE)
altbpnfactors$relfmean <- rowMeans(altrelf, na.rm = TRUE)
altbpnfactors$comfmean <- rowMeans(altcomf, na.rm = TRUE)
altbpnfactors$bpnfmean <- rowMeans(bpnf, na.rm = TRUE)
altbpnfactors$pacmean <- rowMeans(pac, na.rm = TRUE)
altbpnfactors$gsemean <- rowMeans(gse, na.rm = TRUE)
altbpnfactors$disidmean <- rowMeans(disid, na.rm = TRUE)

# scale means for correlation table
subscale_means <- altbpnfactors[, c("autmean", "relmean", "commean", "autfmean", "relfmean", "comfmean", "pacmean", "disidmean", "gsemean")]
#overall score correlation matrix
tab_corr(subscale_means, na.deletion = c("listwise"),
         corr.method = c("spearman"),)
# create descriptive table of items
psych::describe(ttbpn)
psych::describe(bpnf)
psych::describe(pac)
psych::describe(gse)
psych::describe(disid)
# test scale alphas  #####
psych::alpha(aut, use="pairwise.complete.obs")
psych::alpha(rel, use="pairwise.complete.obs")
psych::alpha(com, use="pairwise.complete.obs")
psych::alpha(bpn_f, use="pairwise.complete.obs")
psych::alpha(pac, use="pairwise.complete.obs")
psych::alpha(gse, use = "pairwise.complete.obs")
psych::alpha(disid, use = "pairwise.complete.obs")
psych::alpha(flour, use = "pairwise.complete.obs")

# 1-factor standardized model w/ MLR ####
onefactor.cfa <- '
ttbpn=~ a*aut1 + b*aut2 + c*aut3 + d*aut4 + e*aut5 + f*aut6 +g*rel1 + h*rel2 + i*rel3 + j*rel4 + k*rel5 + l*rel6 + m*com1 + n*com2 + o*com3 + p*com4 + q*com5 + r*com6
'
onefactor.fit <- cfa(onefactor.cfa, data = ardraw, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(onefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(onefactor.fit, sort.=TRUE, minimum.value=3)
# check loading factors on their own
inspect(onefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
onefactor.residuals <- resid(onefactor.fit, type = "standardized")
# covariance of residuals
vcov(onefactor.fit)

lavaanPlot(model = onefactor.fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE)



# 3-factor TTBPN model baseline with MLR ####
threefactor.cfa <- '
aut=~aut1 + aut2+ aut3+ aut4+ aut5+ aut6
rel=~rel1+  rel2+ rel3+ rel4+ rel5+ rel6 
com=~com1+ com2+ com3+ com4 + com5 + com6
aut~~rel
aut~~com
rel~~com
'
threefactor.fit <- cfa(threefactor.cfa, data = ardraw, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(threefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor.residuals <- resid(threefactor.fit, type = "standardized")
# covariance of residuals
vcov(threefactor.fit)


# chisq test of models 
anova(onefactor.fit, threefactor.fit )

# 3- factor modified TTBPN model ####
threefactormod.cfa <- '
aut=~a*aut3 + b*aut4+ c*aut5+ d*aut6
rel=~e*rel1+ f*rel2+ g*rel3+ h*rel4 
com=~i*com1 + j*com2+ k*com3+ l*com6
aut~~aut
rel~~rel
com~~com
rel1 ~~ rel2
rel2 ~~ rel4
aut3 ~~ aut5
com3 ~~ com6
'
threefactormod.fit <- cfa(threefactormod.cfa, data = ardraw, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR", std.lv=TRUE)
# print summary w/ fit statistics
summary(threefactormod.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactormod.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactormod.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactormod.residuals <- resid(threefactormod.fit, type = "standardized")
# covariance of residuals
vcov(threefactormod.fit)
# print Table of Factor loadings
parameterEstimates(threefactormod.fit, ci = TRUE, remove.nonfree = TRUE,  standardized=TRUE) %>% filter(op == "=~") %>% dplyr::select("Indicator"=rhs, "Beta"=std.all,"SE"=se, "Z"=z,  "CI.Lower"=ci.lower, "CI.Upper"=ci.upper) %>% kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")
#print diagram
semPaths(threefactormod.fit, what="paths", whatLabels="par", rotation = 1, label.prop=1.5, edge.label.color = "black", edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", sizeMan = 5, sizeLat = 5,  sizeLat2 = 5, sizeInt = 3, sizeInt2 = 3, curve=1, intercepts = TRUE, edge.label.cex = 1.1, cardinal = FALSE, style ="mx", residuals = TRUE)
lavaanPlot(model = threefactormod.fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE)


#chisq test of 1-factor model vs 3-factor model difference
anova(onefactor.fit, threefactormod.fit)

#chisq test of nested model difference
anova(threefactormod.fit, threefactor.fit)


# 1-factor BPNF model ####
bpn.onefactor.cfa <- '
bpnf=~a*aut_f1 + b*aut_f2 + c*aut_f3 + d*aut_f4 + e*rel_f1 + f*rel_f2 + g*rel_f3 + h*rel_f4 + i*com_f1 + j*com_f2 + k*com_f3 + l*com_f4
'
bpn.onefactor.fit <- cfa(bpn.onefactor.cfa, data = ardraw, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(bpn.onefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(bpn.onefactor.fit, sort.=TRUE, minimum.value=3)
# check loading factors on their own
inspect(bpn.onefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
bpn.onefactor.residuals <- resid(bpn.onefactor.fit, type = "standardized")
# covariance of residuals
vcov(bpn.onefactor.fit)


# 3-factor BPNF model ####
bpnf.threefactor.cfa <- '
autf=~ a*aut_f1 + b*aut_f2 + c*aut_f3 + d*aut_f4
relf=~ e*rel_f1 + f*rel_f2 + g*rel_f3 + h*rel_f4
comf=~ i*com_f1 + j*com_f2 + k*com_f3 + l*com_f4
autf~~n*relf
autf~~o*comf
comf~~p*relf
'
bpnf.threefactor.fit <- cfa(bpnf.threefactor.cfa, data = ardraw, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR", std.lv=TRUE)
summary(bpnf.threefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)

# CR and AVE 
reliability(bpnf.threefactor.fit)

# check mindices for highly correlated items
modificationindices(bpnf.threefactor.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(bpnf.threefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
bpnf.threefactor.residuals <- resid(bpn.threefactor.fit, type = "standardized")
# covariance of residuals
vcov(bpnf.threefactor.fit)
lavaanPlot(model = bpnf.threefactor.fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE)





# TTBPN scale validity measures####
#alt scale w/ final items and complete cases (n = 168)
altaut <- dplyr::select(altbpnfactors, aut2, aut4, aut5, aut6)
altrel <- dplyr::select(altbpnfactors, rel3, rel4, rel5, rel6)
altcom <- dplyr::select(altbpnfactors, com1, com2, com3, com5)
bpnf <-  dplyr::select(ardrawfactors, aut_f1, aut_f2, aut_f3, aut_f4, rel_f1, rel_f2, rel_f3, rel_f4, com_f1, com_f2, com_f3, com_f4)

# TTBPN indicator and composite reliability #
#ttbpn scale with modified variables
ttbpnmod <- dplyr::select(ardrawfactors, aut2 , aut4, aut5, aut6, rel3, rel4, rel5, rel6, com1 , com2, com3, com5)
# Define the model
ttbpnmod.mod <- '
aut=~a*aut2 + b*aut4+ c*aut5+ d*aut6
rel=~e*rel3+ f*rel4+ g*rel5+ h*rel6 
com=~i*com1 + j*com2+ k*com3+ l*com5
aut~~rel
aut~~com
rel~~com
'
# Fit the model
ttbpnmod.fit <- cfa(ttbpnmod.mod, data = ttbpnmod)
sl <- standardizedSolution(ttbpnmod.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(ttbpnmod.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#aut CR calculation
aut.model <- ' aut  =~ aut2 + aut4 + aut5 + aut6'
aut.fit <- cfa(aut.model, data = ttbpn, meanstructure = TRUE)
sl <- standardizedSolution(aut.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altaut)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for aut
ave.aut <- (sum(sl^2))/6
ave.aut
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(aut.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#rel CR calculation 
rel.model <- ' rel  =~ rel3 + rel4 + rel5 + rel6'
rel.fit <- cfa(rel.model, data = altbpnfactors)
sl <- standardizedSolution(rel.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altrel)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
ave.rel <- (sum(sl^2))/6
ave.rel
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(rel.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#com CR calculation 
com.model <- ' com  =~ com1 + com2 + com3 + com5'
com.fit <- cfa(com.model, data = altbpnfactors)
sl <- standardizedSolution(com.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altcom)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for com
ave.com <- (sum(sl^2))/6
ave.com
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(com.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# compute final 3-factor model validity
cr.threef.fit <- cfa(ttbpnmod.mod, data = ttbpnmod, meanstructure = TRUE)
sl <- standardizedSolution(cr.threef.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod)
# Compute residual variance of each item
re <- 1 - sl^2
# Compute complete final scale composite reliability
cr.threef <- sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(cr.threef.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)
library(semTools)
reliability(ttbpnmod.fit)
discriminantValidity(ttbpnmod.fit, cutoff = 0.9, merge = FALSE, level = 0.95)
htmt(ttbpnmod.mod, data = ttbpnmod, sample.cov = NULL, missing = "listwise",
     ordered = NULL, absolute = TRUE)

# create mean correlation table and test for discriminant validity by squaring correlation
ttbpn.factors <- ardrawfactors[, c("autmean", "relmean", "commean")]
corr.ttbpn <- cor(ttbpn.factors)
sq.corr.ttbpn <- corr.ttbpn^2
sq.corr.ttbpn




