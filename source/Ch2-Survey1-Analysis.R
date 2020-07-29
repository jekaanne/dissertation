library(packrat)
library(data.table)
library(tidyverse)
library(xtable)
library(pander)
library(rmarkdown)
library(kableExtra)
library(Matrix)
library(lavaan)
library(pander)
library(psych)
library(semPlot)
# Demographic Data Tables ####
#create contingency tables for each variable and combine 
colstable <- data.table(variables = c("Female", "Male", "Not specified", "Non-white", "White", "Advanced degree","College", "High school or less", "<5,000", "5,000 - 12,000", "12,000 - 25,000", "25,000 - 50,000", "50,000-100,000", "100,000+", "Full Time", "Part time", "Unemployed", "Missing"))
demotable1<-table(ardraw$gender, ardraw$disability_status)
demotable2<-table(ardraw$white, ardraw$disability_status)
demotable3<-table(ardraw$education, ardraw$disability_status)
demotable4<-table(ardraw$emp_status, ardraw$disability_status)
demotable5<-table(ardraw$income_range, ardraw$disability_status)
demotable<- data.table(rbind(demotable1, demotable2, demotable3, demotable4, demotable5))
#create proportions of rows - change margins = 2 if column proportions are preferred
demo_proportions <- prop.table(as.matrix(demotable), margin=1)*100
#combine variables with individual contingency tables
democomplete <- cbind(colstable, demotable, round(demo_proportions, 2))
#print(xtable(democomplete), type="latex", comment=FALSE)
print(democomplete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")





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
altbpnfactors <- dplyr::select(ardraw, aut1, aut2, aut3, aut4, aut5, aut6, 
                        rel1, rel2, rel3, rel4, rel5, rel6,
                        com1, com2, com3, com4, com5, com6,
                        aut_f1, aut_f2, aut_f3, aut_f4, 
                        rel_f1, rel_f2, rel_f3, rel_f4, 
                        com_f1, com_f2, com_f3, com_f4, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  dis_identity1, dis_identity2, dis_identity3, dis_identity4)
# complete cases only
altbpnfactors <- altbpnfactors[complete.cases(altbpnfactors),]

#alt scale w/ final items and complete cases (n = 168)
altaut <- dplyr::select(altbpnfactors, aut2, aut4, aut5, aut6)
altrel <- dplyr::select(altbpnfactors, rel3, rel4, rel5, rel6)
altcom <- dplyr::select(altbpnfactors, com1, com2, com3, com5)
altautf <- dplyr::select(altbpnfactors, aut_f1, aut_f2, aut_f3, aut_f4)
altrelf <- dplyr::select(altbpnfactors, rel_f1, rel_f2, rel_f3, rel_f4)
altcomf <- dplyr::select(altbpnfactors, com_f1, com_f2, com_f3, com_f4)
pac <- dplyr::select(altbpnfactors, trans_pac1, trans_pac2, trans_pac3, trans_pac4)
gse <- dplyr::select(altbpnfactors, gse1, gse2, gse3, gse4)
disid <- dplyr::select(altbpnfactors, dis_identity1, dis_identity2, dis_identity3, dis_identity4)
altbpnf <-  dplyr::select(ardraw, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  dis_identity1, dis_identity2, dis_identity3, dis_identity4)
flour <- dplyr::select(ardraw, flour1, flour2, flour3, flour4, flour5, flour6, flour7, flour8)


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
ardraw$altbpnfmean <- rowMeans(altbpnf, na.rm = TRUE)


#alt-bpn complete cases only
altbpnfactors <- altbpnfactors[complete.cases(altbpnfactors),]
# mean-score-calcualtins removing missing values for ALT-BPNF
altbpnfactors$autmean <- rowMeans(altaut, na.rm = TRUE)
altbpnfactors$relmean <- rowMeans(altrel, na.rm = TRUE)
altbpnfactors$commean <- rowMeans(altcom, na.rm = TRUE)
altbpnfactors$autfmean <- rowMeans(altautf, na.rm = TRUE)
altbpnfactors$relfmean <- rowMeans(altrelf, na.rm = TRUE)
altbpnfactors$comfmean <- rowMeans(altcomf, na.rm = TRUE)
altbpnfactors$bpnfmean <- rowMeans(altbpnf, na.rm = TRUE)
altbpnfactors$pacmean <- rowMeans(pac, na.rm = TRUE)
altbpnfactors$gsemean <- rowMeans(gse, na.rm = TRUE)
altbpnfactors$disidmean <- rowMeans(disid, na.rm = TRUE)

# scale means for correlation table
subscale_means <- altbpnfactors[, c("autmean", "relmean", "commean", "autfmean", "relfmean", "comfmean", "pacmean", "disidmean", "gsemean")]
#overall score correlation matrix
library(apaTables)
subscales.cor <- apa.cor.table(subscale_means, filename = "subscale-correlations", table.number = 1, show.conf.interval = TRUE, landscape = FALSE)
subscales.cor
# create descriptive table of items
library(psych)
psych::describe(ttbpn)
psych::describe(bpnf)
psych::describe(pac)
psych::describe(gse)
psych::describe(disid)
# test scale alphas  #####
library(coefficientalpha)
coefficientalpha::alpha(ttbpn)
alpha(aut, use="pairwise.complete.obs")
alpha(rel, use="pairwise.complete.obs")
alpha(com, use="pairwise.complete.obs")
alpha(bpn_f, use="pairwise.complete.obs")
alpha(pac, use="pairwise.complete.obs")
alpha(gse, use = "pairwise.complete.obs")
alpha(disid, use = "pairwise.complete.obs")
alpha(flour, use = "pairwise.complete.obs")

# assumptions/normality ####
# univariate normality
# boxplots
boxplot(ardraw$ttbpnmean ~ ardraw$disability_status, ylab="Score", col=c("red", "blue"), main="Boxplot of scores in 3 groups")

# checking for multivariate normal #
library(MVN)
mvn(ttbpn, mvnTest = c("mardia", "hz", "royston", "dh", "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE, desc = TRUE, transform = "none", univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"), univariatePlot = "none", multivariatePlot = "none",  multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded", showOutliers = TRUE, showNewData = TRUE)
library(BaylorEdPsych)
library (mvnmle)
LittleMCAR(ardraw2factors)
library(car)
scatterplotMatrix(subscale_means[7:9])
           #univariate plots
result <- mvn(data = ttbpn, mvnTest = "royston", univariatePlot = "histogram")


# test w/ normalized data 
#calculate z scores
z.ttbpn <-scale(ttbpn)
library(MVN)
mvn(ttbpn)
#calculate z score for count
#z.data.p <- scale (X.p)
#one-sample z.test to see if mean is diff than specified value of pop variability
library(TeachingDemos)
z.test(na.omit(data$col), mu = 0.5, stdev = squrt(0.08))

#one-sample t-test to estimate population variance
t.test(data$col, mu = 0.5, alternative = "two.sided", conf.level = 0.95)

# cutoffs for skewness and kurtosis
library("MASS")
library("ICS")
maha1.ttbpn <- sqrt(mahalanobis(ttbpn, colMeans(ttbpn), cov(ttbpn)))
set.seed(1)
covmve.ttbpn <- cov.rob(ttbpn)
maha2.ttbpn <- sqrt(mahalanobis(ttbpn, covmve.ttbpn$center, covmve.ttbpn$cov))
max.maha.ttbpn <- max(c(maha1.ttbpn, maha2.ttbpn))
out.id <- ifelse(maha2.ttbpn <= sqrt(qchisq(0.975, 6)), 0, 1)
par(mfrow = c(1, 2), las = 1)
plot(maha1.ttbpn, xlab = "index" ,ylab = "Mahalanobis distance", ylim = c(0, max.maha.ttbpn), col = out.id + 1, pch = 15 * out.id + 1)
abline(h = sqrt(qchisq(0.975, 6)))
plot(maha2.ttbpn, xlab = "index", ylab = "robust Mahalanobis distance",
     ylim = c(0, max.maha.ttbpn), col = out.id + 1, pch = 15 * out.id + 1)
abline(h = sqrt(qchisq(0.975, 6)))
par(mfrow = c(1, 1))

# test disability group differences
#independent samples t test 
#assumes variances are not equal between groups by default
t.test(var ~ group, data = data, na.rm = TRUE, var.equal = TRUE)
# for a dependent samples t-test (within-subjects) - add "paired = TRUE"
# corrections: rm outliers of error variance

# 1-factor standardized model w/ MLR ####
onefactor.cfa <- '
ttbpn=~ a*aut1 + b*aut2 + c*aut3 + d*aut4 + e*aut5 + f*aut6 +g*rel1 + h*rel2 + i*rel3 + j*rel4 + k*rel5 + l*rel6 + m*com1 + n*com2 + o*com3 + p*com4 + q*com5 + r*com6
'
onefactor.fit <- cfa(onefactor.cfa, data = ttbpn, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(onefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(onefactor.fit, sort.=TRUE, minimum.value=3)
# check loading factors on their own
inspect(onefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
onefactor.residuals <- resid(onefactor.fit, type = "standardized")
# covariance of residuals
vcov(onefactor.fit)

# 3-factor TTBPN model baseline with MLR ####
threefactor.cfa <- '
aut=~aut1 + aut2+ aut3+ aut4+ aut5+ aut6
rel=~rel1+  rel2+ rel3+ rel4+ rel5+ rel6 
com=~com1+ com2+ com3+ com4 + com5 + com6
aut~~aut
rel~~rel
com~~com
aut~~rel
aut~~com
rel~~com
'
threefactor.fit <- cfa(threefactor.cfa, data = ttbpn, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(threefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor.residuals <- resid(threefactor.fit, type = "standardized")
# covariance of residuals
vcov(threefactor.fit)

# 3- factor modified TTBPN model ####
threefactormod.cfa <- '
aut=~a*aut3 + b*aut4+ c*aut5+ d*aut6
rel=~e*rel1+ f*rel2+ g*rel3+ h*rel4 
com=~i*com1 + j*com2+ k*com3+ l*com6
aut~~aut
rel~~rel
com~~com
aut~~rel
aut~~com
rel~~com
'
threefactormod.fit <- cfa(threefactormod.cfa, data = ttbpn, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR", std.lv=TRUE)
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
library(kableExtra)
parameterEstimates(threefactormod.fit, ci = TRUE, remove.nonfree = TRUE,  standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Indicator'=rhs, 
         'Beta'=std.all,
         'SE'=se, 'Z'=z, 
         'CI.Lower'=ci.lower,
         'CI.Upper'=ci.upper) %>% 
  kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")
#print diagram
semPaths(threefactormod.fit, what="paths", whatLabels="par", rotation = 2, label.prop=0.9, edge.label.color = "black", edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", sizeMan = 3, sizeLat = 3,  curve=2)

library(lavaanPlot)
lavaanPlot(model = , labels = labels, coefs = TRUE, covs = TRUE, stars = TRUE)
lavaanPlot(model = threefactormod.fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE)


# 1-factor BPNF model ####
bpn.onefactor.cfa <- '
bpnf=~a*aut_f1 + b*aut_f2 + c*aut_f3 + d*aut_f4 + e*rel_f1 + f*rel_f2 + g*rel_f3 + h*rel_f4 + i*com_f1 + j*com_f2 + k*com_f3 + l*com_f4
'
bpn.onefactor.fit <- cfa(bpn.onefactor.cfa, data = bpnf, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(bpn.onefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(bpn.onefactor.fit, sort.=TRUE, minimum.value=3)
# check loading factors on their own
inspect(bpn.onefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
bpn.onefactor.residuals <- resid(bpn.onefactor.fit, type = "standardized")
# covariance of residuals
vcov(bpn.onefactor.fit)


# 3-factor BPN-F model ####
bpnf.threefactor.cfa <- '
autf=~ a*aut_f1 + b*aut_f2 + c*aut_f3 + d*aut_f4
relf=~ e*rel_f1 + f*rel_f2 + g*rel_f3 + h*rel_f4
comf=~ i*com_f1 + j*com_f2 + k*com_f3 + l*com_f4
autf~~m*autf
relf~~n*relf
comf~~o*comf
comf~~p*relf
autf~~q*relf
autf~~r*comf
'
bpnf.threefactor.fit <- cfa(bpnf.threefactor.cfa, data = bpnf, sample.nobs = 286, meanstructure = TRUE, estimator = "MLR")
summary(bpnf.threefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(bpnf.threefactor.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(bpnf.threefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
bpnf.threefactor.residuals <- resid(bpn.threefactor.fit, type = "standardized")
# covariance of residuals
vcov(bpnf.threefactor.fit)

# 3-factor ALT-bpnf model with other scales ####
alt.bpnf.threefactor.cfa <- '
pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4
gse=~gse1 +gse2 + gse3 +  gse4
disid=~dis_identity1 + dis_identity2+ dis_identity3 + dis_identity4
pac~~gse
pac~~disid
gse~~disid
'
alt.bpnf.threefactor.fit <- cfa(alt.bpnf.threefactor.cfa, data = altbpnfactors, sample.nobs = 168, estimator = "MLR", std.lv=TRUE)
summary(alt.bpnf.threefactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(alt.bpnf.threefactor.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(alt.bpnf.threefactor.fit, what = "std")$lambda
# check residuals (want value < .1)
alt.bpnf.threefactor.residuals <- resid(alt.bpnf.threefactor.fit, type = "standardized")
# covariance of residuals
vcov(alt.bpnf.threefactor.fit)
# print Table of Factor loadings and parameter estimates
parameterEstimates(alt.bpnf.threefactor.fit, ci = TRUE,  standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Indicator'=rhs, 
         'Beta'=std.lv, 
         'SE'=se, 'Z'=z,
         'P(>|z|)'=pvalue,
         'CI.Lower'=ci.lower,
         'CI.Upper'=ci.upper) %>% 
  kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")
#print diagram
semPaths(alt.bpnf.threefactor.fit, what="paths", whatLabels="par", rotation = 2, label.prop=0.9, edge.label.color = "black", edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", sizeMan = 3, sizeLat = 3,  curve=2)


# TTBPN scale validity measures####
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
sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(fit, "std")$lambda
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
# Compute composite comiability
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




# AUT-BPNF scale validity measures for final ####
# Define the model
alt.bpnf.model <- '
pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4
gse=~gse1 +gse2 + gse3 +  gse4
disid=~dis_identity1 + dis_identity2+ dis_identity3 + dis_identity4
pac~~gse
pac~~disid
gse~~disid
'
# Fit the model
alt.bpnf.fit <- cfa(alt.bpnf.model, data = bpnfalt)
sl <- standardizedSolution(alt.bpnffit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(bpnfalt)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#pac CR calculation
pac.model <- 'pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4'
pac.fit <- cfa(pac.model, data = bpnfalt, meanstructure = TRUE)
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
loadMatrix <- inspect(aut.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#gse CR calculation 
gse.model <- 'gse=~gse1 +gse2 + gse3 +  gse4'
gse.fit <- cfa(gse.model, data = bpnfalt)
sl <- standardizedSolution(gse.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(gse)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
alt.ave.rel <- (sum(sl^2))/6
alt.ave.rel
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(gse.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#disid CR calculation 
disid.model <- '
disid=~dis_identity1 + dis_identity2+ dis_identity3 + dis_identity4'
disid.fit <- cfa(disid.model, data = bpnfalt)
sl <- standardizedSolution(disid.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(disid)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for disid
ave.disid <- (sum(sl^2))/6
ave.disid
# Compute composite comiability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(com.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA

# create mean correlation table and test for discriminant validity by squaring correlation
altbpn.factors<- altbpnfactors[, c("pacmean", "gsemean", "disidmean")]
corr.altbpn <- cor(altbpn.factors)
sq.corr.altbpn <- corr.altbpn^2
sq.corr.altbpn

